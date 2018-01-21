c this is <fidase_enfit.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c ----
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
cS
c
c all we need to fit seismogram amplitudes to a smooth spatial decay
c
c REVISIONS and CHANGES
c    15/07/98   V1.0   Thomas Forbriger
c    27/02/99   V1.1   introduced offset dependent scaling
c    04/12/09   V1.2   correct DIN notation for units
c
c==============================================================================
c 
      subroutine enfit(minoff, mindelta, enfexpo)
c
c perform linear regression
c
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_para.inc'
c 
      real minoff, mindelta, enfexpo
cE
c 
      integer pgdeviceid, pgp_open
c 
      if (verbose) then
        print *,' '
        print *,'fit amplitudes to smooth decay'
      endif
c 
      if (nfiles.lt.2) stop 'ERROR (enfit): needs at least 2 input files'
c 
      call enf_index(minoff, mindelta)
      call enf_energies(enfexpo)
      call enf_diffmat
c 
      pgdeviceid=pgp_open(pgdevice)
      if (pgdeviceid.le.0) stop 'ERROR (enfit): opening pg-device'
      call pgsubp(1, 2)
      call enf_epg('before fit')
      call pgupdt
      call enf_solve
      call enf_epg('after fit')
      call pgclos
c 
      call enf_scaldat
c
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine enf_index(minoff, mindelta)
c
c build position index for minimal allowed receiver offset mindelta
c and offsets larger than minoff
c
      real minoff, mindelta
c
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_enfit.inc'
      include 'fidase_para.inc'
c 
cE
      integer i, j, k
      real holdoff
c 
c distribute receivers
      if (verbose) print *,'  distribute receivers...'
      k=0
      i=firstinchain
      do j=1,ntraces
        if (roffset(i).lt.minoff) then
          enf_position(i)=-1
        elseif (k.eq.0) then
          k=1
          enf_offset(k)=roffset(i)
          holdoff=roffset(i)
          enf_position(i)=k
        else
          if ((roffset(i)-holdoff).gt.mindelta) then
            k=k+1
            enf_offset(k)=roffset(i)
            holdoff=roffset(i)
            enf_position(i)=k
          else
            enf_position(i)=k
          endif
        endif
        i=chain(i)
      enddo
      enf_noff=k
c 
      if (enf_noff.lt.3)
     &  stop 'ERROR (enfit): needs at least 3 different offsets'
c 
c calculate mean offsets for positions and
c count receivers at each position
      if (verbose) print *,'  calculate mean positions...'
      do i=1,enf_noff
        if (verbose) print *,'    ',i,'. position'
        holdoff=0.
        enf_Mn(i)=0
        do j=1,ntraces
          if (enf_position(j).eq.i) then
            enf_Mn(i)=enf_Mn(i)+1
            holdoff=holdoff+roffset(j)
            if (verbose) print *,'      receiver ',j,' at ',roffset(j),'m'
          endif
        enddo
        enf_offset(i)=holdoff/float(enf_Mn(i))
        if (verbose) print *,'      mean offset: ',enf_offset(i),'m'
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
cS
c 
      subroutine enf_diffmat
c 
c matrix to calculate derivatives
c 
      include 'fidase_dim.inc'
      include 'fidase_enfit.inc'
      include 'fidase_para.inc'
c 
cE
      integer i
      double precision a, b, c
c
      if (verbose) print *,'  calculate derivative matrix...'
      do i=1,enf_noff-2
        a=2./(enf_offset(i+2)-enf_offset(i))
        b=1./(enf_offset(i+2)-enf_offset(i+1))
        c=1./(enf_offset(i+1)-enf_offset(i))
        enf_Mdif(i,1)=a*c
        enf_Mdif(i,2)=-a*(b+c)
        enf_Mdif(i,3)=a*b
      enddo
c 
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine enf_energies(enfexpo)
c
c calculate energy rms for time series
c
      real enfexpo
c 
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_enfit.inc'
      include 'fidase_para.inc'
c 
cE
      double precision energy, rms, maxamp
      integer i, j
c 
      if (verbose) then
        print *,'  calculate energy rms within traces...'
        print *,'    scale with offset**',enfexpo
      endif
c 
c clear
      do i=1,ntraces
        enf_Vref(i)=0.d0
        do j=1,nfiles-1
          enf_Men(i,j)=0.d0
        enddo
      enddo
c 
      do j=1,nfiles-1
        enf_Vscal(j)=1.d0
      enddo
c
      maxamp=0.d0
      do i=1,ntraces
        if (enf_position(i).gt.0) then
          energy=0.d0
          do j=1,nsamples(i)
            energy=energy+(data(firstsample(i)+j-1)**2)
          enddo
          rms=sqrt(energy/float(nsamples(i)))
          if (fileindex(i).lt.nfiles) then
            enf_Men(enf_position(i),fileindex(i))=rms
          else
            enf_Vref(enf_position(i))=rms
            maxamp=max(maxamp, rms)
          endif
        endif
      enddo
c 
c normalize
      do i=1,ntraces
        if (enf_position(i).gt.0) then
          if (fileindex(i).lt.nfiles) then
            enf_Men(enf_position(i),fileindex(i))=
     &        enf_offset(enf_position(i))**enfexpo*
     &        enf_Men(enf_position(i),fileindex(i))/maxamp
          else
            enf_Vref(enf_position(i))=
     &        enf_offset(enf_position(i))**enfexpo*
     &        enf_Vref(enf_position(i))/maxamp
          endif
        endif
      enddo
c
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine enf_solve
c
c build up system of linear equations and solve it
c
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_enfit.inc'
      include 'fidase_para.inc'
c 
cE
      double precision mat_mn(maxtraces-2, maxtraces)
      double precision mat_nmmn(maxtraces, maxtraces)
      double precision mat_dnmmn(maxfiles-1, maxtraces)
      double precision mat_dnmmnd(maxfiles-1, maxfiles-1)
      double precision vec_rhs(maxfiles-1)
      integer i,j,k, info
c 
      if (verbose) print *,'  build system of linear equations...'
c
      do i=1,enf_noff
        do j=1,enf_noff
          mat_nmmn(i,j)=0.d0
        enddo
      enddo
c 
      do i=1,enf_noff-2
        do j=1,enf_noff
          mat_mn(i,j)=0.d0
        enddo
      enddo
c 
      do i=1,nfiles-1
        do j=1,enf_noff
          mat_dnmmn(i,j)=0.d0
        enddo
      enddo
c 
      do i=1,nfiles-1
        vec_rhs(i)=0.d0
        do j=1,nfiles-1
          mat_dnmmnd(i,j)=0.d0
        enddo
      enddo
c 
      do i=1,enf_noff-2
        do j=1,3
          mat_mn(i,j-1+i)=enf_Mdif(i,j)/float(enf_Mn(j-1+i))
c          print *,i,j-1+i,mat_mn(i,j-1+i), enf_Mn(j-1+i),
c     &      ' Mdif(',i,',',j,'):',
c     &      enf_Mdif(i,j)
        enddo
      enddo
c 
      do i=1,enf_noff
        do j=1,enf_noff
          do k=1,enf_noff-2
            mat_nmmn(i,j)=mat_nmmn(i,j)+mat_mn(k,i)*mat_mn(k,j)
          enddo
c          print *,i,j,mat_nmmn(i,j)
        enddo
      enddo
c 
      do i=1,nfiles-1
        do j=1,enf_noff
          do k=1,enf_noff
            mat_dnmmn(i,j)=mat_dnmmn(i,j)+enf_Men(k,i)*mat_nmmn(k,j)
          enddo
        enddo
      enddo
c 
      do i=1,nfiles-1
        do j=1,nfiles-1
          do k=1,enf_noff
            mat_dnmmnd(i,j)=mat_dnmmnd(i,j)+mat_dnmmn(i,k)*enf_Men(k,j)
          enddo
c          print *,i,j,mat_dnmmnd(i,j)
        enddo
      enddo
c 
      do i=1,nfiles-1
        do j=1,enf_noff
          vec_rhs(i)=vec_rhs(i)-mat_dnmmn(i,j)*enf_Vref(j)
        enddo
c        print *,vec_rhs(i)
      enddo
c 
      if (verbose) print *,'  solve system of linear equations...'
c 
      call dposv('U', nfiles-1, 1, mat_dnmmnd, maxfiles-1,
     &  vec_rhs, maxfiles-1, info)
c 
      if (info.lt.0) then
        print *,'ERROR (dposv): argument ',-info,' has illegal value'
        stop 'ERROR (enfit): could not solve system of linear equations'
      elseif (info.gt.0) then
        print *,'ERROR (dposv): leading minor order ',info,' is not'
        print *,'               positive definite'
        stop 'ERROR (enfit): could not solve system of linear equations'
      endif
c 
      do i=1,nfiles-1
c        print *,enf_Vscal(i), vec_rhs(i)
        enf_Vscal(i)=vec_rhs(i)
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
cS
c 
      subroutine enf_epg(title)
c
c plot energies
c 
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_enfit.inc'
c 
      character title*(*)
c
cE
      integer i, j
      real minen, maxen, minoff, maxoff
      real ts_min, ts_max
c 
      real derivative(maxtraces-2)
      real amplitude(maxtraces)
c 
      minen=0.
      maxen=0.
      maxoff=ts_max(enf_offset, enf_noff)
      minoff=ts_min(enf_offset, enf_noff)
      do i=1,ntraces
        if (enf_position(i).gt.0) then
          if (fileindex(i).lt.nfiles) then
            minen=min(minen, enf_Men(enf_position(i), fileindex(i))*
     &        enf_Vscal(fileindex(i)))
            maxen=max(maxen, enf_Men(enf_position(i), fileindex(i))*
     &        enf_Vscal(fileindex(i)))
          else
            minen=min(minen, enf_Vref(enf_position(i)))
            maxen=max(maxen, enf_Vref(enf_position(i)))
          endif
        endif
      enddo
c 
      call pgpage
      call pgsave
      call pgsch(1.5)
      call pgvstd
      call pgswin(minoff, maxoff, minen, maxen)
      call pgbox('ABCNST', 0., 0, 'ABNST',0., 0)
      call pglab('offset / m', 'scaled relative energy rms',title)
c 
      do i=1,ntraces
        if (enf_position(i).gt.0) then
          if (fileindex(i).lt.nfiles) then
            call pgpt1(roffset(i), 
     &        sngl(enf_Men(enf_position(i), fileindex(i))*
     &        enf_Vscal(fileindex(i))), fileindex(i)+2)
          else
            call pgpt1(roffset(i), sngl(enf_Vref(enf_position(i))),
     &        fileindex(i)+2)
          endif
        endif
      enddo
c 
c calculate derivatives
      do i=1,enf_noff
        amplitude(i)=enf_Vref(i)
        do j=1,nfiles-1
          amplitude(i)=amplitude(i)+enf_Vscal(j)*enf_Men(i,j)
        enddo
        amplitude(i)=amplitude(i)/float(enf_Mn(i))
      enddo
c 
      call pgline(enf_noff, enf_offset, amplitude)
c 
      do i=1,enf_noff-2
        derivative(i)=0.
        do j=1,3
          derivative(i)=derivative(i)+
     &      amplitude(i-1+j)*enf_Mdif(i,j)
        enddo
      enddo
c 
      minen=ts_min(derivative, enf_noff-2)
      maxen=ts_max(derivative, enf_noff-2)
      call pgswin(minoff, maxoff, minen, maxen)
      call pgsci(2)
      call pgbox('A', 0., 0, 'CMST', 0., 0)
      call pgmtxt('R', 2.5, 0.5, 0.5, 'second derivative of energy rms')
      call pgpt(enf_noff-2, enf_offset(2), derivative, -3)
      call pgunsa
c 
      return
      end
c 
c----------------------------------------------------------------------
cS
c
      subroutine enf_scaldat
c
c scale data
c
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_enfit.inc'
      include 'fidase_para.inc'
c 
cE
      integer i, j
c 
      if (verbose) then
        print *,'  amplitude scales to be applied:'
        do i=1,nfiles-1
          print 50,filename(i)(1:index(filename(i), ' ')),enf_Vscal(i)
        enddo
      endif
c 
      do i=1,ntraces
        if (fileindex(i).lt.nfiles) then
          do j=1,nsamples(i)
            data(firstsample(i)-1+j)=data(firstsample(i)-1+j)*
     &        enf_Vscal(fileindex(i))
          enddo
        endif
      enddo
c
      return
   50 format(5x,a40,' * ',f10.5)
      end
c
c ----- END OF fidase_enfit.f -----
