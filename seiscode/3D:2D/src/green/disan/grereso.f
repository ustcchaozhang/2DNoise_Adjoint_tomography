c this is <grereso.f>
c------------------------------------------------------------------------------
c
c Copyright 1999,2010 by Thomas Forbriger (IfG Stuttgart)
c
c Resolution analysis for Fourier-Bessel transformation
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
c
c
c REVISIONS and CHANGES
c    06/07/99   V1.0   Thomas Forbriger
c    12/08/99   V1.1   uhh - there was an ugly bug in readoffsets when
c                      stepping the sort-loop
c    24/08/99   V1.2   omega-values weren't set correctly
c    28/08/99   V1.3   * add test for slant stack
c                      * secure sea-level for small Hankel-arguments
c    01/09/99   V1.4   * changed kappa attenuation to exp(-r/kappa) and
c                        default value of kappa
c                      * invgram does ONLY depend on r(i) and rho - so why
c                        call it from an inner loop
c    29/04/00   V1.5   * provide inverse Bessel transform
c                      * corrected HOP gram calculation error
c                      * normalize seismograms per frequency
c    09/07/00   V1.6   * now use the cylindrical solution of the scalar wave
c                        eqution as synthetic wavefield
c                      * do not normalize seismograms
c                      * introduced switch to turn of omega**2 scaling
c    16/07/00   V1.7   * normseis just removes 1/sqrt(r)
c    19/10/00   V1.8   * returned to Hankel functions in subroutine seismo
c                        (used far-field exponential function before)
c    04/04/01   V1.9   * have to reactivate old far-field version to match
c                        distel requirements
c    13/11/10   V1.10  * replace libnumrec by GSL
c
c==============================================================================
c
      program grereso
c
      character*79 version
      parameter(version=
     &'GRERESO  V1.10  Resolution analysis for Fourier-Bessel transformation')
c 
      include 'grereso.inc'
c
      character*80 offsetfile, basename, infile
c 
      logical rhoset,fixedomega,slantstack,convolve,besseltransform
      logical distelfield
      real vmin,vmax,pi,drmin
      parameter(pi=3.1415926535897931159979)
      integer i,islo,iom,ip,nv
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=13)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-O, 2h-K, 2h-R, 2h-T, 2h-h, 2h-H,2h-S,2h-B,
     &           2h-l,2h-X,2h-F/
      data opthasarg/3*.FALSE.,5*.TRUE.,2*.FALSE.,.TRUE.,2*.FALSE./
      data optarg/3*1h-,5h1.e60,1h-,2h0.,4*1h-,3h1.5,2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      print *,version
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,'Usage: grereso basename offsetfile'
        print *,'               [ fmin,fmax,nf pmin,pmax,np smin,smax,ns |'
        print *,'               C filename ]'
        print *,'               [-O] [-K kappa] [-R rho] [-T frac]'
        print *,'               [-H 1|2] [-h 1|2] [-S] [-B] [-l lowexp]'
        print *,'               [-X] [-F]'
        print *,'   or: grereso -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'Resolution analysis for Fourier-Bessel transformation'
        print *,' '
        print *,'basename     base to build output filename from'
        print *,'offsetfile   file to read offsets from (as used in chaco)'
        print *,' '
        print *,'fmin         minimum frequency'
        print *,'fmax         maximum frequency'
        print *,'nf           number of frequencies'
        print *,' '
        print *,'pmin         minimum slowness'
        print *,'pmax         maximum slowness'
        print *,'np           number of slowness values'
        print *,' '
        print *,'smin         minimum test slowness'
        print *,'smax         maximum test slowness'
        print *,'ns           number of test slowness values'
        print *,' '
        print *,'C filename   select convolve mode with file ''filename'' '
        print *,' '
        print *,'-O           write one file per frequency (default is' 
        print *,'             to write one file per test slowness)'
        print *,'-S           test slant stack instead of HOP (Henry,'
        print *,'             Orcutt and Parker) analysis - seimsogram'
        print *,'             amplitudes will be normalized according to'
        print *,'             plane waves'
        print *,'-B           test inverse Bessel transform (approximation'
        print *,'             by trapezoid rule) instead of HOP analysis'
        print *,'-K kappa     offset damping factor exponent'
        print *,'             (seismogram amplitudes are attenuated with'
        print *,'             exp(-r/kappa) where r is the offset given'
        print *,'             in meters)'
        print *,'             default: ',optarg(4)
        print *,'-l lowexp    a quasi lowpass filter will be applied to'
        print *,'             seismograms by multiplying with'
        print *,'             omega**(-lowexp)'
        print *,'             default: ',optarg(11)
        print *,'-R rho       scalar product damping constant (see greda)'
        print *,'             rho should be somewhere around delta_r, where'  
        print *,'             delta_r is the minimal offset step'
        print *,'             default: 1/rmin, where rmin is the minimal'
        print *,'             offset interval'
        print *,'-T frac      tapering fraction for cosine offset taper'
        print *,'             at the far edge'
        print *,'             default: ',optarg(6)
        print *,'-h 1|2       set analysis function (representers) to'
        print *,'             Hankel 1 or 2 (default is Bessel)'
        print *,'-H 1|2       set wavefield function (representers) to'
        print *,'             Hankel 1 or 2 (default is Bessel)'
        print *,'-X           omit omega**2 scaling for Bessel transform and'
        print *,'             HOP analysis'
        print *,'-F           use far-field approximation as test wave-'
        print *,'             field (just as defined within distel ;-))'
        print *,'             default: use Hankel- or Bessel-function'
        print *,' '
        print *,'convolve mode'
        print *,'  A spectra-file is read and convolved with the resolution'
        print *,'  matrix. The result is a spectrum that should be equivalent'
        print *,'  to the result that would be obtained when calculating'
        print *,'  synthetic seimsmograms for the given receiver spread'
        print *,'  and applying a Henry,Orcutt,Parker-Analysis on them.'
        print *,'  Only the options -R, -S, -T and -h will affect this'
        print *,'  analysis.'
        print *,' '
        print *,'compiled dimensions:'
        print *,'  number of receivers:           ',maxr
        print *,'  number of slowness values:     ',maxslo
        print *,'  number of test slowness values:',maxpf
        print *,'  number of frequencies:         ',maxpf
        print *,' '
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call getarg(3,argument)
      convolve=(argument(1:1).eq.'C')
c 
      if (convolve) then
        call tf_cmdline(5, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
        call getarg(4,infile)
        call readinfile(infile)
      else
        call tf_cmdline(6, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
c 
        call getarg(3,argument)
        read(argument, *) vmin,vmax,nv
        if (nv.gt.maxpf) stop 'ERROR: too many frequencies'
        nom=nv
        if (nom.gt.1) then
          do i=1,nom
            om(i)=2.d0*pi*((vmax-vmin)*(i-1)/(nv-1)+vmin)
          enddo
        else
          nom=1
          om(1)=2.d0*pi*vmin
        endif
c 
        call getarg(4,argument)
        read(argument, *) vmin,vmax,nv
        if (nv.gt.maxslo) stop 'ERROR: too many slowness values'
        nslo=nv
        if (nslo.gt.1) then
          do i=1,nslo
            slo(i)=1.e-3*((vmax-vmin)*(i-1)/(nv-1)+vmin)
          enddo
        else
          nslo=1
          slo(1)=1.e-3*vmin
        endif
c 
        call getarg(5,argument)
        read(argument, *) vmin,vmax,nv
        if (nv.gt.maxpf) stop 'ERROR: too many test slowness values'
        np=nv
        if (np.gt.1) then
          do i=1,np
            tslo(i)=1.e-3*((vmax-vmin)*(i-1)/(nv-1)+vmin)
          enddo
        else
          np=1
          tslo(1)=1.e-3*vmin
        endif
      endif
cc  
      debug=optset(1)
      verbose=optset(2)
c     
      call getarg(1,basename)
      call getarg(2,offsetfile)
c 
      fixedomega=optset(3)
      read(optarg(4), *) kappa
      rhoset=optset(5)
      if (rhoset) read(optarg(5), *) rho
      read(optarg(6), *) tfrac
      analyh1=(optarg(7)(1:1).eq.'1')
      analyh2=(optarg(7)(1:1).eq.'2')
      seismoh1=(optarg(8)(1:1).eq.'1')
      seismoh2=(optarg(8)(1:1).eq.'2')
      slantstack=optset(9)
      besseltransform=optset(10)
      read(optarg(11), *) lowexp
      omscale=(.not.optset(12))
      distelfield=optset(13)
c 
      call readoffsets(offsetfile)
c 
      drmin=r(nr)
      do i=1,nr-1
        drmin=min(drmin,(r(i+1)-r(i)))
      enddo
      if (.not.(rhoset)) rho=1./max(1.e-20,drmin)
      print *,'rho is:    ',rho
      print *,'kappa is:  ',kappa
      print *,'lowexp is: ',lowexp
      print *,'tfrac is:  ',tfrac
      print *,'maximum offset:      ',r(nr)
      print *,'minimum offset step: ',drmin
c 
      print *,' '
      print *,'  number of receivers:           ',nr
      print *,'  number of slowness values:     ',nslo
      print *,'  number of test slowness values:',np
      print *,'  number of frequencies:         ',nom
c
c------------------------------------------------------------------------------
c go
      if (convolve) then
        print *,'convolve read spectrum...'
c test slowness array equals slowness array 
        np=nslo
        do ip=1,np
          tslo(ip)=slo(ip)
        enddo
        dp=slo(2)-slo(1)
c Gram matrix will never change 
        if (besseltransform) then
          call btrgram
        else
          call invgram
        endif
c go through all frequencies 
        do iom=1,nom
c calculate resolution matrix just as it will be done below
          print 50,'frequency ',om(iom)*0.5/pi,' ',iom,' of ',nom,'...'
          if (slantstack) then
            do ip=1,np
              remat(1,ip)=(0.,0.)
              do islo=2,nslo
                call seismo(om(iom),tslo(ip),distelfield)
                call normseis
                call slstack(om(iom), slo(islo), remat(islo,ip))
              enddo
            enddo
          elseif (besseltransform) then
            do ip=1,np
              remat(1,ip)=(0.,0.)
              do islo=2,nslo
                call seismo(om(iom),tslo(ip),distelfield)
                call sumit(om(iom),slo(islo),remat(islo,ip))
                call btrscale(om(iom),slo(islo),remat(islo,ip))
              enddo
            enddo
          else
            do ip=1,np
              remat(1,ip)=(0.,0.)
              do islo=2,nslo
                call seismo(om(iom),tslo(ip),distelfield)
                call sumit(om(iom),slo(islo),remat(islo,ip))
                call hopscale(om(iom),slo(islo),remat(islo,ip))
              enddo
            enddo
          endif
c calculate expansion integral by trapezoid rule
          print *,'  ...go for trapezoid rule...'
          do islo=1,nslo
            spec(islo, iom)=tslo(1)*inspec(1,iom)*remat(islo,1)
            spec(islo, iom)=spec(islo,iom)+
     &                      tslo(np)*inspec(np,iom)*remat(islo,np)
            spec(islo, iom)=spec(islo,iom)*0.5
            do ip=2,(np-1)
              spec(islo, iom)=spec(islo,iom)+
     &                        tslo(ip)*inspec(ip,iom)*remat(islo,ip)
            enddo
            spec(islo, iom)=spec(islo,iom)*dp
          enddo
        enddo
        call writespec(basename)
      elseif (fixedomega) then
        if (slantstack) then
          print *,'test slant stack...'
          do iom=1,nom
            print 50,'frequency ',om(iom)*0.5/pi,' ',iom,' of ',nom,'...'

            do ip=1,np
              spec(1,ip)=(0.,0.)
              do islo=2,nslo
                call seismo(om(iom),tslo(ip),distelfield)
                call normseis
                call slstack(om(iom), slo(islo), spec(islo,ip))
              enddo
            enddo
            call writefixedom(basename,iom)
          enddo
        elseif (besseltransform) then
          print *,'test Bessel transform analysis...'
          call btrgram
          do iom=1,nom
            print 50,'frequency ',om(iom)*0.5/pi,' ',iom,' of ',nom,'...'

            do ip=1,np
              spec(1,ip)=(0.,0.)
              do islo=2,nslo
                call seismo(om(iom),tslo(ip),distelfield)
                call sumit(om(iom),slo(islo),spec(islo,ip))
                call btrscale(om(iom),slo(islo),spec(islo,ip))
              enddo
            enddo
            call writefixedom(basename,iom)
          enddo
        else
          print *,'test HOP analysis...'
          call invgram
          do iom=1,nom
            print 50,'frequency ',om(iom)*0.5/pi,' ',iom,' of ',nom,'...'

            do ip=1,np
              spec(1,ip)=(0.,0.)
              do islo=2,nslo
                call seismo(om(iom),tslo(ip),distelfield)
                call sumit(om(iom),slo(islo),spec(islo,ip))
                call hopscale(om(iom),slo(islo),spec(islo,ip))
              enddo
            enddo
            call writefixedom(basename,iom)
          enddo
        endif
      else
        if (slantstack) then
          print *,'test slant stack...'
          do ip=1,np
            print 50,'test slowness ',tslo(ip)*1.e3,' ',ip,' of ',np,'...'

            do iom=1,nom
              call seismo(om(iom),tslo(ip),distelfield)
              call normseis
              spec(1,iom)=(0.,0.)
              do islo=2,nslo
                call slstack(om(iom), slo(islo), spec(islo,iom))
              enddo
c              print *,'spec ',(islo,iom,abs(spec(islo,iom)), islo=1,nslo)
            enddo
            call writefixedp(basename,ip)
          enddo
        elseif (besseltransform) then
          print *,'test Bessel transform analysis...'
          call btrgram
          do ip=1,np
            print 50,'test slowness ',tslo(ip)*1.e3,' ',ip,' of ',np,'...'

            do iom=1,nom
              call seismo(om(iom),tslo(ip),distelfield)
              spec(1,iom)=(0.,0.)
              do islo=2,nslo
                call sumit(om(iom),slo(islo),spec(islo,iom))
                call btrscale(om(iom),slo(islo),spec(islo,iom))
              enddo
            enddo
            call writefixedp(basename,ip)
          enddo
        else
          print *,'test HOP analysis...'
          call invgram
          do ip=1,np
            print 50,'test slowness ',tslo(ip)*1.e3,' ',ip,' of ',np,'...'

            do iom=1,nom
              call seismo(om(iom),tslo(ip),distelfield)
              spec(1,iom)=(0.,0.)
              do islo=2,nslo
                call sumit(om(iom),slo(islo),spec(islo,iom))
                call hopscale(om(iom),slo(islo),spec(islo,iom))
              enddo
            enddo
            call writefixedp(basename,ip)
          enddo
        endif
      endif
c
      stop
   50 format(a,f6.1,a,i3,a,i3,a)
      end
c 
c======================================================================
c 
      subroutine writefixedp(basename,ifile)
c 
      character basename*(*)
      integer ifile
c 
      character*80 greensfile
      integer j
c 
      greensfile=basename
      j=index(greensfile,' ')
      write(greensfile(j:j+4), '(1h.,i3.3)') ifile
      call writespec(greensfile)
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine writespec(outfile)
c 
      character outfile*(*)
c 
      include 'grereso.inc'
c 
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c 
      integer i,j, lu
      parameter(lu=10)
c 
c write coefficients (easy to use)
c 
      print *,'opening coefficient file ',outfile(1:index(outfile,' ')),
     &  ' - overwrite mode'
      open(lu, file=outfile, form='unformatted', err=99)
      call tf_magic(cmagic, magic)
      write(lu, err=98) magic
      write(lu, err=98) nom, nslo
      write(lu, err=98) (om(i), i=1,nom), (slo(i), i=1,nslo)
      write(lu, err=98) ((spec(j,i), i=1,nom), j=1,nslo)
      close(lu, err=97)
c      print *,'all ', ((j,i,abs(spec(j,i)), i=1,nom), j=1,nslo)
c 
      return
   99 stop 'ERROR: opening coefficient file'
   98 stop 'ERROR: writing coefficient file'
   97 stop 'ERROR: closing coefficient file'
      end
c
c----------------------------------------------------------------------
c 
      subroutine writefixedom(basename,ifile)
c 
      character basename*(*)
      integer ifile
c 
      include 'grereso.inc'
c 
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='123P')
c 
      character greensfile*80
      integer i,j, lu
      parameter(lu=10)
c 
c write expansion coefficients (easy to use)
c 
      greensfile=basename
      j=index(greensfile,' ')
      write(greensfile(j:j+4), '(1h.,i3.3)') ifile
      print *,'opening coefficient file ',greensfile(1:index(greensfile,' ')),
     &  ' - overwrite mode'
      open(lu, file=greensfile, form='unformatted', err=99)
      call tf_magic(cmagic, magic)
      write(lu, err=98) magic
      write(lu, err=98) np, nslo
      write(lu, err=98) (tslo(i), i=1,np), (slo(i), i=1,nslo)
      write(lu, err=98) ((spec(j,i), i=1,np), j=1,nslo)
      close(lu, err=97)
c 
      return
   99 stop 'ERROR: opening coefficient file'
   98 stop 'ERROR: writing coefficient file'
   97 stop 'ERROR: closing coefficient file'
      end
c
c----------------------------------------------------------------------
c
      subroutine readinfile(infile)
c 
      character infile*(*)
c 
      include 'grereso.inc'
c magic number for binary file identification
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
c file
      integer lu
      parameter(lu=20)
c 
      integer i,j
c
      print *,'read coefficient file ',infile(1:index(infile, ' '))
      open(lu, file=infile, form='unformatted', status='old', err=89)
c check byte sex
      read(lu, err=88, end=87) inmagic
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        print *,'running on Intel...'
      elseif (cpu.eq.2) then
        print *,'running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        print *,'matching bytesex - good...'
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=86)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=88, end=87) nom, nslo
      if ((nom.gt.maxpf).or.(nslo.gt.maxslo)) then
        close(lu, err=86)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=88, end=87) (om(i), i=1,nom), (slo(i), i=1,nslo)
      read(lu, err=88, end=87) ((inspec(j,i), i=1,nom), j=1,nslo)
      close(lu, err=86)
      print *,'coefficient file read and closed'
      return
c 
   89 stop 'ERROR: opening coefficient file'
   88 stop 'ERROR: reading coefficient file'
   87 stop 'ERROR: reading coefficient file - unexpected end'
   86 stop 'ERROR: closing coefficient file'
      end
c
c----------------------------------------------------------------------
c 
      subroutine readoffsets(filename)
c 
      character filename*(*)
c 
      include 'grereso.inc'
c 
      integer lu,i,j
      parameter(lu=10)
      real c1,c2,c3,h
c 
      print *,'opening offset file ',filename(1:index(filename,' '))
      open(lu, file=filename, status='old', err=99)
      nr=0
    1 continue
      read(lu, *, err=98, end=2) c1,c2,c3
      if (nr.lt.maxr) then
        nr=nr+1
        r(nr)=sqrt(c1*c1+c2*c2+c3*c3)
        goto 1
      endif
    2 continue
      close(lu,err=97)
c
      do i=1,nr
        do j=nr,i+1,-1
          if (r(i).gt.r(j)) then
            h=r(i)
            r(i)=r(j)
            r(j)=h
          endif
        enddo
c        print *,i,r(i)
      enddo
c
      return
   99 stop 'ERROR: opening offset file'
   98 stop 'ERROR: reading offset file'
   97 stop 'ERROR: closing offset file'
      end
c
c----------------------------------------------------------------------
c
      subroutine btrgram
c 
c calculate 'inverse gram matrix' for inverse Bessel transform
c
      include 'grereso.inc'
c
      integer i
c this part uses sparse matrix
c Nebendiagonale
      do i=1,(nr-1)
        igr(i,2)=0.d0
      enddo

c Hauptdiagonale
      do i=2,(nr-1)
        igr(i,1)=0.5*(r(i+1)-r(i-1))*r(i)
      enddo
      igr(1,1)=0.5*(r(2)-r(1))*r(1)
      igr(nr,1)=0.5*(r(nr)-r(nr-1))*r(nr)
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine invgram
c 
c calculate inverse gram matrix for given angular frequency
c
      include 'grereso.inc'
c
      double precision arg, p(maxr), q(maxr)
      double precision tf_gsl_sf_bessel_k0
      double precision tf_gsl_sf_bessel_i0
      integer i
c      integer j
c     
      do i=1,nr
        arg=r(i)*rho
        p(i)=tf_gsl_sf_bessel_i0(arg)
        q(i)=tf_gsl_sf_bessel_k0(arg)
      enddo
c1111
c this part uses sparse matrix
c Nebendiagonale
      do i=1,(nr-1)
        igr(i,2)=1./(p(i)*q(i+1)-q(i)*p(i+1))
      enddo

c Hauptdiagonale
      igr(1,1)=-igr(1,2)*p(2)/p(1)
      igr(nr,1)=-igr(nr-1,2)*q(nr-1)/q(nr)
      do i=2,(nr-1)
        igr(i,1)=igr(i-1,2)*igr(i,2)*(p(i+1)*q(i-1)-q(i+1)*p(i-1))
      enddo
c1111

c2222
c this part uses full matrix array
c      do i=1,nr
c        do j=1,nr
c          igr(i,j)=0.d0
c        enddo
c      enddo

c      do i=1,nr-1
c        igr(i,i+1)=1./(p(i)*q(i+1)-q(i)*p(i+1))
c        igr(i+1,i)=igr(i,i+1)
c      enddo

c      igr(1,1)=-igr(1,2)*p(2)/p(1)
c      igr(nr,nr)=-igr(nr,nr-1)*q(nr-1)/q(nr)

c      do i=2,nr-1
c        igr(i,i)=igr(i,i-1)*igr(i,i+1)*(p(i+1)*q(i-1)-q(i+1)*p(i-1))
c      enddo
c2222
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine sumit(omega,p,coeff)
c
c calculate sum over inverse gram matrix
c (gram matrix and seismogram spectra must be given)
c
      real omega,p
      complex coeff
      double precision tf_dj0
      double precision tf_dy0
      include 'grereso.inc'
c
      double complex hq(maxr), ime
      double precision lambda, arg
      integer i
c      integer j
      parameter(ime=(0.d0,1.d0))
c
      lambda=rho/omega

      do i=1,nr
        arg=p*omega*r(i)
        if (analyh1) then
          hq(i)=tf_dj0(arg)+ime*tf_dy0(arg)
        elseif (analyh2) then
          hq(i)=tf_dj0(arg)-ime*tf_dy0(arg)
        else
          hq(i)=tf_dj0(arg)
        endif
      enddo

c1111
c this part uses a sparse matrix 
c that looks to strange to me (29/04/2000)
c well, it actually matched the calculation in greda.f
c however there was still a bug in greda...
c so anyway...
c      coeff=(0.,0.)
c      do i=1,nr
c        coeff=coeff+hq(i)*u(i)*igr(i,1)
c      enddo
c      do i=1,nr-1
c        coeff=coeff+igr(i,2)*(hq(i)*u(i+1)+u(i)*hq(i+1))
c      enddo
      coeff=hq(1)*(u(1)*igr(1,1)+u(2)*igr(1,2))  
      coeff=coeff+hq(nr)*(u(nr)*igr(nr,1)+u(nr-1)*igr(nr-1,2))
      do i=2,(nr-1)
        coeff=coeff+hq(i)*(u(i)*igr(i,1)+u(i-1)*igr(i-1,2)+u(i+1)*igr(i,2))
      enddo
c1111

c2222
c this part uses a full matrix array
c      coeff=(0.,0.)
c      do i=1,nr
c        do j=1,nr
c          coeff=coeff+igr(i,j)*hq(i)*u(j)
c        enddo
c      enddo
c2222
      return
      end
c----------------------------------------------------------------------
c 
      subroutine btrscale(omega,p,coeff)
c 
c scale Bessel transform coefficient
c
      real omega,p
      complex coeff
c 
      include 'grereso.inc'
c 
      if (omscale) coeff=coeff*(omega*omega)
c      coeff=coeff*omega
c
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine hopscale(omega,p,coeff)
c 
c scale HOP coefficient
c
      real omega,p
      complex coeff
c 
      include 'grereso.inc'
c 
      if (omscale) then
        coeff=coeff/(p*p+(rho*rho/(omega*omega)))
      else
        coeff=coeff/(p*p*omega*omega+rho*rho)
      endif
c
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine slstack(omega,p,coeff)
c
      real omega,p
      complex coeff
      include 'grereso.inc'
c
      double complex ime
      integer i
      parameter(ime=(0.d0,1.d0))
c
      coeff=(0.,0.)

      do i=1,nr
        coeff=coeff+u(i)*exp(ime*omega*p*r(i))
      enddo
c
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine seismo(omega,p0,distelfield)
c
      real omega, p0
      logical distelfield
c 
      include 'grereso.inc'
c
      integer i
      double precision arg, toff,a,pi,maxval,thisr
      double precision tf_dj0
      double precision tf_dy0
      parameter(pi=3.14159265358979311599796346854d0)
      double complex ime
      parameter(ime=(0.d0,1.d0))

      toff=(1.-tfrac)*r(nr)
      a=pi*0.5d0/(r(nr)*tfrac)

      maxval=0.
      do i=1,nr
        thisr=max(r(i),1.d-10)
        arg=max(omega*p0*thisr,1.d-40)
        if (seismoh1) then
c          u(i)=(tf_dj0(arg)+ime*tf_dy0(arg))*((1./r(i))**kappa)
c          u(i)=(tf_dj0(arg)+ime*tf_dy0(arg))*exp(-r(i)/kappa)
          if (distelfield) then
            u(i)=exp(ime*arg)/sqrt(thisr)
          else
            u(i)=(tf_dj0(arg)+ime*tf_dy0(arg))
          endif
        elseif (seismoh2) then
c          u(i)=(tf_dj0(arg)-ime*tf_dy0(arg))*((1./r(i))**kappa)
c          u(i)=(tf_dj0(arg)-ime*tf_dy0(arg))*exp(-r(i)/kappa)
          if (distelfield) then
            u(i)=exp(-ime*arg)/sqrt(thisr)
          else
            u(i)=(tf_dj0(arg)-ime*tf_dy0(arg))
          endif
        else
c          u(i)=tf_dj0(arg)*((1./r(i))**kappa)
c          u(i)=tf_dj0(arg)*exp(-r(i)/kappa)
          if (distelfield) then
            u(i)=cos(arg)/sqrt(thisr)
          else
            u(i)=tf_dj0(arg)
          endif
        endif
        u(i)=u(i)*exp(-r(i)/kappa)
        if (r(i).gt.toff) u(i)=u(i)*cos(a*(r(i)-toff))
c        maxval=max(maxval,abs(u(i)))
c        if (r(i).gt.toff) then
c          print *,'o',omega,'p0',p0,'r',r(i),'fac',((1./r(i))**kappa)*cos(a*(r(i)-toff))
c        else
c          print *,'o',omega,'p0',p0,'r',r(i),'fac',((1./r(i))**kappa)
c        endif
      enddo
      
c      if (omega.gt.1.e-6) then
c        maxval=(omega**lowexp)
c      endif
c      do i=1,nr
c        u(i)=u(i)/maxval
c      enddo

      return
      end
c
c----------------------------------------------------------------------
c
      subroutine normseis
c
      include 'grereso.inc'
c
      integer i
      double precision toff,a,pi
      parameter(pi=3.14159265358979311599796346854d0)

c      toff=(1.-tfrac)*r(nr)
c      a=pi*0.5d0/(r(nr)*tfrac)

      do i=1,nr
c        u(i)=u(i)/abs(u(i))*((1./r(i))**kappa)
c        u(i)=u(i)/abs(u(i))*exp(-r(i)/kappa)
c        if (r(i).gt.toff) u(i)=u(i)*cos(a*(r(i)-toff))
        u(i)=u(i)*sqrt(r(i))
      enddo
c      print *,'nr,u',nr, (u(i), i=1,nr)

      return
      end
c
c ----- END OF grereso.f -----
