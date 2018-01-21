c this is <rhesyg.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c RHEology depending SYnthetic Greens function
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
c REVISIONS and CHANGES
c    20/10/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program rhesyg
c
      character*79 version
      parameter(version='RHESYG   V1.0   RHEology depending SYnthetic Greens function')
c
      integer mu, mom
      parameter (mu=1000, mom=300)
      integer nu, nom
      complex green(mom, mu)
      double complex thisgreen
c 
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c
      integer lu
      parameter (lu=20)
c 
      character*80 modelname, sourcename, greenname, line
      real fmax, umax
      real om(mom), u(mu)
c 
c standard linear solid
      double precision fr,fp
c 
      integer i, j, iargc, index
      real pi
      parameter(pi=3.14159265)
c 
c go
      print *,version
      print *,'Usage: rhesyg modelfile sourcefile greenfile'
      print *,'           -P nf,nu,fmax,umax | -F file'
      print *,'           -S fr,fp'
      print *,' '
      print *,'RHEology depending SYnthetic Greens function'
      print *,' '
      if (iargc().eq.7) then
        call getarg(4, line)
      else
        line='none'
      endif
      if ((line(1:2).ne.'-P').and.(line(1:2).ne.'-F')) then
        print *,' '
        print *,'  modelfile    refmet style flat earth model'
        print *,'  sourcefile   refmet style flat source configuration'
        print *,'  greenfile    greda style greens matrix (output)'
        print *,' '
        print *,'  -P           take parameters from command line'
        print *,
     &   '  -F file      choose parameters matching to greenfile ''file'' '
        print *,' '
        print *,'  nf           number of frequencies'
        print *,'  nu           number of phase slowness values'
        print *,'  fmax         maximum frequency (Hz)'
        print *,'  umax         maximum slowness (s/km)'
        print *,' '
        print *,'selectable rheology models:'
        print *,' '
        print *,'  -S fr,fp'
        print *,' '
        print *,'  selects a standard linear solid'
        print *,'  fr   is the frequency at which the model is given'
        print *,'  fp   is the relaxation frequency for the SLS'
        print *,' '
        stop
      endif
c 
c get command line parameters
      call getarg(1, modelname)
      call getarg(2, sourcename)
      call getarg(3, greenname)

      if (line(1:2).eq.'-P') then
        call getarg(5, line)
        read(line, *) nom, nu, fmax, umax
c 
        if (nom.gt.mom) stop 'ERROR: too many frequencies'
        if (nu.gt.mu) stop 'ERROR: too many slownesses'
c 
c prepare frequency and slowness values to calculate greens function
        fmax=2.*pi*fmax
        do i=1,nom
          om(i)=float(i-1)*fmax/(nom-1)
        enddo
c 
        do i=1,nu
          u(i)=float(i-1)*umax/(nu-1)
        enddo
      elseif (line(1:2).eq.'-F') then
        call getarg(5, line)
        call greenread(line, 
     &       mu, mom, u, om,
     &       nu, nom)
      else
        stop 'ERROR: there is something wrong with your command line'
      endif
c 
c get rheology settings
      call getarg(6,line)
      if (line(1:3).eq.'-S ') then
        call getarg(7, line)
        read (line, *) fr,fp
      else
        stop 'ERROR: unknown rheology selected'
      endif
c 
c report on range
      print *,'calculating ',nu,' slowness steps from ',u(1),' s/km to ',
     &        u(nu),' s/km'
      print *,'calculating ',nom,' frequency steps from ',om(1)/2./pi,
     &       ' Hz to ', om(nom)/2./pi,' Hz'
c 
c fill model information from refmet format file
      call gr_refmod(modelname, sourcename)
c 
c go through all slowness values
      do i=1,nu
c
        do j=1,nom
c prepare complex velocities for this model
          call gr_slsfsm(dble(om(j)/2./pi), fr, fp)
c prepcalculate everything that doesn't change within one slowness
c (that is each layer interface matrix)
          call gr_prep(dble(u(i)))
c go through all frequencies and get the expansion coefficients
c which are the green matrix elements
          call gr_green(dble(om(j)), thisgreen)
c scale from s/km to s/m units
          green(j,i)=cmplx(thisgreen*1.e6)
c          if (i.eq.7) print *,'f#',j,om(j),om(j)/2./pi
        enddo
c        print *,'step into u#',i,u(i)
      enddo
c 
c change scale (from s/km to s/m)
      do i=1,nu
        u(i)=u(i)*1.e-3
      enddo
c 
c write green code (easy to use)
      print *,'opening green file ',greenname(1:index(greenname,' ')),
     &    ' - overwrite mode'
      open(lu, file=greenname, form='unformatted', err=98)
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom, nu
      write(lu, err=97) (om(i), i=1,nom), (u(i), i=1,nu)
      write(lu, err=97) ((green(i,j), i=1,nom), j=1,nu)
      close(lu, err=96)
c 
      stop
   98 stop 'ERROR: opening green file'
   97 stop 'ERROR: writing green file'
   96 stop 'ERROR: closing green file'
      end
c
c======================================================================
c 
c some subroutines
c
c----------------------------------------------------------------------
c 
c this routine reads in the data
c
      subroutine greenread(filename, 
     &     maxslo, maxfreq, slo, om,
     &     nslo, nom)
c
      character filename*(*)
      integer maxslo, maxfreq
      real om(maxfreq), slo(maxslo)
      integer nslo, nom
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i
c 
      print *,' '
      print *,'read green file ',filename(1:index(filename, ' '))
      open(lu, file=filename, form='unformatted', status='old', err=99)

c check byte sex
      read(lu, err=98, end=97) inmagic
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
        close(lu, err=96)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=98, end=97) nom, nslo
      if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
        close(lu, err=96)
        stop 'ERROR: data exceeds array bounds'
      endif
      read(lu, err=98, end=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
      close(lu, err=96)
      print *,'green file read and closed'
      
      do i=1,nslo
        slo(i)=slo(i)*1000.
      enddo

      return
   99 stop 'ERROR: opening green file'
   98 stop 'ERROR: reading green file'
   97 stop 'ERROR: reading green file - unexpected end'
   96 stop 'ERROR: closing green file'
      end
c       
c ----- END OF syg.f -----
c
c ----- END OF rhesyg.f -----
