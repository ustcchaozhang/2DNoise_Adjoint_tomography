c this is <syg.f>
c------------------------------------------------------------------------------
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c calculate a greens matrix for a layer stack model
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
c    30/07/97   V1.0   Thomas Forbriger
c    03/11/97   V1.1   set parameters from existing file
c    14/04/00   V1.2   calculate green coefficients for radial component too
c    14/03/02   V1.3   do not calculate static case with flgevas
c    15/03/02   V1.4   iragc was declared twice
c    26/03/02   V1.5   now optionally support complex angular frequencies
c    04/03/10   V1.6   comment regarding the source amplitude was wrong!
c    12/01/2011 V1.6b  - added definition of Fourier transformation 
c                        online help text
c
c==============================================================================
c
      program syg
c
c variables
      character*79 version
      parameter (version=
     &  'SYG   V1.6b  calculate synthetic greens matrix')
c
      integer mu, mom
      parameter (mu=10000, mom=4100)
      integer nu, nom
      complex green(mom, mu)
      double complex thisgreen, dummygreen
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
      double precision sigma
      logical use_complex_freq
c 
      integer i, j, index
      real pi
      parameter(pi=3.14159265)
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical radialcomponent, nostatic, verbose
c here are the keys to our commandline options
      data optid/'-r','-x','-v','-s'/
      data opthasarg/3*.FALSE.,.TRUE./
      data optarg/3*'-','0.'/
c 
c go
      print *,version
      print *,'Usage: syg modelfile sourcefile greenfile '
      print *,'           -P nf,nu,fmax,umax | -F file [-r] [-x] [-v]'
      print *,'           [-s sigma]'
      if (iargc().ge.5) then
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
        print *,'  -r           calculate radial components coefficients'
        print *,'  -x           do NOT calculate static case'
        print *,'               (i.e. 0Hz and 0s/km)'
        print *,'  -v           be verbose'
        print *,'  -s sigma     set imaginary part of angular frequency'
        print *,'               to -sigma.'
        print *,' '
        print *,'Definition of the Fourier transformation:'
        print *,'The Fourier transformation used in this program and in'
        print *,'related programs (like gremlin, greda, and gresy) is'
        print *,'defined as'
        print *,' '
        print *,'  U(omega) = int_-infnity^+infnity u(t) exp(-i*omega*t) dt'
        print *,' '
        print *,'Theoretical descriptions of wave propagation often use'
        print *,'exp(i*omega*t) as transform kernel instead of exp(-i*omega*t)'
        print *,'in order to make positive wavenumbers equivalent to wave'
        print *,'propagation in positive coordinate direction. The Fourier'
        print *,'coefficients calculated by this program consequently are'
        print *,'the complex conjugates of those used in theory.'
        print *,' '
c 4.3.2010 notice: the following comment was wrong!
c gr_refsub reads the source amplitude and uses it for scaling
c        print *,'Notice: the source amplitude is ignored.'
        print *,'Array dimensions:'
        print *,'  maximum number of slowness values: ',mu
        print *,'  maximum number of frequencies:     ',mom
        stop
      endif
c 
c get command line parameters
      call getarg(1, modelname)
      call getarg(2, sourcename)
      call getarg(3, greenname)
c 
      call tf_cmdline(6, lastarg, maxopt, optid, optarg, optset, opthasarg)
      radialcomponent=optset(1)
      nostatic=optset(2)
      verbose=optset(3)
      use_complex_freq=optset(4)
      read(optarg(4), *) sigma

      if (use_complex_freq) then
        print *,'use complex frequencies: imaginary part is ',-sigma
      endif

      if (radialcomponent) then
        print *,'calculate radial component''s coefficients'
      else
        print *,'calculate vertical component''s coefficients'
      endif

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
c report on range
      print *,'calculating ',nu,' slowness steps from ',u(1),' s/km to ',
     &        u(nu),' s/km'
      print *,'calculating ',nom,' frequency steps from ',om(1)/2./pi,
     &       ' Hz to ', om(nom)/2./pi,' Hz'
c 
c fill model information from refmet format file
      call gr_refmod(modelname, sourcename)
c
c are we using complex frequencies?
      if (use_complex_freq) call gr_setsigma(sigma)
c 
c go through all slowness values
      do i=1,nu
c
c prepcalculate everything that doesn't change within one slowness
c (that is each layer interface matrix)
        call gr_prep(dble(u(i)))
c        if (verbose) print *,u(i),'s/km'
        do j=1,nom
c go through all frequencies and get the expansion coefficients
c which are the green matrix elements
          if (nostatic.and.((j.eq.1).or.(i.eq.1))) then
            thisgreen=(0.d0,0.d0)
          else
c            if (verbose) print *,om(j)/(2.*pi),'Hz'
            if (radialcomponent) then
              call gr_greenzr(dble(om(j)), dummygreen, thisgreen)
            else
              call gr_green(dble(om(j)), thisgreen)
            endif
          endif
c scale from s/km to s/m units (the refmet code uses slowness values in s/km
c for integration, thus the green coefficients are too small by a factor of
c 1.e6) as we will write the file in s/m units.
c after that the green coefficients are in unit m**3/s an represent a
c displacement waveform in m
          green(j,i)=cmplx(thisgreen*1.e6)
c          if (i.eq.7) print *,'f#',j,om(j),om(j)/2./pi
        enddo
c        print *,'step into u#',i,u(i)
      enddo
c 
c change scale (from s/km to s/m)
c slowness in file must be given in s/m
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
