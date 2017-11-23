c this is <magres.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c MAsk GREen Select
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
c    28/02/99   V1.0   Thomas Forbriger
c    15/03/2011 V1.1   support mask files produced by gretap
c
c==============================================================================
c
      program magres
c
      character*79 version
      parameter(version='MAGRES   V1.1   MAsk GREen Select')
c 
      integer maxslo, maxfreq
      parameter (maxslo=10000, maxfreq=5000)
      complex green1(maxfreq, maxslo)
      real om1(maxfreq), slo1(maxslo)
      complex green2(maxfreq, maxslo)
      real weights(maxfreq, maxslo)
      real om2(maxfreq), slo2(maxslo)
      integer nom1, nom2, nslo1, nslo2
      character*80 greeninfile, maskfile, outfile
      double precision rms1, rms2, rms3
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c file
      integer lu,i,j
      parameter(lu=20)
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/'-d', '-v'/
      data opthasarg/2*.FALSE./
      data optarg/2*'-'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().ne.3)) then
        print *,version
        print *,'Usage: magres greenin mask greenout'
        print *,'   or: magres -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'MAsk GREen Select'
        print *,' '
        print *,'This algorithm uses a mask-file (created with dig or gretap) to'
        print *,'select parts of a given green file.'
        print *,' '
        print *,'greenin      input greens file'
        print *,'mask         input mask file'
        print *,'greenout     output greens file'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
c
c------------------------------------------------------------------------------
c go
      call getarg(1, greeninfile)
      call getarg(2, maskfile)
      call getarg(3, outfile)

      print *,' '
      call greenread(greeninfile, 
     &     maxslo, maxfreq, slo1, om1,
     &     nslo1, nom1, green1)

c copy slowness and frequency values in case we are up to read a weights file
      nslo2=nslo1
      nom2=nom1
      do i=1,nslo1
        slo2(i)=slo1(i)
      enddo
      do i=1,nom1
        om2(i)=om1(i)
      enddo
      print *,' '

      call maskread(maskfile, 
     &     maxslo, maxfreq, slo2, om2,
     &     nslo2, nom2, green2, weights)

      if (nslo1.ne.nslo2) stop 'ERROR: different number of slownesses'
      if (nom1.ne.nom2) stop 'ERROR: different number of frequencies'

      if (abs((om1(1)-om2(1))/om1(1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'
      if (abs((om1(nom1)-om2(nom1))/om1(nom1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'
      if (abs((slo1(1)-slo2(1))/slo1(1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'
      if (abs((slo1(nslo1)-slo2(nslo1))/slo1(nslo1)).gt.0.01) 
     &   stop 'ERROR: frequency values differ more than 1%'

      rms1=0.d0
      rms2=0.d0
      rms3=0.d0
      do i=1,nslo1
        do j=1,nom1
          if (j.ne.1) then
            rms1=dble(green1(j,i)*conjg(green1(j,i))+rms1)
            rms2=dble(green2(j,i)*conjg(green2(j,i))+rms2)
          endif
          green1(j,i)=green1(j,i)*green2(j,i)
          if (j.ne.1) rms3=dble(green1(j,i)*conjg(green1(j,i))+rms3)
        enddo
      enddo
      rms1=rms1/(nom1*nslo1)
      rms2=rms2/(nom1*nslo1)
      rms3=rms3/(nom1*nslo1)
      rms1=sqrt(rms1)
      rms2=sqrt(rms2)
      rms3=sqrt(rms3)
      print *,' '
      print *,'rms ',greeninfile(1:index(greeninfile,' ')),rms1
      print *,'rms ',maskfile(1:index(maskfile,' ')),rms2
      print *,'rms ',outfile(1:index(outfile,' ')),rms3
      print *,' '

c write green code (easy to use)
c 
      print *,' '
      print *,'opening green file ',outfile(1:index(outfile,' ')),
     &    ' - overwrite mode'
      open(lu, file=outfile, form='unformatted', err=98)
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom1, nslo1
      write(lu, err=97) (om1(i), i=1,nom1), (slo1(i), i=1,nslo1)
      write(lu, err=97) ((green1(i,j), i=1,nom1), j=1,nslo1)
      close(lu, err=96)
c 
      stop
   98 stop 'ERROR: opening output green file'
   97 stop 'ERROR: writing output green file'
   96 stop 'ERROR: closing output green file'
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
     &     nslo, nom, green)
c
      character filename*(*)
      integer maxslo, maxfreq
      complex green(maxfreq, maxslo)
      real om(maxfreq), slo(maxslo)
      integer nslo, nom
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
c 
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
      read(lu, err=98, end=97) ((green(i,j), i=1,nom), j=1,nslo)
      close(lu, err=96)
      print *,'green file read and closed'

      return
   99 stop 'ERROR: opening green file'
   98 stop 'ERROR: reading green file'
   97 stop 'ERROR: reading green file - unexpected end'
   96 stop 'ERROR: closing green file'
      end
c----------------------------------------------------------------------
c 
c this routine reads in the weights
c
      subroutine maskread(filename, 
     &     maxslo, maxfreq, slo, om,
     &     nslo, nom, green, weights)
c
      character filename*(*)
      integer maxslo, maxfreq
      complex green(maxfreq, maxslo)
      real weights(maxfreq, maxslo)
      real om(maxfreq), slo(maxslo)
      integer nslo, nom
      integer inmagic, cpu, match
      character*4 wcmagic, cmagic, incmagic
      parameter(cmagic='1234')
      parameter(wcmagic='123S')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
c 
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
        print *,'matching bytesex for coefficients - good...'
        read(lu, err=98, end=97) nom, nslo
        if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
          close(lu, err=96)
          stop 'ERROR: data exceeds array bounds'
        endif
        read(lu, err=98, end=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
        read(lu, err=98, end=97) ((green(i,j), i=1,nom), j=1,nslo)
        close(lu, err=96)
        print *,'green file read and closed'
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        call tf_bytesex(wcmagic, inmagic, cpu, match)
        if (match.eq.1) then
          print *,'matching bytesex for weights - good...'
          read(lu, err=98, end=97) nom, nslo
          if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
            close(lu, err=96)
            stop 'ERROR: data exceeds array bounds'
          endif
          read(lu, err=98, end=97) ((weights(i,j), i=1,nom), j=1,nslo)
          close(lu, err=96)
          do i=1,nom
            do j=1,nslo
              green(i,j)=cmplx(weights(i,j),0.)
            enddo
          enddo
          print *,'weights file read and closed'
        elseif (match.eq.2) then 
          print *,'bytesex not matching - we will have to swap!'
          stop 'ERROR: do not know how to do that...'
        else
          close(lu, err=96)
          print *,'bytesex read is ',incmagic
          stop 'ERROR: bytesex is unkown - oh oh...'
        endif
      endif

      return
   99 stop 'ERROR: opening mask file'
   98 stop 'ERROR: reading mask file'
   97 stop 'ERROR: reading mask file - unexpected end'
   96 stop 'ERROR: closing mask file'
      end

c
c ----- END OF magres.f -----
