c this is <grepg_readfourier.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c read Fourier coefficients for traces
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
c    13/09/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine grepg_readfourier(filename, verbose, om, nom, maxom)
c
      character filename*(*)
      logical verbose
      integer nom, maxom
      real om(maxom)
c 
      include 'grepg_fourier.inc'
c 
      integer inmagic, cpu, match
      character*4 incmagic
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
      real pi2
      parameter(pi2=2.*3.1415926535)
c 
      print *,'read Fourier file ',filename(1:index(filename, ' '))
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
        print *,'bytesex read (',incmagic,') ist not Fourier'
        stop 'ERROR: unknown...'
      endif
c 
      read(lu, err=98, end=97) fnom, fntr
      if ((fnom.gt.fmaxom).or.(fntr.gt.fmaxtr)) then
        close(lu, err=96)
        stop 'ERROR: data exceeds array bounds'
      else
        read(lu, err=98, end=97) (fom(i), i=1,fnom), 
     &    (foffs(i), i=1,fntr)
        read(lu, err=98, end=97) ((fourier(j,i), i=1,fnom), j=1,fntr)
        close(lu, err=96)
        print *,'Fourier file read and closed'
      endif

c plausibility checks
      if (fnom.ne.nom) stop 'ERROR: inconsistent number of frequencies'
      do i=1,fnom
        if (abs((om(i)/fom(i))-1.).gt.0.001)
     &    stop 'ERROR: inconsistent frequencies'
      enddo
      if (verbose) then
        print *,'  read ',fnom,' Fourier coefficients'
        print *,'  for ',fntr,' seismic traces'
      endif
      print *,' '

      return
   99 stop 'ERROR: opening Fourier file'
   98 stop 'ERROR: reading Fourier file'
   97 stop 'ERROR: reading Fourier file - unexpected end'
   96 stop 'ERROR: closing Fourier file'
      end
c
c----------------------------------------------------------------------
c
      subroutine grepg_fourierinfo
      include 'grepg_fourier.inc'
      print *,' '
      print *,'magic number for Fourier data: ',cmagic
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine grepg_nofourier
      include 'grepg_fourier.inc'
      fourieravail=.true.
      return
      end

c ----- END OF grepg_readfourier.f ----- 
