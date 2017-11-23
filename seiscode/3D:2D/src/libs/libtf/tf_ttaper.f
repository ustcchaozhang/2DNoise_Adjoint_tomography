c this is <tf_ttaper.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1998 by Thomas Forbriger (IfG Stuttgart)
c
c routines to work with time tapers as used for surface wave seimics
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
c    08/07/98   V1.0   Thomas Forbriger
c    04/02/10   V1.1   had to fix reading 
c                      gfortran apparently interprets formats in a different
c                      way
c
c==============================================================================
cS
c
      subroutine tf_ttapread(filename, t, x, npicks, maxpicks, text)
c
      character*(*) filename
      character*(*) text
      real t(maxpicks,4)
      real x(maxpicks,4)
      integer npicks(maxpicks)
      integer maxpicks
c
cE
      integer lu, i, j
      parameter(lu=9)
c 
c      print *,'open file:'
c      print *,filename
      open(lu, file=filename, status='old', err=99)
c     
      read(lu, 50, err=98, end=97) text
c      print *,'comment:'
c      print *,text
      do i=1,4
        read(lu, *, err=98, end=97) 
c        print *,'skipped 1'
        read(lu, 51, err=98, end=97) npicks(i)
        read(lu, *, err=98, end=97) 
c        print *,i,npicks(i)
        if (npicks(i).gt.0) read(lu, 52, err=98, end=97)
     &    (x(j,i), t(j,i), j=1,npicks(i))
      enddo
c
      close(lu, err=96)
c
      return
   50 format(/a80)
   51 format(18x,i3)
   52 format(2g15.6)
   99 stop 'ERROR (tf_ttapread): opening file'
   98 stop 'ERROR (tf_ttapread): reading file'
   97 stop 'ERROR (tf_ttapread): reading file - unexpected end'
   96 stop 'ERROR (tf_ttapread): closing file'
      end
c
c----------------------------------------------------------------------
cS
      subroutine tf_ttapwrite(filename, t, x, npicks, maxpicks, text)
c
      character filename*(*)
      character text*(*)
      real t(maxpicks,4)
      real x(maxpicks,4)
      integer npicks(maxpicks)
      integer maxpicks
c
cE
      integer lu, i, j
      parameter(lu=9)
c 
      open(lu, file=filename, status='new', err=99)
c     
      write(lu, 50, err=98) text
      do i=1,4
        write(lu, 51, err=98) i,npicks(i)
        if (npicks(i).gt.0) write(lu, 52, err=98)
     &    (x(j,i), t(j,i), j=1,npicks(i))
      enddo
c
      close(lu, err=96)
c
      return
   50 format('offset dependent time domain taper'/a)
   51 format(/'taper set ',i2,' with ',i3,' picks'/' offset [m]       time [s]')
   52 format(2g15.6)
   99 stop 'ERROR (tf_ttapwrite): opening file'
   98 stop 'ERROR (tf_ttapwrite): writing file'
   96 stop 'ERROR (tf_ttapwrite): closing file'
      end
c
c----------------------------------------------------------------------
cS
c
      real function tf_taptime(offset, t, x, npicks)
c
c this function returns the linearly interpolated time between taper
c picks for offset. outside the defined taper the time of the most distant pick
c or the least distant pick is given respectively.
c
      real t(npicks), x(npicks), offset
      integer npicks
c
cE
      integer i
      real result
c 
      if (offset.lt.x(1)) then
        result=t(1)
      elseif (offset.gt.x(npicks)) then
        result=t(npicks)
      else
        i=0
        do while ((i.lt.npicks).and.(offset.gt.x(i+1)))
          i=i+1
        enddo
        result=t(i)+((t(i+1)-t(i))/(x(i+1)-x(i)))*(offset-x(i))
      endif
c 
      tf_taptime=result
c 
      return
      end
c
c----------------------------------------------------------------------
cS
c
      subroutine tf_ttapeval(offset, timeoffirst, dt, data, nsamples,
     &  x, t, npicks, maxpicks,verbose)
c 
c taper a complete time series with offset dependent taper
c
      real t(maxpicks,4)
      real x(maxpicks,4)
      integer npicks(maxpicks)
      integer maxpicks, nsamples
      real offset, timeoffirst, dt, data(nsamples)
      logical verbose
c
cE
      integer it(4)
      real tap(4)
      integer i
      real tf_taptime, tf_costap
c 
      do i=1,4
        tap(i)=tf_taptime(offset, t(1,i), x(1,i), npicks(i))
        it(i)=int((tap(i)-timeoffirst)/dt)
        if (i.gt.2) it(i)=it(i)+1
        it(i)=min(it(i),nsamples)
        it(i)=max(it(i),1)
      enddo
c 
      if (verbose) print 50,offset,(tap(i),i=1,4)
c 
      do i=1,nsamples
        data(i)=data(i)*tf_costap(i,it(1),it(2),it(3),it(4))
      enddo
c 
      return
   50 format('cosine taper at ',f6.1,'m: lb: ',f6.3,'s le: ',
     &  f6.3,'s rb: ',f6.3,'s re: ',f6.3,'s')
      end
c
c ----- END OF tf_ttaper.f -----
