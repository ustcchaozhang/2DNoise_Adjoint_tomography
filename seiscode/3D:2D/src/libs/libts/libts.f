c this is <libts.f>
cS
c======================================================================
c
c Copyright (c) 1997, 2013 by Thomas Forbriger (IfG Stuttgart)
c
c This library containes some usefull routines for time series handling.
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
c
c   12/05/1997    V1.00    Thomas Forbriger (IfG Stuttgart)
c   27/06/1997    V1.01    some new routines
c   13/12/2007    V1.02    modifications to ts_linint to satisfy compiler
c                          ts_linint is not yet implemented
c   17/12/2007    V1.03    ts_linint is put into comments
c
c======================================================================
cS
c contents
c
c ts_libversion   V1.00    returns the actual library version
c ts_rms          V1.00    returns rms avlue of dataset
c ts_average      V1.00    returns mean value of dataset
c ts_peak         V1.00    evaluate peak values
c ts_linint       V1.00    return linearly interpolated value
c ts_min          V1.01    calculate minimum value
c ts_max          V1.01    calculate maximum value
c
cE
c======================================================================
c 
c subroutines
cS
c----------------------------------------------------------------------
      real function ts_libversion()
c 
c returns the actual version number
c
cE
      real version
      version=1.03
      ts_libversion=version
      return
      end
cS
c----------------------------------------------------------------------
      real function ts_rms(data, nsamples)
c
c returns the rms value of the single precision dataset
c
      integer nsamples
      real data(nsamples)
cE
      double precision rmssum
      real rms
      integer sample
c 
      rmssum=0
      do sample=1,nsamples
        rmssum=rmssum+data(sample)*data(sample)
      enddo
      rms=sqrt(rmssum)/nsamples
      ts_rms=rms
      return
      end
cS
c----------------------------------------------------------------------
      real function ts_average(data, nsamples)
c
c returns the mean of the single precision dataset
c
      integer nsamples
      real data(nsamples)
cE
      double precision avgsum
      real average
      integer sample
c 
      avgsum=0
      do sample=1,nsamples
        avgsum=avgsum+data(sample)
      enddo
      average=avgsum/float(nsamples)
      ts_average=average
      return
      end
cS
c----------------------------------------------------------------------
      subroutine ts_peak(data, n, min, max, imin, imax)
c 
c scan array data over n samples and set the min, max variables to
c the peak values and imin, imax to the peak value indizes
c
      integer n
      real data(n)
      real min, max
      integer imin, imax
cE
      integer sample
c
      min=data(1)
      max=data(1)
      imin=1
      imax=1
      do sample=1,n
        if (min.gt.data(sample)) then
          min=data(sample)
          imin=sample
        endif
        if (max.lt.data(sample)) then
          max=data(sample)
          imax=sample
        endif
      enddo
      return
      end
cS
c----------------------------------------------------------------------
cc      real function ts_linint(data, n, dt, tanf, t)
c 
c returns a linearly interpolated datavalue
c
c input:
c   data     time series
c   n        number of samples
c   dt       sampling interval
c   tanf     starting time of time series in seconds
c   t        time for which we look for the interpolated value
c
cc      integer n
cc      real data(n)
cc      real dt, tanf, t
cE
c      double precision deltat
cc      print *,'OH not yet implemented in libts.f'
cc      ts_linint=0.
cc      return
cc      end
cS
c----------------------------------------------------------------------
c  
c calculate maximum value of a certain dataset
c
      real function ts_max(data, n)
c  
      real data(n)
      integer n
cE 
      integer i
      real maxval
c  
      maxval=data(1)
      do i=1,n
        maxval=max(data(i),maxval)
      enddo
      ts_max=maxval
      return
      end 
cS 
c----------------------------------------------------------------------
c  
c calculate minimum value of a certain dataset
c
      real function ts_min(data, n)
c  
      real data(n)
      integer n
cE 
      integer i
      real minval
c  
      minval=data(1)
      do i=1,n
        minval=min(data(i),minval)
      enddo
      ts_min=minval
      return
      end 
c  
cE
c ----- END OF libts.f -----
