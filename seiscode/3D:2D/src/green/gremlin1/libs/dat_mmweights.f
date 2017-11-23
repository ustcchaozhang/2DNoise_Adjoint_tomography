cS
c this is <dat_mmweights.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
c create data weights from misfit
c
c weights are calculated and written to rgweight and rtweights. subsequently
c dat_dmode is called to transfer weights to gweight and tweight.
c 
c if
c
c   d(j)    is a data value and
c   s(j)    is the corresponding reference synthetic and
c   e       is the given error level (data tolerance), then
c   w(j)    is the weight value and is
c
c                       |d(j)|**2 + |s(j)|**2
c           fac1,2**2 * ---------------------  +  (thres1*e)**2
c                                2
c   w(j) =  ---------------------------------------------------
c                    |d(j)-s(j)|**2 + (thres2*e)**2
c
c where fac1 is used for traveltimes and fac2 is used for greens
c
c REVISIONS and CHANGES
c    07/05/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine dat_mmweights(thres1,thres2,fac1,fac2)
c
c parameters
c thresholds relative to error value
      real thres1,thres2,fac1,fac2
c
c get common blocks
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
      include 'glq_para.inc'
c
cE
      integer i,j
      real ttth1,ttth2,gth1,gth2,f1,f2
c 
      ttth1=(thres1*tterror)**2
      ttth2=(thres2*tterror)**2
      gth1=(thres1*gerror)**2
      gth2=(thres2*gerror)**2
      f1=fac1**2
      f2=fac2**2
c
c zero arrays
      do i=1,glqd_mslo
        do j=1,glqd_mfre
          rgweight(i,j)=0.
        enddo
      enddo
c 
      do i=1,glqd_mtts
        rtweight(i)=0.
      enddo
c
c calculate weights from relative misfit
      do i=1,rng_xmax
        rtweight(i)=(f1*0.5*(travt(i,di_mread)**2+travt(i,di_mref)**2)
     &               +ttth1)/((travt(i,di_mread)-travt(i,di_mref))**2
     &               +ttth2)
      enddo
c
      do i=rng_smin,rng_smax
        do j=rng_fmin,rng_fmax
          rgweight(i,j)=(f2*0.5*(real(green(i,j,di_mread))**2+
     &                      imag(green(i,j,di_mread))**2+
     &                      real(green(i,j,di_mref))**2+
     &                      imag(green(i,j,di_mref))**2)+
     &                 gth1)/(gth2+
     &                 real(green(i,j,di_mread)-green(i,j,di_mref))**2+
     &                 imag(green(i,j,di_mread)-green(i,j,di_mref))**2)
        enddo
      enddo
c
      call dat_dmode
c
      return
      end
c
c ----- END OF dat_mmweights.f ----- 
