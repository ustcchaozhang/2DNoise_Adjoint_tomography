c this is <tf_inttodouble.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c convert an integer array to double precision
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
c    24/06/97   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
c tf_inttodouble
c
c converts an integer dataset to real*8 dataset
c (used for sff data)
      subroutine tf_inttodouble(nsamples, maxsamples,
     &        idata, rdata, ampfac)
c parameters
      integer nsamples, maxsamples
      integer idata(maxsamples)
      double precision rdata(maxsamples)
      real ampfac
c
cE
c variables
      integer sample
c go
      do sample=1,nsamples
        rdata(sample)=dble(idata(sample))*dble(ampfac)
      enddo
      return
      end
c
c
c ----- END OF tf_inttodouble.f -----
