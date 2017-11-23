c this is <tf_realtoint.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c convert real array to integer
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
c tf_realtoint
c
c converts an real*4 dataset to integer dataset
c (used for sff data)
      subroutine tf_realtoint(nsamples, maxsamples,
     &         idata, rdata, ampfac)
c parameters
      integer nsamples, maxsamples
      integer idata(maxsamples)
      real rdata(maxsamples)
      real ampfac
c
cE
c variables
      integer sample
c go
      ampfac=0.
      do sample=1,nsamples
        ampfac=max(ampfac, abs(rdata(sample)))
      enddo
      ampfac=ampfac/(2.**23)
      if (ampfac.eq.0.) ampfac=1.
      do sample=1,nsamples
        idata(sample)=nint(rdata(sample)/ampfac)
      enddo
      return
      end
c
c
c ----- END OF tf_realtoint.f -----
