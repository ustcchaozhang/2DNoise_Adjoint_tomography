c this is <tf_doubletoint.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c convert double precision array to integer array
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
c    17/12/07   V1.1   use double precision
c    18/01/07   V1.2   used input factor ampfac for scaling
c                      ampfac was set do dampfac after scaling was done
c                      it is a miracle to me how this function could ever have
c                      worked correctly - it is routinely used for data
c                      preparation in stufi
c
c==============================================================================
cS
c
c tf_doubletoint
c
c converts an real*8 dataset to integer dataset
c (used for sff data)
      subroutine tf_doubletoint(nsamples, maxsamples,
     &         idata, rdata, ampfac)
c parameters
      integer nsamples, maxsamples
      integer idata(maxsamples)
      double precision rdata(maxsamples)
      real ampfac
c 
cE
c variables
      integer sample
      double precision dampfac, scalfac
c go
      dampfac=0.
      do sample=1,nsamples
        dampfac=max(dampfac, abs(rdata(sample)))
c        print *,'DEBUG: ',sample,rdata(sample),dampfac
      enddo
c      print *,'DEBUG: dampfac ',dampfac
c 
c This routine scales the integer dataset to a maximum amplitude of 2.**24.
c The integer dataset may contain values up to 2.**31-1 and the 
c double precision dataset has also a larger dynamic range than 2.**24.
c There occured problems with higher amplitudes than 2.**24 because
c the sff writing algorithm calculates second differences. On high
c frequencies this may lead to an increase in amplitude by a factor 4.
c This might exceed the integer range. Therefor we use 2.**24
c which should provide a good resolution for most time series.
c
      dampfac=dampfac/(2.d0**24)
      if (dampfac.eq.0.) dampfac=1.
c      print *,'DEBUG: dampfac ',dampfac
      ampfac=dampfac
      scalfac=1.d0/dampfac
      do sample=1,nsamples
        idata(sample)=nint(rdata(sample)*scalfac)
c        print *,'DEBUG: ',sample,idata(sample)
      enddo
      return
      end
c
c ----- END OF tf_doubletoint.f -----
