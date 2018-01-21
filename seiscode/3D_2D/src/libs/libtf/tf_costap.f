c this is <tf_costab.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c evaluate cosine taper function
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
c    31/07/97   V1.1   right edge of taper was completely wrong!
c    08/08/97   V1.2   taper was still wrong 
c                      now cut regions start and end smooth
c                      added a second taper with hard edges
c
c==============================================================================
cS
c
c cosine tapering-function
c
      real function tf_costap(sample,  
     &    wilb, wile, wirb, wire) 
c
c declare parameters
c   
c   sample:      number of sample for which we desire the factor
c   wilb:        first sample of left taper
c   wile:        last sample of left taper
c   wirb:        first sample of right taper
c   wire:        last sample of right taper
      integer sample
      integer wilb, wile, wirb, wire
c 
cE
      real pi, tapval
      parameter(pi=3.141592653)
c
c go ahead
c
      if (sample.lt.wilb) then
        tapval=0.
      elseif (sample.lt.wile) then
        tapval=0.5*(1.-cos(pi*float(sample-wilb)/float(wile-wilb)))
      elseif (sample.lt.wirb) then
        tapval=1.
      elseif (sample.lt.wire) then
        tapval=0.5*(1.+cos(pi*float(sample-wirb)/float(wire-wirb)))
      else
        tapval=0.
      endif
      tf_costap=tapval
      return
      end
c----------------------------------------------------------------------
cS
c
c cosine tapering-function with hard edges
c
      real function tf_sincostap(sample,  
     &    wilb, wile, wirb, wire) 
c
c declare parameters
c   
c   sample:      number of sample for which we desire the factor
c   wilb:        first sample of left taper
c   wile:        last sample of left taper
c   wirb:        first sample of right taper
c   wire:        last sample of right taper
      integer sample
      integer wilb, wile, wirb, wire
c 
cE
      real pi2, tapval
      parameter(pi2=0.5*3.141592653)
c
c go ahead
c
      if (sample.lt.wilb) then
        tapval=0.
      elseif (sample.lt.wile) then
        tapval=sin(pi2*float(sample-wilb)/float(wile-wilb))
      elseif (sample.lt.wirb) then
        tapval=1.
      elseif (sample.lt.wire) then
        tapval=cos(pi2*float(sample-wirb)/float(wire-wirb))
      else
        tapval=0.
      endif
      tf_sincostap=tapval
      return
      end
c
c ----- END OF tf_costap.f -----
