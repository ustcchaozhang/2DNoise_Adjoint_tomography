c this is <seife_normalize.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
c
c normalize to given value
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
c
c REVISIONS and CHANGES
c    09/03/2010   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine seife_normalize(nil,par,x,n,msg)
c  normalize to given absolute maximum
      double precision x(n),f,v
      character par*35,msg*(*)
      logical nil
      read(par,*) f
      v=0.d0
      do j=1,n
        v=max(v,abs(x(j)))
      enddo
      v=f/v
      do j=1,n
        x(j)=x(j)*v
      enddo
      write(msg,'("nrm  ",g10.3)') f
      nil=.false.
      return
      end
c
c ----- END OF seife_normalize.f ----- 
