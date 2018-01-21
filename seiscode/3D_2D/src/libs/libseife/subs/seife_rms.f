c this is <seife_rms.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
c
c calculate rms value
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
c    01/03/2004   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine seife_rms(nil,par,x,n,dt,msg)
c
c declare parameters
      real*8 x(n)
      character par*(*), msg*(*)
      logical nil
c
cE
c------------------------------------------------------------------------------
c go
      double precision rms=0.d0
      do 1 j=1,n
    1   rms=rms+x(j)**2
      rms=sqrt(rms/dble(n))
      write(msg,'("rms value: ",e10.3)') rms
      nil=.false.
      return
      end
c
c ----- END OF seife_rms.f ----- 
