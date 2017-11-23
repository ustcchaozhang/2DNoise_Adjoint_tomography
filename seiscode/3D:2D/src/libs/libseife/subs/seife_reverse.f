c this is <seife_reverse.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 1984 by Erhard Wielandt
c This code was part of seife.f. A current version of seife.f can be obtained
c from http://www.software-for-seismometry.de/
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
c extracted from libseife.f
c
c REVISIONS and CHANGES
c    25/10/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine seife_reverse(nil,par,x,n,msg)
c  reverse signal in time
      implicit real*8 (a-h,o-z)
      logical nil
      dimension x(n)
      character par*35,msg*(*)
      do j=1,n/2
        jj=n+1-j
        xx=x(j)
        x(j)=x(jj)
        x(jj)=xx
      enddo
      write(msg,'("rev - signal was reversed")')
      nil=.false.
      return
      end
c
c ----- END OF seife_reverse.f -----
