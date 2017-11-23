c this is <seife_factor.f>
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
c    18/01/2008   V1.1   added function seife_mim
c
c==============================================================================
c
      subroutine seife_factor(nil,par,x,n,msg)
c  multiply by a constant factor
      double precision x(n),f
      character par*35,msg*(*)
      logical nil
      read(par,*) f
      do 1 j=1,n
    1 x(j)=f*x(j)
      write(msg,'("fac  ",g10.3)') f
      nil=.false.
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine seife_mim(nil,par,x,n,msg)
c  report minimum and maximum value
      double precision x(n)
      double precision m1,m2
      character par*35,msg*(*)
      logical nil
      m1=x(1)
      m2=m1
      do j=1,n
        m1=min(m1,x(j))
        m2=max(m2,x(j))
      enddo
      write(msg,'("mim  ",g10.3,1x,g10.3)') m1,m2
      nil=.false.
      return
      end
c
c ----- END OF seife_factor.f -----
