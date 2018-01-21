c this is <seife_first.f>
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
c    02/09/2005   V1.1   provide function to apply first to filtered series
c
c==============================================================================
c
      subroutine seife_first(x,n)
      real*8 x(n),x0
      x0=x(1)
      do 1 i=1,n
    1 x(i)=x(i)-x0
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine seife_resetfirst(nil,par,x,n,r,msg)
      character par*35,msg*(*)
      logical nil
      real*8 x(n)
      logical r
      integer i
      save 
      real*8 x0
      if (r) then
        read(par,*) n2
        if(n2.le.1.or.n2.gt.n) n2=n
        x0=0.
        do i=1,n2
          x0=x0+x(i)
        enddo
        x0=x0/n2
        do i=1,n
          x(i)=x(i)-x0
        enddo
        write(msg,90) n2
      else
        do i=1,n
          x(i)=x(i)+x0
        enddo
        msg='restored value of first sample'
      endif
      nil=.false.
      return
   90 format('fir ',i3)
      end
c
c ----- END OF seife_first.f -----
