c this is <seife_delay.f>
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
      subroutine seife_delay(nil,par,x,n,dt,tsec,msg)
c  interpolate between samples
      real*8 x(n),xx,b,bb
      character par*35,msg*(*)
      logical nil
      read(par,*) t
      if(t.gt.0.) then
        m=int(t/dt)+1
        b=m-t/dt
        xx=x(n+1-m)
        do 2 j=n,m+1,-1
    2     x(j)=x(j-m)
        do 3 j=2,m
    3     x(j)=x(1)
      else
        m=int(-t/dt)
        b=-m-t/dt
        do 4 j=1,n-m
    4     x(j)=x(j+m)
          do 5 j=n-m,n
    5       x(j)=x(n)
        xx=x(n)
      endif
      bb=1.d0-b
      do 1 j=1,n-1
    1 x(j)=bb*x(j)+b*x(j+1)
      x(n)=bb*x(n)+b*xx
      write(msg,'("del  ",f10.2"   tsec not adjusted")') t
c     tsec=tsec-t*dt
      nil=.false.
      return
      end
c
c ----- END OF seife_delay.f -----
