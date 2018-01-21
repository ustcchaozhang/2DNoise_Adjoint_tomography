c this is <seife_trend.f>
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
      subroutine seife_trend(nil,par,x,n,msg)
c  remove trend
      implicit real*8 (a-h,o-z)
      logical nil
      character par*35,msg*(*)
      dimension x(n)
      read(par,*) n2
      if(n2.le.1.or.n2.gt.n) n2=n
      gn = n2
      alpha = 0.5d0*gn*(gn+1.d0)
      beta = (2.d0*gn+1.d0)*(gn+1.d0)*gn/6.d0
      det = gn*beta-alpha*alpha
      sx = 0.d0
      sjx = 0.d0
      do 1001 j=1,n2
      sx = sx+x(j)
 1001 sjx = sjx+x(j)*j
      a = (sx*beta-sjx*alpha)/det
      b = (sjx*gn-sx*alpha)/det
      do 1002 j=1,n
 1002 x(j) = x(j)-a-b*j
      write(msg,'("tre  ",2f10.3)') a,b
      nil=.false.
      return
      end
c
c ----- END OF seife_trend.f -----
