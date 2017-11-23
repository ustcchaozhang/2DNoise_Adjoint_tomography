c this is <seife_rekfl.f>
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
c    11/07/2005   V1.1   support debug mode
c
c==============================================================================
c
      subroutine seife_rekfl(x,y,n,f0,f1,f2,g1,g2)
c  perform recursive filtering
c 
      implicit real*8 (a-h,o-z)
      dimension x(n),y(n)
      include 'seife_common.inc'
c 
      if (ldebug) then
        print 50,'DEBUG (seife_rekfl): ',f0,f1,f2,g1,g2
      endif
      xa=0.d0
      xaa=0.d0
      ya=0.d0
      yaa=0.d0
      do 1 j=1,n
      xn=x(j)
      y(j)=f0*xn+f1*xa+f2*xaa+g1*ya+g2*yaa
      xaa=xa
      xa=xn
      yaa=ya
    1 ya=y(j)
      return
   50 format (a, ' f0=',g10.3, ' f1=',g10.3, ' f2=',g10.3, 
     &  ' g1=',g10.3, ' g2=',g10.3)
      end
c
c ----- END OF seife_rekfl.f -----
