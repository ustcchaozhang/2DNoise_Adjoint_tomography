c this is <seife_gauss.f>
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
      subroutine seife_gauss(aik,m,n,rs,f)
c  solve linear equations
      implicit real*8 (a-h,o-z)
      dimension aik(n,n),rs(n),f(n),h(14),imax(13)
      include 'seife_common.inc'
c 
      if (ldebug) then
        print 50,'DEBUG (seife_gauss): ',m,n
      endif
      do 1401 j=1,m
      aikmax=0.d0
      do 1402 k=1,m
      h(k)=aik(j,k)
      if(abs(h(k)).le.aikmax) go to 1402
      aikmax=abs(h(k))
      index=k
 1402 continue
      h(m+1)=rs(j)
      do 1403 k=1,m
      q=aik(k,index)/h(index)
      do 1404 l=1,m
 1404 aik(k,l)=aik(k,l)-q*h(l)
 1403 rs(k)=rs(k)-q*h(m+1)
      do 1405 k=1,m
 1405 aik(j,k)=h(k)
      rs(j)=h(m+1)
 1401 imax(j)=index
      do 1406 j=1,m
      index=imax(j)
 1406 f(index)=rs(j)/aik(j,index)
      return
   50 format(a,'matrix size: m=',i4,' n=',i4)
      end
c
c ----- END OF seife_gauss.f -----
