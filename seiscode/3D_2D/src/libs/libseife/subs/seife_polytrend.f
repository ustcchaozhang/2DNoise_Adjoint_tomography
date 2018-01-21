c this is <seife_polytrend.f>
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
      subroutine seife_polytrend(nil,par,x,n, msg)
c  remove polynomial trend
      parameter(ndi=6)
      implicit real*8 (a-h,o-z)
      dimension x(n),b(ndi),c(ndi,ndi),a(ndi)
      logical nil
      character msg*(*)
      character*35 par
      read(par,*) m
      m=min(m,ndi-1)
      fnh=n/2.
      one=1.d0

      do j=1, m+1
        do k=1, m+1
          c(j,k)=0
          do i=1, n
            c(j,k)=c(j,k)+(dble(i)/fnh-one)**(j+k-2)
          enddo
        enddo
        b(j)=0
        do i=1,n
          b(j)=b(j)+(dble(i)/fnh-one)**(j-1)*x(i)
        enddo
      enddo
      call seife_gauss(c,m+1,ndi,b,a)
c      write(msg,100) (j-1,a(j),j=1,m+1)
c  100 format(i5,e15.6)
      do i=1,n
        xpol=a(m+1)
        do j=m,1,-1
          xpol=xpol*(dble(i)/fnh-one)+a(j)
        enddo
        x(i)=x(i)-xpol
      enddo

      write(msg,'("pol  ",i5)') m
      nil=.false.
      return
      end
c
c ----- END OF seife_polytrend.f -----
