c this is <seife_baseline.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c force signal to its baseline
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
c    14/06/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine seife_baseline(nil,par,x,n,msg)
c 
      character par*35,msg*(*)
      integer n
      real*8 x(n)
      logical nil
c 
      integer n2,j,n3
      double precision va,ve,m
c 
      read(par,*) n2,n3
      if(n2.lt.1.or.n2.gt.n) n2=n
      if(n3.lt.1.or.n3.gt.n) n3=n
      print *,n2,n3,n
      va=0.d0
      ve=0.d0
      do j=1,n2
        va=va+x(j)
        ve=ve+x(n3+1-j)
      enddo
      va=va/n2
      ve=ve/n2
      m=(ve-va)/(n3-n2-1.d0)
      do j=1,n
        x(j)=x(j)-va-m*(j-1.d0)
      enddo
      write(msg,'("fbl  ",3(2x,e16.6))') va,ve,m
      nil=.false.
c 
      return
      end
c
c ----- END OF seife_baseline.f ----- 
