c this is <f77procs.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c Fortran 77 subroutines
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
c    22/12/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine fillarray(vec1, vec2, n1, n2)
c
c writes to common block
c
      include 'f77common.inc'
c
      integer n1,n2
      real vec1(n1),vec2(n2)
c
      double complex ime
      parameter(ime=(0.d0,1.d0))
c
      integer i,j
c
      if (n1.gt.amax) stop 'ERROR: first dimension is too large'
      if (n2.gt.bmax) stop 'ERROR: second dimension is too large'
      na=n1
      nb=n2
      do i=1,n1
        do j=1,n2
          array(i,j)=vec1(i)+ime*vec2(j)
        enddo
      enddo
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine sums(vec,nmax,n)
c
c reads from common block
c
      include 'f77common.inc'
c
      complex vec(nmax)
      integer n,nmax
c
      integer i,j
c
      if (nb.gt.nmax) stop 'ERROR: vector too small'
      n=nb
      do i=1,nb
        vec(i)=(0.,0.)
        do j=1,na
          vec(i)=vec(i)+array(j,i)
        enddo
      enddo
      return
      end
c
c----------------------------------------------------------------------
c
      double complex function total(i)
c
c returns a value derived from common block
c
      include 'f77common.inc'
c
      integer i
c
      double complex result
      integer j
c
      if ((i.lt.1).or.(i.gt.na)) stop 'ERROR: illegal index'
      result=(0.d0,0.d0)
      do j=1,nb
        result=result+array(i,j)
      enddo
      total=result
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine fill(a, ld1, n1, ld2, n2, ld3, n3)
c
c fill a three-domensional array that was passed to the subroutine
c
      integer ld1,n1,ld2,n2,ld3,n3
      integer a(ld1,ld2,ld3)
c
      integer i,j,k
c
      do i=1,n1
        do j=1,n2
          do k=1,n3
            a(i,j,k)=i+10*j+100*k
          enddo
        enddo
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine comdim(maxa, maxb)
c
c we have no access to the defined dimensions of the common block
c this subroutines passes the values to the rest of the world
c
      integer maxa,maxb
c
      include 'f77common.inc'
c
      maxa=amax
      maxb=bmax
c
      return
      end
c
c ----- END OF f77procs.f ----- 
