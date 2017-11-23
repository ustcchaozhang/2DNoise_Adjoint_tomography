c this is <tf_swap.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c swap memory bytes to match processor bytesex
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
c    25/06/97   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      subroutine tf_swap(array, n)
c
c swap bytesex of array
c
      integer n, array(n)
c 
cE
      integer v,i
      character*1 c(5)
      equivalence(v,c)
c     
      if (n.lt.1) 
     &  stop 'ERROR (tf_swap): number of elements to swap is less than one'
      do i=1,n
        v=array(i)
        c(5)=c(1)
        c(1)=c(4)
        c(4)=c(5)
        c(5)=c(2)
        c(2)=c(3)
        c(3)=c(5)
        array(i)=v
      enddo
      return
      end
c
c ----- END OF tf_swap.f -----
