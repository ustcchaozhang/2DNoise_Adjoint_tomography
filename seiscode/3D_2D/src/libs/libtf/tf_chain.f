c this is <tf_chain.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c Build a chain from arrays
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
c    25/07/97   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
c build a chain for a real valued array
c 
c array:     is the real valued array for which we will build the chain
c chain:     is a pointer-chain
c first:     will indicate the first chain element
c updown:    >0: build chain for increasing order
c            <0: build chain for decreasing order
c
      subroutine tf_rchain(array, chain, n, first, updown)
      real array(n)
      integer n, chain(n), first, updown
c
cE
      integer i, j, k, ud
c 
      if (n.lt.1) stop 'ERROR (tf_rchain): no elements'
c
      do i=1,n
        chain(i)=-1
      enddo
      first=1
      ud=sign(1,updown)
c
      if (n.gt.1) then
        do i=2,n 
          if ((ud*array(i)).lt.(ud*array(first))) then
            chain(i)=first
            first=i
          else
            j=first
    1       continue
              k=j
              j=chain(k)
              if (j.lt.0) then
                chain(k)=i
              else
                if ((ud*array(i)).lt.(ud*array(j))) then
                  chain(i)=j
                  chain(k)=i
                else
                  goto 1
                endif
              endif
          endif
        enddo
      endif
c 
      return
      end
c
c ----- END OF tf_chain.f -----
