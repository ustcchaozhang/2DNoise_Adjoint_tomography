c this is <tt_intt.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c return travel times
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
c    17/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine tt_intt(offset,t1,t2)
c
c declare parameters
      real offset,t1,t2
      include 'tt_dim.inc'
      include 'tt_model.inc'
      include 'tt_work.inc'
c
cE
c declare local variables
      real offset2
      integer i
c
c------------------------------------------------------------------------------
c go
      offset2=backoff-offset
      t1=-1.
      t2=-1.
      if (offset.ge.0.) then
        t1=ti1(1)+offset*sapp1(1)
        do i=2,nlay
          t1=min(t1,ti1(i)+offset*sapp1(i))
        enddo
      endif
      if (offset2.ge.0.) then
        t2=ti2(1)+offset2*sapp2(1)
        do i=2,nlay
          t2=min(t2,ti2(i)+offset2*sapp2(i))
        enddo
      endif
c
      return
      end
c
c ----- END OF tt_intt.f -----
