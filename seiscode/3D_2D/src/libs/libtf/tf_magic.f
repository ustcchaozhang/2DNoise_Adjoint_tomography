c this is <tf_magic.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c create magic number from string
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
c    26/06/97   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      subroutine tf_magic(cmagic, magic)
c
c If you give me a 4-character magic number string, I will provide
c you with the corresponding magic number indicating your CPU bytesex.
c
      character*4 cmagic
      integer magic
c
cE
      integer i, v
c
      magic=0
      do i=1,4
        v=ichar(cmagic(i:i))
        if (v.lt.0) v=v+256
        magic=magic*256+v
      enddo
      return
      end
c
c ----- END OF tf_magic.f -----
