c this is <tfstr_trimlen.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
c
c find counter to discard trailing spaces
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
c    19/01/99   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      integer function tfstr_trimlen(string)
c 
c function returns index of last non-space character in string
c
c declare parameters
      character*(*) string
c 
cE
c
      do tfstr_trimlen=LEN(string),1,-1
        if (string(tfstr_trimlen:tfstr_trimlen).ne.' ') return
      enddo
c
      return
      end
c
c ----- END OF tfstr_trimlen.f -----
