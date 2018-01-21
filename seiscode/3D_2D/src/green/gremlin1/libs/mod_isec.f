c this is <mod_isec.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c return section index containing depth in model
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
c    24/03/98   V1.0   Thomas Forbriger
c    13/01/99   V2.0   model definition changed (see glq_model.inc)
c
      integer function mod_isec(depth, mod)
c
      real depth
      integer mod
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer i
c 
      if (verb_subaction) print *,'ENTER mod_isec(',depth,',',mod,')'
c
c      i=glqm_nsec
c      do while ((mdepth(i, mod).gt.depth).and.(i.gt.0))
c        i=i-1
c      enddo
      i=1
      do while ((mdepth(i, mod).lt.depth).and.(i.lt.glqm_nsec))
        i=i+1
      enddo
c 
      mod_isec=i
c 
      if (verb_subaction) print *,'LEAVE mod_isec (result=',i,')'
c 
      return
      end
c
c ----- END OF mod_isec.f -----
