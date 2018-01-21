c this is <sousou_skipdata.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c skip a full datablock
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
c    09/07/98   V1.0   Thomas Forbriger
c    17/11/98          taken from fidase
c
c==============================================================================
c
      subroutine skipdata(lu, last)
c
c skip a full data block at unit lu
c last will be true is this was the last trace in the file
c after reading the last trace this routine will close lu
c 
      integer lu
      logical last
c
cE
c declare variables
      integer ierr
      character code*20
c go
      call sff_skipdata(lu, code, last, ierr)
      if (ierr.ne.0) stop 'ERROR (skipdata): skipping trace'
      return
      end
c
c ----- END OF fidase_skipdata.f -----
