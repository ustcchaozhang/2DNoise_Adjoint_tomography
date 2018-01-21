c this is <tt_inlay.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c return number of layers
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
c    16/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine tt_inlay(numlay)
c
c declare parameters
      integer numlay
      include 'tt_dim.inc'
      include 'tt_model.inc'
c
cE
c
c------------------------------------------------------------------------------
c go
      numlay=nlay
c
      return
      end
c
c ----- END OF tt_inlay.f -----
