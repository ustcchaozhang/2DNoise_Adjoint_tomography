c this is <tt_ingeom.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c get frame geometry
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
      subroutine tt_ingeom(ilay, cmplength, thick1, thick2)
c
c declare parameters
      integer ilay
      real cmplength, thick1, thick2
      include 'tt_dim.inc'
      include 'tt_model.inc'
      include 'tt_work.inc'
c
cE
c
c------------------------------------------------------------------------------
c go
      if ((ilay.lt.1).or.(ilay.gt.nlay)) 
     &  stop 'ERROR (tt_ingeom): layer index out of range'
      cmplength=cmp(ilay)
      thick1=d1(ilay)
      thick2=d2(ilay)
c
      return
      end
c
c ----- END OF tt_ingeom.f -----
