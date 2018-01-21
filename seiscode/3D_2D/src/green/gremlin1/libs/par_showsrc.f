c this is <par_showsrc.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c
c==============================================================================
c
cS
c----------------------------------------------------------------------
c 
      subroutine par_showsrc
c 
c display parameter source set
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
c
cE
      print 50, src_depth, src_amp, src_type
c 
      return
   50 format(/'source parameters',/
     &       '  source depth',t30,'[sdep]: ',f10.7,' km',/
     &       '  source amplitude',t30,'[samp]: ',f10.3,/
     &       '  source type',t30,'[styp]: ',i10/
     &       '  (type=1: vertical force    type=2: explosion)')
      end
c
c ----- END OF par_showsrc.f -----
