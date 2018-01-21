c this is <par_definv.f>
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
cS
c
      block data par_definv
c 
      include 'glq_invpara.inc'
c 
c least squares parameters
      data lq_nmean/5/
      data lq_msteps/20/
      data lq_mindown/5/
      data lq_relimp/1.e-5/
      data lq_x2lim/0.1/
      data lq_numax/1.e3/
      data lq_numin/1.e-10/
c 
      end
cE
c ----- END OF par_definv.f -----
