c this is <par_defverb.f>
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
c
      block data par_defverb
c 
      include 'glq_verbose.inc'
c 
c verbosity
      data verb_subaction, verb_subresult, verb_subinput,
     &     verb_substrategy, verb_medstrategy/ 5*.false./
      data verb_topstrategy, verb_allwarn, verb_io/ 3*.true./
      data verb_graph/ .true. /
c 
      end
c
cE
c ----- END OF par_defverb.f -----
