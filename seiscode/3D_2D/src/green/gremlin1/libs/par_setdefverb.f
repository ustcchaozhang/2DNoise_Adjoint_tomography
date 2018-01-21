c this is <par_setdefverb.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c After splitting the source into different object files, the linker
c refuses to link blockdata subroutines.
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
c    06/04/98   V1.0   Thomas Forbriger
c    24/01/99   V1.1   new option verb_changes
c
c==============================================================================
c
cS
c
      subroutine par_setdefverb
c 
      include 'glq_verbose.inc'
c 
c verbosity
      verb_subaction=.false.
      verb_subresult=.false.
      verb_subinput=.false.
      verb_substrategy=.false.
      verb_medstrategy=.true.
      verb_topstrategy=.true.
      verb_allwarn=.true.
      verb_io=.true.
      verb_graph=.true.
      verb_debug=.false.
      verb_changes=.true.
      verb_model=.false.
c 
      return
      end
c
cE
c
c ----- END OF par_setdefverb.f -----
