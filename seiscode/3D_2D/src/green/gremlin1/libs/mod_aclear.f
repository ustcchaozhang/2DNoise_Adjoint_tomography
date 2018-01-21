c this is <mod_aclear.f>
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
      subroutine mod_aclear
c
c clear anonymous parameters
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer i
c 
      if (verb_subaction) print *,'ENTER mod_aclear'
c 
      if (verb_subaction) print *,
     &  'NOTICE (mod_aclear): clear anonymous model parameters'
c
      do i=1,glqm_mano
        mdelta(i)=0.
      enddo
c 
      if (verb_subaction) print *,'LEAVE mod_aclear'
c
      return
      end
c
c ----- END OF mod_aclear.f -----
