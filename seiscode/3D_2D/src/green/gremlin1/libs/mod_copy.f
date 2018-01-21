c this is <mod_copy.f>
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
c 
cS
c----------------------------------------------------------------------
c
      subroutine mod_copy(from, to)
c
c copy a model
c
c from:   index of source
c to:     index of target
c
      integer from, to
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer i,j,k
c 
      if (verb_subaction) print *,'ENTER mod_copy(',from,',',to,')'
c 
      if (verb_subaction) print *,
     &   'NOTICE (mod_copy): copy model ',from,' to ',to
c
      if ((from.gt.glqm_mmod).or.(from.lt.1))
     &  call mod_panic('ERROR (mod_copy): source index out of range')
      if ((to.gt.glqm_mmod).or.(to.lt.1))
     &  call mod_panic('ERROR (mod_copy): target index out of range')
c 
      do i=1,glqm_nsec
        mdepth(i, to)=mdepth(i, from)
        do j=1,glqm_mpar
          do k=1,glqm_mpol
            model(k, i, j, to)=model(k, i, j, from)
          enddo
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE mod_copy'
c 
      return
      end
c
c ----- END OF mod_copy.f -----
