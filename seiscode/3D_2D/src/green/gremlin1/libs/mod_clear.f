c this is <mod_clear.f>
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
      subroutine mod_clear(n, i)
c 
c clear model space and set number of sections
c
c n:  number of sections
c i:  model index within array
c
      integer i, n
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer j, k, l
c 
      if (verb_subaction) print *,'ENTER mod_clear(',n,',',i,')'
c 
      if (verb_subaction) print *,
     &  'NOTICE (mod_clear): clear named polynomial model ',i
      if (verb_subaction) print *,
     &  'NOTICE (mod_clear): set number of polynomial sections to ',n
c 
      glqm_nsec=n
      do j=1,glqm_nsec
        mdepth(j, i)=0.
        do k=1,glqm_mpar
          glqm_npol(j,k)=1
          glqm_follow(j,k)=.false.
          do l=1,glqm_mpol
            model(l, j, k, i)=0.
          enddo
        enddo
      enddo
c 
      if (verb_subresult) print *,'NOTICE: cleared model space!'
c 
      if (verb_subaction) print *,'LEAVE mod_clear'
c 
      return
      end
c
c ----- END OF mod_clear.f -----
