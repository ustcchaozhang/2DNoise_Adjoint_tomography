c this is <mod_follow.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c make a section parameter follow its lower neighbour
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
c    13/01/99   V2.0   model definition changed (see glq_model.inc)
c
      subroutine mod_follow(modindex)
c
c modindex:   index of model to work on
c
      integer modindex
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
cE
      double precision x, ptop, pdiff, xprev
      integer i, j
c 
      if (verb_subaction) print *,'ENTER mod_follow(',modindex,')'
c 
      if (verb_subaction) print *,'NOTICE (mod_follow): ',
     &  'make section parameters follow their lower neighbours ',
     &  'in model no. ',modindex
c 
c take first section too just to check inconsistencies
      do j=1,glqm_nsec
        x=(mdepth(j-1,modindex)-mdepth(j,modindex))*0.5d0
        if (j.gt.2) then
          xprev=(mdepth(j-1,modindex)-mdepth(j-2,modindex))*0.5d0
        else
          xprev=(mdepth(j-1,modindex))*0.5d0
        endif
        do i=1,glqm_mpar
          if (glqm_follow(j,i)) then
c            print *,'DEBUG: x, xprev: ',x,xprev
            if (j.eq.1) then
              if (verb_allwarn) print *,'WARNING (mod_follow): ',
     &          'topmost section - no section to follow'
            else
c value at bottom of previous layer
              ptop=model(1, j-1, i, modindex)
              if (glqm_npol(j-1, i).gt.1)
     &          ptop=ptop+xprev*model(2,j-1,i,modindex)
              if (glqm_npol(j-1, i).gt.2)
     &          ptop=ptop+xprev*xprev*model(3,j-1,i,modindex)
c difference to value at middle of this layer
              pdiff=0.d0
              if (glqm_npol(j, i).gt.1)
     &          pdiff=pdiff+x*model(2,j,i,modindex)
              if (glqm_npol(j, i).gt.2)
     &          pdiff=pdiff+x*x*model(3,j,i,modindex)
c              print *,'DEBUG: ptop, pdiff',ptop,pdiff
c adjust coefficient 1 - act on mean value
              model(1, j, i, modindex)=ptop-pdiff
              if (verb_substrategy) print *,'NOTICE (mod_follow): ',
     &          'parameter ',i,' in section ',j,':',
     &          ' adjust expansion coefficient 1 to ',
     &          model(1, j, i, modindex)
            endif
          endif
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE mod_follow'
c 
      return
      end
c
c ----- END OF mod_follow.f -----
