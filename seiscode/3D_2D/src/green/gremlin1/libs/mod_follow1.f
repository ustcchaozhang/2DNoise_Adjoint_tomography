c this is <mod_follow1.f>
c this was <mod_follow.f>
c------------------------------------------------------------------------------
cS
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
c make a section parameter follow its lower neighbour
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    13/01/99          just keep old version after change of model definition
c                      (see glq_model.inc)
c
      subroutine mod_follow1(modindex)
c
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
      double precision d, a1, a2, p2
      integer i, j
c 
      if (verb_subaction) print *,'ENTER mod_follow1(',modindex,')'
c 
      if (verb_subaction) print *,'NOTICE (mod_follow1): ',
     &  'make section parameters follow their lower neighbours ',
     &  'in model no. ',modindex
c 
      do i=1,glqm_mpar
        do j=1,glqm_nsec
          if (glqm_follow(j,i)) then
            if (j.eq.glqm_nsec) then
              if (verb_allwarn) print *,'WARNING (mod_follow1): ',
     &          'lowermost section - no section to follow for parameter ',i
            else
              d=mdepth(j+1, modindex)-mdepth(j, modindex)
              p2=model(1, j+1, i, modindex)
              a1=model(1, j, i, modindex)
c              print *,'DEBUG: p2,d,a1 ',p2,d,a1
              if (glqm_npol(j,i).eq.1) then
                model(2, j, i, modindex)=(p2-a1)/d
                if (verb_substrategy) print *,'NOTICE (mod_follow1): ',
     &            'parameter ',i,' in section ',j,':',
     &            ' adjust expansion coefficient 2 to ',
     &            model(2, j, i, modindex)
              elseif (glqm_npol(j,i).eq.2) then
                a2=model(2, j, i, modindex)
                model(3, j, i, modindex)=(p2-a1-a2*d)/(d*d)
                if (verb_substrategy) print *,'NOTICE (mod_follow1): ',
     &            'parameter ',i,' in section ',j,':',
     &            ' adjust expansion coefficient 3 to ',
     &            model(3, j, i, modindex)
              else
                if (verb_allwarn) print *,'WARNING (mod_follow1): ',
     &            'no free parameter to adjust for parameter ',i,
     &            ' in section ',j
              endif
            endif
          endif
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE mod_follow1'
c 
      return
      end
c
c ----- END OF mod_follow.f -----
c
c ----- END OF mod_follow1.f -----
