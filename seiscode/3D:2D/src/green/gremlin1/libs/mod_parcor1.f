c this is <mod_parcor1.f>
c this was <mod_parcor.f>
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
c apply anonymous parameters to named polynomial model
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    14/04/98   V1.1   warn if changing top of first section
c    14/01/99          keep old version
c
      subroutine mod_parcor1
c
c calculated model from reference and changes
c
c 02/01/98:   no parameters allowed. 
c             reference is always mb_ref 
c             results will be in mb_work
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
c all anonymous values are relative to the named reference
c 
c all anonymous parameters will mean realtive changes of the absolute
c value of the named parameter at the bottom of each section or the
c top of the halfspace (for deepest section). This is meant relative
c to the absolute value at the top of the section.
c this applies to named parameters of all polynomial orders.
c
c sections below the top of the bottom halfspace will be ignored.
c 
c 19/01/98: all parameter changes are done in absolute values
c
cE
c
      integer i,j,k,l
c      real d
c 
      if (verb_subaction) print *,'ENTER mod_parcor1'
c
      if (verb_subaction) print *,
     &   'NOTICE (mod_parcor1): applying anonymous model parameters to model'
c 
      i=0
c 
      do j=1,glqm_nsec
        if (destim(j)) then
          if (j.eq.1) then
            print *,'WARNING (mod_parcor1): top of section 1 is defined to',
     &        ' be top free surface at depth 0! do not use this as parameter'
          endif
          i=i+1
          if (mdepth(j,mb_ref).ge.chop_hs) then
            print *,'WARNING (mod_parcor1): section ',j,
     &        ' in reference is below top of halfspace'
            call mod_panic('ERROR (mod_parcor1): that MUST NOT happen - call par_sano')
          else
c            mdepth(j, mb_work)=mdepth(j, mb_ref)*(1.+mdelta(i))
            mdepth(j, mb_work)=mdepth(j, mb_ref)+mdelta(i)
          endif
        else
          mdepth(j, mb_work)=mdepth(j, mb_ref)
        endif
      enddo
c 
      do j=1,glqm_mpar
        do k=1,glqm_nsec
c          if (k.eq.glqm_nsec) then
c            d=chop_hs-mdepth(k, mb_ref)
c          else
c            if (mdepth(k+1,mb_ref).ge.chop_hs) then
c              d=chop_hs-mdepth(k, mb_ref)
c            else
c              d=mdepth(k+1, mb_ref)-mdepth(k, mb_ref)
c            endif
c          endif
          do l=1,glqm_npol(k, j)
            if (mestim(k, j)) then
              i=i+1
c              model(l, k, j, mb_work)=model(l, k, j, mb_ref)+
c     &          model(1, k, j, mb_ref)*mdelta(i)/(d**(l-1))
              model(l, k, j, mb_work)=model(l, k, j, mb_ref)+mdelta(i)
            else
              model(l, k, j, mb_work)=model(l, k, j, mb_ref)
            endif
          enddo
        enddo
      enddo
c 
      if (i.ne.mod_n) 
     &  call mod_panic('ERROR (mod_parcor1): wrong number of anonymous parameters')
c 
c make section parameters follow
      call mod_follow(mb_work)
c 
      if (verb_subaction) print *,'LEAVE mod_parcor1'
c 
      return
      end
c
c ----- END OF mod_parcor.f -----
c
c ----- END OF mod_parcor1.f -----
