c this is <mod_parcor.f>
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
c    14/01/99   V2.0   - model definition changed (see glq_model.inc)
c                      - changes of parameter for polynomial coefficient
c                        number 3 will be done in a way that does not
c                        affect the mean value within a section
c                      - check follow flag
c    22/01/99   V2.1   - changes section thickness rather than
c                        section depth - this prevents interfaces to
c                        overrun each other
c    11/04/00   V2.2   call mod_track
c
      subroutine mod_parcor
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
      double precision sectop, secbot
c      real d
c 
      if (verb_subaction) print *,'ENTER mod_parcor'
c
      if (verb_subaction) print *,
     &   'NOTICE (mod_parcor): applying anonymous model parameters to model'
c 
      i=0
c 
c initialize depth values (there will be only changes applied later)
      do j=1,glqm_nsec
        mdepth(j, mb_work)=mdepth(j, mb_ref)
      enddo
c 
      do j=1,glqm_nsec
        if (destim(j)) then
c all depth mean now section depths
c          if (j.eq.1) then
c            print *,'WARNING (mod_parcor): top of section 1 is defined to',
c     &        ' be top free surface at depth 0! do not use this as parameter'
c          endif
          i=i+1
c there is no chop_hs anymore
c          if (mdepth(j,mb_ref).ge.chop_hs) then
c            print *,'WARNING (mod_parcor): section ',j,
c     &        ' in reference is below top of halfspace'
c            call mod_panic('ERROR (mod_parcor): that MUST NOT happen - call par_sano')
c          else
c            mdepth(j, mb_work)=mdepth(j, mb_ref)*(1.+mdelta(i))
c change thickness by increasing all section depths from this on
            do k=j,glqm_nsec
              mdepth(k, mb_work)=mdepth(k, mb_ref)+mdelta(i)
            enddo
c            mdepth(j, mb_work)=mdepth(j, mb_ref)+mdelta(i)
c          endif
c        else
c          mdepth(j, mb_work)=mdepth(j, mb_ref)
        endif
      enddo
c 
      do j=1,glqm_mpar
        sectop=0.d0
        secbot=0.d0
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
          sectop=secbot
          secbot=mdepth(k, mb_work)
          if (mestim(k, j)) then
            do l=1,glqm_npol(k, j)
              if (.not.(glqm_follow(k, j).and.(l.eq.1))) then
                i=i+1
c              model(l, k, j, mb_work)=model(l, k, j, mb_ref)+
c     &          model(1, k, j, mb_ref)*mdelta(i)/(d**(l-1))
                model(l, k, j, mb_work)=model(l, k, j, mb_ref)+mdelta(i)
c coefficient 3 should not affect mean value within section
                if (l.eq.3) model(1, k, j, mb_work)=model(1, k, j, mb_work)-
     &            (mdelta(i)*((secbot-sectop)**2)/12.d0)
              endif
            enddo
          else
            do l=1,glqm_npol(k, j)
              model(l, k, j, mb_work)=model(l, k, j, mb_ref)
            enddo
          endif
        enddo
      enddo
c 
      if (i.ne.mod_n) 
     &  call mod_panic('ERROR (mod_parcor): wrong number of anonymous parameters')
c 
c make section parameters follow
      call mod_follow(mb_work)
c
c let vp track vs
      if (vptrackfactor.gt.0.d0) call mod_track(mb_work)
c 
      if (verb_subaction) print *,'LEAVE mod_parcor'
c 
      return
      end
c
c ----- END OF mod_parcor.f -----
