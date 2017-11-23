c this is <mod_check.f>
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
      logical function mod_check()
c
c check chopped model
c
c all parameters will be checked for the discrete model
c be more verbose
c 20/01/98: polynomial model values (and feasibility ranges) are
c given as reciprocal values for velocities and Q
c V2.1: return to velocity and Q
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
cE
      integer lay, par
c      real rangemin(glqm_mpar), rangemax(glqm_mpar)
      logical result
c
      if (verb_subaction) print *,'ENTER mod_check'
c 
c      do par=1,glqm_mpar
c        if (par.eq.mi_density) then
c          rangemin(par)=rng_mmin(par)
c          rangemax(par)=rng_mmax(par)
c        else
c          rangemin(par)=1./rng_mmax(par)
c          rangemax(par)=1./rng_mmin(par)
c        endif
c      enddo
c
      result=.true.
      lay=2
      if (verb_subresult) then
        do while ((result).and.(lay.le.glqm_nlay))
          do par=1,glqm_mpar
            if (dmodel(lay, par).lt.rng_mmin(par)) then
              result=.false.
              print *,'NOTICE (mod_check): layer ',lay,' parameter ',
     &          par,' at depth ',dmodel(lay,mi_depth),' failed range ',
     &          rng_mmin(par),'<=par'
              print *,'NOTICE (mod_check): parameter is ',dmodel(lay,par)
            endif
            if (dmodel(lay, par).gt.rng_mmax(par)) then
              result=.false.
              print *,'NOTICE (mod_check): layer ',lay,' parameter ',
     &          par,' at depth ',dmodel(lay,mi_depth),' failed range ',
     &          rng_mmax(par),'>=par'
              print *,'NOTICE (mod_check): parameter is ',dmodel(lay,par)
            endif
          enddo
          if (dmodel(lay, mi_alpha).lt.dmodel(lay, mi_beta)) then
            result=.false.
            print *,'NOTICE (mod_check): layer ',lay,
     &        ' at depth ',dmodel(lay,mi_depth),
     &        ' Vs exceeds Vp value'
            print *,'NOTICE (mod_check): Vp is ',dmodel(lay,mi_alpha)
            print *,'NOTICE (mod_check): Vs is ',dmodel(lay,mi_beta)
          endif
          if (dmodel(lay, mi_depth).lt.dmodel(lay-1,mi_depth)) then
            result=.false.
            print *,'NOTICE (mod_check): layer ',lay,
     &        ' at depth ',dmodel(lay,mi_depth),
     &        ' is above layer ',lay-1,' with ',dmodel(lay-1, mi_depth)
          endif
          lay=lay+1
        enddo
      else
        do while ((result).and.(lay.le.glqm_nlay))
          do par=1,glqm_mpar
            if (dmodel(lay, par).lt.rng_mmin(par)) result=.false.
            if (dmodel(lay, par).gt.rng_mmax(par)) result=.false.
          enddo
          if (dmodel(lay, mi_alpha).lt.dmodel(lay, mi_beta)) result=.false.
          if (dmodel(lay, mi_depth).lt.dmodel(lay-1,mi_depth)) result=.false.
          lay=lay+1
        enddo
      endif
c 
      mod_check=result
c 
      if ((.not.(result)).and.(verb_allwarn)) print *,
     &  'NOTICE (mod_check): discrete named model failed feasibility check'
c
      if (verb_subaction) print *,'LEAVE mod_check (result=',result,')'
c 
      return
      end
c
c ----- END OF mod_check.f -----
