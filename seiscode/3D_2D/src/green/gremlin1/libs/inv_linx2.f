c this is <inv_linx2.f>
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
      logical function inv_linX2(nu,x2value)
c
c find model estimation for given nu and return error value
c use linearized forward modeling by inv_linsynt
c 
c nu:        find model for this parameter
c x2value:   corresponding error function
c     
      real nu, x2value
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_invres.inc'
      include 'glq_verbose.inc'
c 
cE
      logical result, inv_model, mod_prep
      real dat_X2
c 
      if (verb_subaction) print *,'ENTER inv_linX2(',nu,',',x2value,')'
c
      lq_moderror=.false.
      lq_inverror=.false.
c 
      result=inv_model(nu)
      if (result) then
c we do not really need a reflectivity model here
c do it just to check the model feasibility
        result=mod_prep()
        if (result) then
          call inv_linsynt
          x2value=dat_X2(.false.)
        else
          lq_moderror=.true.
          if (verb_allwarn) print *,'WARNING (inv_linX2): ',
     &      'model failed'
        endif
      else
        lq_inverror=.true.
        if (verb_allwarn) print *,'WARNING (inv_linX2): ',
     &    'could not solve system of linear equations'
      endif
c 
      inv_linX2=result
c 
      if (verb_subaction) print *,'LEAVE inv_linX2 (result=',result,')'
c 
      return
      end
c
c ----- END OF inv_linx2.f -----
