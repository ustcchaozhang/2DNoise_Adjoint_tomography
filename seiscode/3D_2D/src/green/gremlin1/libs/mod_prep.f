c this is <mod_prep.f>
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
      logical function mod_prep()
c
c prepare model for synthetic calculation
c
c a return value of .false. will indicate that the model defined
c by the reference model in combination with the anonymous parameters
c is infeasible in any sense.
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      logical result, mod_pcheck, mod_check
c 
      if (verb_subaction) print *,'ENTER mod_prep'
c 
      if (verb_subaction) print *,
     &  'NOTICE (mod_prep): prepare discrete named model from reference'
c
      result=.true.
c 
      call mod_parcor
      result=mod_pcheck(mb_work)
      if (result) then
        call mod_chop(mb_work)
        result=mod_check()
      else
        if (verb_allwarn) print *,'WARNING: (mod_prep): ',
     &    'model failed polynomial parameter check'
      endif
c 
      if ((verb_allwarn).and.(.not.(result))) print *,'WARNING: (mod_prep): ',
     &  'model failed parameter check'
c
      mod_prep=result
c 
      if (verb_subaction) print *,'LEAVE mod_prep (result=',result,')'
c 
      return
      end
c
c ----- END OF mod_prep.f -----
