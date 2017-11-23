c this is <inv_mat.f>
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
      logical function inv_mat()
c
c calculate everything from a new matrix of partial derivatives
c down to dssdelta begging with reference synthetics
c     
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_invres.inc'
      include 'glq_verbose.inc'
c 
cE
      logical result, inv_part, dat_cref
      real dat_X2
c 
      if (verb_subaction) print *,'ENTER inv_mat'
c 
      lq_moderror=.false.
c
      if (verb_medstrategy) print *,'NOTICE (inv_mat): ',
     &  'calculate reference synthetics'
      result=dat_cref()
      if (result) then
        lq_x2ref=dat_X2(.true.)
        if (verb_medstrategy) print *,'NOTICE (inv_mat): ',
     &    'calculate partial derivatives'
        result=inv_part()
      endif
c 
      if (result) then
        call inv_dss
        call inv_dssd
        call inv_dssdelta
      else
        lq_moderror=.true.
        if (verb_allwarn) print *,
     &    'ERROR (inv_mat): could not calculate partial derivatives'
      endif
c 
      inv_mat=result
c 
      if (verb_subaction) print *,'LEAVE inv_mat (result=',result,')'
c 
      return
      end
c
c ----- END OF inv_mat.f -----
