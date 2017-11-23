c this is <inv_x2hist.f>
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
      logical function inv_X2hist(nu,x2value,iext)
c
c find model estimation for given nu and return error value
c nu and x2 will be added to history list
c 
c nu:        find model for this parameter
c x2value:   corresponding error function
c iext:      index of extreme value in history (on return)
c     
      real nu, x2value
      integer iext
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_invpara.inc'
      include 'glq_invres.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
c 
cE
      logical result, inv_X2
c 
      if (verb_subaction) print *,'ENTER inv_X2hist(',nu,',',
     &  x2value,',',iext,')'
c 
      result=inv_x2(nu, x2value)
      if (result)
     &  call inv_addtolist(lq_parimp, lq_x2imp, lq_npts, glqm_mpts, iext,
     &    nu, x2value, .true., .true.)
c 
      inv_x2hist=result
c 
      if (verb_subaction) print *,'LEAVE inv_X2hist (result=',result,')'
c 
      return
      end
c
c ----- END OF inv_x2hist.f -----
