c this is <inv_dss.f>
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
c    08/04/98   V1.1   removed ugly bug! routine was just calculating product
c                      but not the sum... (oh dear!)
c
c==============================================================================
c
cS
c----------------------------------------------------------------------
c 
      subroutine inv_dss
c 
c calculate D°*S°*S 
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
c 
cE
      integer im, id, islo, ifre, itt
c 
      if (verb_subaction) print *,'ENTER inv_dss'
c 
      if (verb_subaction) print *,'NOTICE (inv_dss): calculate D°*S°*S'
c 
      id=0
      do islo=rng_smin,rng_smax
        do ifre=rng_fmin,rng_fmax
          id=id+1
          do im=1,mod_n
            lq_dss(id, im)=conjg(lq_d(id,im))*gweight(islo,ifre)
c            print *,'DEBUG: islo ifre id im ',lq_dss(id,im)
          enddo
        enddo
      enddo
      do itt=1,rng_xmax
        id=id+1
        do im=1,mod_n
          lq_dss(id, im)=conjg(lq_d(id,im))*tweight(itt)
        enddo
      enddo
c 
      if (id.ne.dat_n) 
     &  call mod_panic('ERROR (inv_dss): wrong number of data points')
c 
      if (verb_subaction) print *,'LEAVE inv_dss'
c 
      return
      end
c
c ----- END OF inv_dss.f -----
