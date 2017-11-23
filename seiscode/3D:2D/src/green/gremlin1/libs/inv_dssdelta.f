c this is <inv_dssdelta.f>
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
      subroutine inv_dssdelta
c
c calculate D°*S°*S*(real-synt_ref)
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
      double complex sum
c 
      if (verb_subaction) print *,'ENTER inv_dssdelta'
c 
      if (verb_subaction) print *,
     &  'NOTICE (inv_dssdelta): calculate D°*S°*S*(real-synt_ref)'
c 
      do im=1,mod_n
        sum=0.d0
        id=0
        do islo=rng_smin,rng_smax
          do ifre=rng_fmin,rng_fmax
            id=id+1
            sum=sum+lq_dss(id, im)*
     &          (green(islo, ifre, di_mread)-green(islo, ifre, di_mref))
          enddo
        enddo
        do itt=1,rng_xmax
          id=id+1
          sum=sum+lq_dss(id, im)*
     &        (travt(itt, di_mread)-travt(itt, di_mref))
        enddo
        lq_dssdelta(im)=sum
      enddo
c 
      if (id.ne.dat_n) 
     &  call mod_panic('ERROR (inv_dssdelta): wrong number of data points')
c 
      if (verb_subaction) print *,'LEAVE inv_dssdelta'
c 
      return
      end
c
c ----- END OF inv_dssdelta.f -----
