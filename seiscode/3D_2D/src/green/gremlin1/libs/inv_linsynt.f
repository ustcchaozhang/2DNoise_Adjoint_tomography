c this is <inv_linsynt.f>
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
      subroutine inv_linsynt
c
c calculate new model by 
c
c   d = d^0 + D * mdelta
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
c 
cE
      integer id, im, itt, islo ,ifre
c 
      if (verb_subaction) print *,'ENTER inv_linsynt'
c
      if (verb_subaction) print *,'NOTICE (inv_linsynt): ',
     &  'calculate linearized forward functional'
c 
      id=0
      do islo=rng_smin,rng_smax
        do ifre=rng_fmin,rng_fmax
          id=id+1
          green(islo, ifre, di_mcalc)=green(islo, ifre, di_mref)
          do im=1,mod_n
            green(islo, ifre, di_mcalc)=green(islo, ifre, di_mcalc)+
     &        lq_d(id, im)*mdelta(im)
          enddo
        enddo
      enddo
      do itt=1,rng_xmax
        id=id+1
        travt(itt, di_mcalc)=travt(itt, di_mref)
        do im=1,mod_n
          travt(itt, di_mcalc)=travt(itt, di_mcalc)+
     &      lq_d(id, im)*mdelta(im)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE inv_linsynt'
c
      return
      end
c
c ----- END OF inv_linsynt.f -----
