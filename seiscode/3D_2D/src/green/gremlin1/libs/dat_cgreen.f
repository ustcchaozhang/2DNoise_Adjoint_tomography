cS       
c this is <dat_cgreen.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c calculate a pure reflectivity green within the bounds
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
c the current discrete model will be taken
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    04/07/02   V1.1   remove offset from mseisfk coefficients if requested
c
c==============================================================================
c 
      subroutine dat_cgreen
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
      include 'glq_mseiscorr.inc'
c
cE
      double complex thisgreen
      integer islo, ifre
c 
      if (verb_subaction) print *,'ENTER dat_cgreen'
c 
      if (verb_subaction) print *,
     &   'NOTICE (dat_cgreen): calculate pure reflectivity green'
c 
c set the model
      call gr_dsetmod(dmodel(1, mi_depth), dmodel(1, mi_alpha),
     &               dmodel(1, mi_beta), dmodel(1, mi_density),
     &               dmodel(1, mi_Qalpha), dmodel(1, mi_Qbeta),
     &               src_type, src_depth, src_amp, glqm_nlay)
c 
      do islo=rng_smin,rng_smax
        call gr_prep(dble(dat_slo(islo)))
        do ifre=rng_fmin,rng_fmax
           call gr_green(dble(dat_om(ifre)), thisgreen)
           green(islo, ifre, di_mcalc)=thisgreen
        enddo
      enddo
c 
c correct mseisfk coefficients if requested
      if (msc_apply) call dat_mscorr
c 
      if (verb_subaction) print *,'LEAVE dat_cgreen'
c 
      return
      end
c
c ----- END OF dat_cgreen.f -----
