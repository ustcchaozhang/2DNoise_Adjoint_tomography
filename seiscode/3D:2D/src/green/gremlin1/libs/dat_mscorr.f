c this is <dat_mscorr.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c apply mseisfk offset correction
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
c    04/07/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine dat_mscorr
c
c calculate a pure reflectivity green within the bounds
c the current discrete model will be taken
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
      if (verb_subaction) print *,'ENTER dat_mscorr'
c 
      if (verb_subaction) print *,
     &   'NOTICE (dat_mscorr): remove offset'
c 
      do islo=rng_smin,rng_smax
        call gr_prep(dble(dat_slo(islo)))
        do ifre=rng_fmin,rng_fmax
           green(islo, ifre, di_mcalc)=
     &       green(islo, ifre, di_mcalc)-msc_green(islo,ifre)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_mscorr'
c 
      return
      end
c
c ----- END OF dat_mscorr.f ----- 
