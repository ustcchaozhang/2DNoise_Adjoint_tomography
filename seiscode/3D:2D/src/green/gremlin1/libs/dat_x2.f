c this is <dat_x2.f>
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
c 
cS
c----------------------------------------------------------------------
c
      real function dat_X2(ref)
c
c calculate value of error functional
c (synthetics will be taken as they are - no calculation)
c
c ref=.true.:   data should be taken from reference data array
c
      logical ref
c 
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
cE
c 
      real result
      double complex diff
      double precision sum, rdiff
      integer islo, ifre, i, data_index
c 
      if (verb_subaction) print *,'ENTER dat_X2(',ref,')'
c 
      if (ref) then
        data_index=di_mref
        if (verb_subaction) print *,
     &    'NOTICE (dat_X2): calculate misfit of reference data'
      else
        data_index=di_mcalc
        if (verb_subaction) print *,
     &    'NOTICE (dat_X2): calculate misfit of synthetic data'
      endif
c 
      result=0.
      sum=0.d0
c 
c calculate green misfit
      do islo=rng_smin,rng_smax
        do ifre=rng_fmin,rng_fmax
          diff=green(islo, ifre, di_mread)-green(islo, ifre, data_index)
          sum=sum+(real(diff)**2+imag(diff)**2)*gweight(islo, ifre)
        enddo
      enddo
c 
      if (verb_subresult) print *,
     &   'NOTICE (dat_X2): misfit for green ',data_index,' is ',sum
c 
c calculate travel time misfit
      do i=1,rng_xmax
        rdiff=travt(i, di_mread)-travt(i, data_index)
        sum=sum+rdiff**2*tweight(i)
      enddo
c 
      if (verb_subresult) print *,
     &   'NOTICE (dat_X2): total misfit for ',data_index,' is ',sum
c 
      result=sum
      dat_X2=result
c 
      if (verb_subaction) print *,'LEAVE dat_X2 (result=',result,')'
c 
      return
      end
c
c ----- END OF dat_x2.f -----
