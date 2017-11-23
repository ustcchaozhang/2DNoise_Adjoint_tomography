c this is <inv_part.f>
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
cS
c
      logical function inv_part()
c 
c calculate matrix of partial derivatives
c 
c reference model is expected to be in mb_ref
c reference synthetics are expected to be in di_mref
c 
c 19/01/98: partial derivatives will be calculated relative to the 
c search range
c 20/01/98: old weights become now real search ranges
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
c 
cE
      integer im, id, i, islo, ifre, itt
      logical mod_prep, result
c 
      if (verb_subaction) print *,'ENTER inv_part'
c 
      result=.true.
c 
      if (verb_subaction)
     &  print *,'NOTICE (inv_part): calculate partial derivatives for ',mod_n,
     &  'anonymous parameters'
c go through all anonymous parameters
      do im=1,mod_n
        if (verb_medstrategy) 
     &     print *,'NOTICE (inv_part): ',
     &       'calculate partial derivative for parameter ',im
        if (result) then
c set up anonymous parameters
          do i=1,mod_n
            mdelta(i)=0.d0
          enddo
          mdelta(im)=pvar_pdev*mweight(im)
c calculate synthetics with mdelta variation
          result=mod_prep()
          if (result) then
            call dat_synt(.false.)
c calculate partial derivatives
            id=0
            do islo=rng_smin,rng_smax
              do ifre=rng_fmin,rng_fmax
                id=id+1
                lq_d(id, im)=(green(islo, ifre, di_mcalc)-
     &                       green(islo,ifre,di_mref))/
     &                       mdelta(im)
              enddo
            enddo
            do itt=1,rng_xmax
              id=id+1
              lq_d(id, im)=(travt(itt, di_mcalc)-travt(itt, di_mref))/
     &                     mdelta(im)
            enddo
          else
            if (verb_allwarn)
     &        print *,'ERROR (inv_part): illegal model for parameter ',im
          endif
        endif
      enddo
c 
      if (result) then
        dat_n=id
        if (verb_subresult)
     &    print *,'NOTICE (inv_part): partial derivatives calculated for ',
     &    dat_n, 'data points'
      else
        dat_n=0
        if (verb_allwarn) print *,
     &    'WARNING (inv_part): calculation of partial derivatives failed'
      endif
c 
      inv_part=result
c 
      if (verb_subaction) print *,'LEAVE inv_part (result=',result,')'
c DEBUG
c      do im=1,mod_n
c        print *,'DEBUG: part.der. for model par. ',im
c        do id=1,dat_n
c          print *,'DEBUG: data value ',id,lq_d(id,im)
c        enddo
c      enddo
c 
      if (mod_n.lt.1) print *,'WARNING (inv_part): ',
     &  'no model parameters are active'
      return
      end
c
c ----- END OF inv_part.f -----
