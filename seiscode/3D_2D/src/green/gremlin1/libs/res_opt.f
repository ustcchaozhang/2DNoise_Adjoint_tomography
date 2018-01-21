c this is <res_opt.f>
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
c check the resolution of one model parameter by changing the
c parameter and optimizing with the rest keeping the total rms
c error at 1
c
c REVISIONS and CHANGES
c    08/04/98   V1.0   Thomas Forbriger
c    17/04/98   V1.1   normalize stabilization factor
c    02/06/00   V2.0   introduced mweightcondition
c                      changed algorithm completely
c               V2.1   take real part of lq_dssd everywhere!
c    19/04/02   V3.0   reduced to core calculations
c                      ATTENTION: There was a severe error in the
c                      error-calculation loop. The index counter jmm was not
c                      initialized correctly. Results must have been wrong.
c                      This was corrected 19/04/2002 with version 4.13 of
c                      gremlin.
c    03/05/02   V3.1   use external parselect
c    06/05/02   V3.2   wrong calculation of deltanofiterror
c
c==============================================================================
cS 
c 
      logical function res_opt(mindex, nu, deltapar, deltaerror, 
     &                         deltamisfit, deltanofiterror)
c
c rate the total resolution for a model parameter by changing this
c parameter and optimizing with the rest of free parameters keeping
c the resulting relative rms error increase at finalrms
c
c lq_dssd should be ready calculated (using inv_part, inv_dss and inv_dssd)
c
c input:
c   mindex:   index of model parameter to check
c   nu:       stabilization factor
c
c output:
c   deltapar:           variation of parameter mindex
c   deltaerror:         change of error square sum due to change of parameter
c                       mindex and optimization of all other parameters
c   deltamisfit:        change of misfit due to (no damping term) du to change
c                       of parameter mindex and optimization of all other 
c                       parameters
c   deltanofiterror:    change of error square sum only due to change of
c                       parameter mindex (no check for trade-off)
c
      integer mindex
      real nu, deltapar, deltaerror, deltamisfit, deltanofiterror
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
      include 'glq_reso.inc'
c 
cE
      integer im,jm,imm,jmm,nmm, info, nrhs, ldb
      character*1 uplo
      logical result
      real normnu, errstep, penaltystep
c 
      if (verb_subaction) print *,'ENTER res_opt(',mindex,',',nu,')'
c
      result=.true.
c find selected parameter
c
c we set up a reduced system of linear equations. all parameters not matching
c the condition below will be omitted (although they are selected for
c inversion). This is necessary due to the fact, that only full sections (all
c polynomial orders) may be activated for inversion. If we wanr to check and
c optimize only for the mean value, we have to exclude the other orders.
      call res_parselect
c normalize stabilization factor
      normnu=nu/float(mod_n)
c calculate dssd+nuww and right hand side
c
c this sets up the system of linear equations
c
c   sum_(n != K) R_ln dm_n = - R_lK Dm_K
c
c where dm_n are the parameter optimizations, Dm_K is a specified change in
c the tested parameter (K=mindex), which is one search range here. And
c
c   R_ln = Re( M_ln )
c
c where
c
c   M = D°*S°*S*D + normnu * W°*W
c
c where D are the partial derivatives, S is the matrix of data tolerance
c weights and W is the matrix of reziprocal search ranges.
      imm=0
      do im=1,mod_n
        if ((parselect(im)).and.(im.ne.mindex)) then
          imm=imm+1
          jmm=0
          do jm=1,mod_n
            if ((parselect(jm)).and.(jm.ne.mindex)) then
              jmm=jmm+1
              if (im.eq.jm) then
                lq_dssd_nuww(imm, jmm)=real(lq_dssd(im, jm))+
     &            normnu/(mweight(im)*mweight(im))
c calculate "right hand side"
                lq_mestim(imm)=-real(lq_dssd(im, mindex))*mweight(mindex)
              else
                lq_dssd_nuww(imm, jmm)=real(lq_dssd(im, jm))
              endif
            endif
          enddo
        endif
      enddo
c we changed the test-paremeter (mindex) by one search range:
      deltapar=mweight(mindex)
c remember reduced system size
      nmm=imm
c 
c call lapack routine
      uplo='U'
      nrhs=1
      ldb=glqm_mano
c IMSL
c      call erset(0,1,-1)
c      call dlsads(mod_n, lq_dssd_nuww, glqm_mano, mdelta, lq_mestim)
c      info=0
c      do im=1,mod_n
c        lq_mestim(im)=mdelta(im)
c      enddo
c LAPACK
c      call dgesv(mod_n, nrhs, lq_dssd_nuww, glqm_mano, ipiv,
c     &           lq_mestim, ldb, info)
      call dposv(uplo, nmm, nrhs, lq_dssd_nuww, glqm_mano,
     &           lq_mestim, ldb, info)
c      call cposv(uplo, mod_n, nrhs, lq_dssd_nuww, glqm_mano,
c     &           lq_mestim, ldb, info)
      if (info.lt.0) then
        if (verb_allwarn) print *,
     &    'ERROR (res_opt): dposv reported illegal arg ',-info
        result=.false.
      elseif (info.gt.0) then
        if (verb_allwarn) print *,
     &          'ERROR (res_opt): dposv reported minor order ',info,
     &          'is not positive definit'
        result=.false.
      else
        result=.true.
c
c calculate linear rms error estimate for that model parameter change
c order of IF/ELSEIF matters!
c a parameter may be mindex and not be selected - however even if mindex is
c selected it will not be contained in the reduced model vector lq_mestim
c
        deltaerror=0.
        deltamisfit=0.
        deltanofiterror=0.
        imm=0
        do im=1,mod_n
          jmm=0
          if (im.eq.mindex) then
            do jm=1,mod_n
              if (jm.eq.mindex) then
                errstep=mweight(im)*mweight(jm)*real(lq_dssd(im,jm))
                penaltystep=normnu
                deltaerror=deltaerror+errstep+penaltystep
                deltanofiterror=deltanofiterror+errstep+penaltystep
                deltamisfit=deltamisfit+errstep
              elseif (parselect(jm)) then
                jmm=jmm+1
                errstep=mweight(im)*lq_mestim(jmm)*real(lq_dssd(im,jm))
                deltaerror=deltaerror+errstep
                deltamisfit=deltamisfit+errstep
              endif
            enddo
          elseif (parselect(im)) then
            imm=imm+1
            do jm=1,mod_n
              if (jm.eq.mindex) then
                errstep=lq_mestim(imm)*mweight(mindex)*real(lq_dssd(im,jm))
                deltaerror=deltaerror+errstep
                deltamisfit=deltamisfit+errstep
              elseif (parselect(jm)) then
                jmm=jmm+1
                errstep=lq_mestim(imm)*lq_mestim(jmm)*
     &                  real(lq_dssd(im,jm))
                if (jmm.eq.imm) then
                  penaltystep=normnu*lq_mestim(imm)**2/(mweight(im)**2)
                else
                  penaltystep=0.
                endif
                deltaerror=deltaerror+errstep+penaltystep
                deltamisfit=deltamisfit+errstep
              endif
            enddo
          endif
        enddo
      endif
c 
      res_opt=result
c 
      if (verb_subaction) print *,'LEAVE res_opt (result=',result,')'
c 
      return
      end
c
c ----- END OF res_opt.f -----
