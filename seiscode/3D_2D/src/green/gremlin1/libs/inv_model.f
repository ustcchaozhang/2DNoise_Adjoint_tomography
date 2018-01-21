c this is <inv_model.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c calculate model estimation for stabilization factor nu
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
c    08/04/98   V1.1   changed to use correct real system of linear equations
c    17/04/98   V1.2   normalize stabilization factor
c 
      logical function inv_model(nu)
c
c 20/01/98: now use weights as search ranges
c 08/04/98: changed to use correct real system of linear equations
c
      real nu
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
c 
cE
      integer im, jm, info, nrhs, ldb
      character*1 uplo
      logical result
      real normnu
c 
      if (verb_subaction) print *,'ENTER inv_model(',nu,')'
c
      if (verb_subaction) print *,
     &  'NOTICE (inv_model): calculate model estimation for ',nu
      result=.true.
c calculate dssd+nuww
      do im=1,mod_n
        do jm=1,mod_n
c          lq_dssd_nuww(im, jm)=lq_dssd(im, jm)
          lq_dssd_nuww(im, jm)=real(lq_dssd(im, jm))
c          print *,'DEBUG: lq_dssd(',im,',',jm,')=',lq_dssd(im,jm)
        enddo
      enddo
c normalize stabilization factor
      normnu=nu/float(mod_n)
      do im=1,mod_n
        lq_dssd_nuww(im, im)=lq_dssd_nuww(im, im)+normnu/
     &    (mweight(im)*mweight(im))
c        print *,'DEBUG: lq_dssd_nuww(',im,',',im,')=',lq_dssd_nuww(im,im)
c        lq_mestim(im)=lq_dssdelta(im)
        lq_mestim(im)=real(lq_dssdelta(im))
c        print *,'DEBUG: lq_dssdelta(',im,')=',lq_dssdelta(im)
      enddo
c 
c call lapack routine
      uplo='U'
      nrhs=1
      ldb=glqm_mano
c IMSL
c      call erset(0,1,-1)
c      call dlsadh(mod_n, lq_dssd_nuww, glqm_mano, lq_dssdelta, lq_mestim)
c      info=0
c LAPACK
      call dposv(uplo, mod_n, nrhs, lq_dssd_nuww, glqm_mano,
     &           lq_mestim, ldb, info)
c      call zposv(uplo, mod_n, nrhs, lq_dssd_nuww, glqm_mano,
c     &           lq_mestim, ldb, info)
c      call cposv(uplo, mod_n, nrhs, lq_dssd_nuww, glqm_mano,
c     &           lq_mestim, ldb, info)
      if (info.lt.0) then
        if (verb_allwarn) print *,
     &    'ERROR (inv_model): zposv reported illegal arg ',-info
        result=.false.
      elseif (info.gt.0) then
        if (verb_allwarn) print *,
     &          'ERROR (inv_model): zposv reported minor order ',info,
     &          'is not positive definit'
        result=.false.
      else
c 
c copy model estimate
        do im=1,mod_n
          mdelta(im)=lq_mestim(im)
c          mdelta(im)=real(lq_mestim(im))
c          if ((imag(lq_mestim(im))/mdelta(im)).gt.1.e-10)
c     &      print *,'WARNING (inv_model): ',
c     &      'where does this imaginary part come from ',
c     &      imag(lq_mestim(im))/mdelta(im)
        enddo
      endif
c 
      inv_model=result
c 
      if (verb_subaction) print *,'LEAVE inv_model (result=',result,')'
c 
      return
      end
c
c ----- END OF inv_model.f -----
