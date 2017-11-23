c this is <pg_min.f>
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
      logical function pg_min(limit)
c
c tries to minimize the error function and returns in case of error
c or when the error reaches limit
c
c the return value will indicate success or failure
c
      real limit
c 
      include 'glq_dim.inc'
      include 'glq_verbose.inc'
c
cE
      integer mm, nnstep, nnnu, iparnu, dparnu, iparstart, niter
      parameter(mm=50)
      real xstep(mm), stepx2(mm), nu(mm), x2(mm)
      real thisnu, dat_X2, thisx2, refx2, origx2, startx2
      logical result, hot, inv_mat, notfound, inv_model, inv_X2
      logical improve
      real numax, numin, implimit
      parameter (numax=1.e6, numin=1.e-10, implimit=1.e-5)
c 
      thisnu(iparnu)=(10.**(-0.1*iparnu))
c 
      if (verb_subaction) print *,'ENTER pg_min(',limit,')'
c 
      result=.true.
      improve=.true.
c start procedure
      call dat_dmode
      result=inv_mat()
      origx2=dat_X2(.true.)
      nnstep=0
      niter=0
      call pgs_addval(nnstep, mm, xstep, stepx2, float(niter), 0.)
      if (result) then
c display reference
        call pgpage
        call mod_chop(mb_ref)
        call pg_mod(mb_ref)
        call pg_green(5, di_mref)
        call pg_tt(4, .true.)
      else
        if (verb_allwarn)
     &    print *,'WARNING (pg_min): error during derivative preparation'
      endif
c 
      iparstart=0
      hot=.true.
      do while ((hot).and.(result).and.(improve))
        if (verb_medstrategy) print *,'NEXT SEARCH'
        nnnu=0
        thisx2=dat_X2(.true.)
c pole position
        notfound=.true.
        dparnu=1
c look for nu to start with
        iparnu=iparstart
c        result=inv_X2(thisnu(iparnu), refx2)
        do while ((notfound).and.(result))
c          if (result) then
c            iparnu=iparstart+1
c            result=inv_X2(thisnu(iparnu), thisx2)
c          endif
c          if (result) then
c            dparnu=1
c            if (refx2.lt.thisx2) dparnu=-1
c          endif
          result=inv_X2(thisnu(iparnu), thisx2)
          iparnu=iparnu+dparnu
          if (result) result=inv_X2(thisnu(iparnu), refx2)
          if (refx2.le.thisx2) then
            notfound=.false.
          else
            iparnu=iparnu-10
          endif
        enddo
c serach
        notfound=.true.
        startx2=thisx2
        call pgs_addval(nnnu, mm, nu, x2, float(iparnu), 
     &    (startx2-thisx2))
        if (verb_medstrategy) print *,'FIND MINIMUM'
        do while ((notfound).and.(result))
          refx2=thisx2
          iparnu=iparnu+dparnu
          if ((thisnu(iparnu).lt.numin).or.(thisnu(iparnu).gt.numax)) then
            result=.false.
            if (verb_allwarn) print *,'WARNING: nu touched bounds: min nu max',
     &        numin, thisnu(iparnu), numax
          endif
          if (result) result=inv_X2(thisnu(iparnu), thisx2)
          call pgs_addval(nnnu, mm, nu, x2, float(iparnu), 
     &      (startx2-thisx2))
          if ((thisx2.gt.refx2).and.(result)) then
            notfound=.false.
            iparnu=iparnu-dparnu
            iparstart=max(0,iparnu-10)
            if (refx2.lt.limit) hot=.false.
          endif
        enddo
        niter=niter+1
        call pgs_addval(nnstep, mm, xstep, stepx2, float(niter), 
     &    (origx2-refx2))
        if (((stepx2(nnstep)-stepx2(nnstep-1))/
     &    (origx2-limit)).lt.implimit) then
          improve=.false.
          if (verb_medstrategy) print *,
     &      'NOTICE (pg_min): model does not seem to improve anymore'
        endif
c calculate new reference
        if (result) result=inv_model(thisnu(iparnu))
        if (result) then
          call mod_parcor
          call mod_copy(mb_work, mb_ref)
          result=inv_mat()
        endif
        if (result) then
          if (verb_medstrategy) print *,
     &      'NOTICE (pg_min): found better estimate'
          call pgpage
          call pgs_curve(6, nnstep, xstep, stepx2, 'iteration', 
     &      '\gx\u2\d\dorig\u-\gx\u2')
          call pgs_curve(7, nnnu, nu, x2, 'parameter', 
     &      '\gx\u2\d\dstart\u-\gx\u2')
        endif
      enddo
c 
      if (verb_medstrategy) then
        if (result) then
          if (improve) then
            print *,'NOTICE (pg_min): found a best fitting model below ',limit
          else
            print *,'NOTICE (pg_min): model did not seem to improve anymore'
          endif
        else
          print *,'WARNING (pg_min): there occured an error - ',
     &          'anyway there might be an improved model'
        endif
      endif
c
      pg_min=result
c 
      if (verb_subaction) print *,'LEAVE pg_min (result=',result,')'
c 
      return
      end
c
c ----- END OF pg_min.f -----
