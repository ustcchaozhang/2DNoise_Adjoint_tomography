c this is <res_optrms.f>
cS
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
c rms-version of res_opt
c
c REVISIONS and CHANGES
c    16/04/2002   V1.0   Thomas Forbriger
c    03/05/2002   V1.1   use external parselect
c
c ============================================================================
c
      logical function res_optrms(mindex, nu, finalrms, x2ref)
c
      integer mindex
      real nu, finalrms, x2ref
c
c input:
c   nu:           damping weight factor
c   finalrms:     the relative increase in rms-error allowed
c                 (relative to the misfit of the reference synthetics)
c   x2ref:        the misfit for the reference synthetics
c
c output:
c   on output the mdelta array in the glq_model common block contains
c   appropriate changes for the test results
c 
c   return .true. if executed correctly
c
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
      include 'glq_reso.inc'
c 
c local variables
      real deltapar, deltaerror, deltamisfit, deltanofiterror
      real rmserror, nofiterror, modelfactor
      integer im,imm,msec,mpar,mpol
      logical res_opt
      character*20 parname
      logical result
c 
cE
c
      if (verb_subaction) print *,'ENTER res_optrms(',mindex,',',nu,
     &   ',',finalrms,')'
c
      if (verb_subaction) print *,
     &  'NOTICE (res_optrms): rate resolution for parameter ',mindex,
     &  ' and nu ',nu,' and final rms ',finalrms
c calculate raw test
      if (res_opt(mindex, nu, deltapar, deltaerror, 
     &                         deltamisfit, deltanofiterror)) then
c 
        result=.true.
c
        rmserror=sqrt(deltaerror)
        nofiterror=sqrt(deltanofiterror)
        modelfactor=sqrt((finalrms+1.)**2-1.)*sqrt(x2ref)/rmserror
c
c expand solution vector
        imm=0
        do im=1,mod_n
          if (im.eq.mindex) then
            mdelta(im)=deltapar*modelfactor
          else
            if (parselect(im)) then
              imm=imm+1
              mdelta(im)=lq_mestim(imm)*modelfactor
            else
              mdelta(im)=0.d0
            endif
          endif
        enddo
c 
c print result
        call mod_identify(mindex, msec, mpol, mpar, parname)
        print *,'RESULTS (res_optrms):'
        print *,'  testing model parameter ',mindex
        print 50,mpol-1,parname(1:index(parname,' ')),msec
        print 51,deltapar,sqrt(1.+(nofiterror**2)/x2ref)-1.
        print 52,sqrt(1.+(rmserror**2)/x2ref)-1.
        print 53,finalrms
        do im=1,mod_n
          call mod_identify(im, msec, mpol, mpar, parname)
          print 54,im,mpol-1,parname,msec,mdelta(im)
        enddo
      else
        result=.false.
      endif
c 
      res_optrms=result
c 
      if (verb_subaction) print *,'LEAVE res_optrms (result=',result,')'
c 
      return
   50 format(3x,'that is ord. ',i1,' of ',a,' in sec. ',i2)
   51 format(3x,'changing just this by ',g9.2,
     &  ' leads to relative rms-error increase of ',g12.5)
   52 format(3x,' optimizing others in addition ',
     &  ' leads to relative rms-error increase of ',g12.5)
   53 format(3x,'parameter changes to be applied',
     &  ' for a relative rms-error increase of   ',g12.5,':')
   54 format(5x,i2,': ord. ',i1,' of ',a14,' in sec. ',i2,
     &  ' to be changed by ',g12.5)
      end
c
c ----- END OF res_optrms.f ----- 
