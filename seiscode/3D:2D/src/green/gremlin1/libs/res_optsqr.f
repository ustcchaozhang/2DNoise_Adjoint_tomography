c this is <res_optsqr.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c variance version of res_opt
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
c    16/04/2002   V1.0   Thomas Forbriger
c    03/05/2002   V1.1   use parselect from glq_reso
c
c ============================================================================
c
      logical function res_optsqr(mindex, nu, finaldeltaerr, x2ref)
c
      integer mindex
      real nu, finaldeltaerr, x2ref
c
c input:
c   nu:                damping weight factor
c   finaldeltaerr:     the relative increase in error suqare sum allowed
c                      (relative to the misfit of the reference synthetics)
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
      real modelfactor
      integer im,imm,msec,mpar,mpol
      logical res_opt
      character*20 parname
      logical result
c 
cE
c
      if (verb_subaction) print *,'ENTER res_optsqr(',mindex,',',nu,
     &   ',',finaldeltaerr,')'
c
      if (verb_subaction) print *,
     &  'NOTICE (res_optsqr): rate resolution for parameter ',mindex,
     &  ' and nu ',nu,' and final error increase ',finaldeltaerr
c calculate raw test
      if (res_opt(mindex, nu, deltapar, deltaerror, 
     &                         deltamisfit, deltanofiterror)) then
c 
        result=.true.
c
        modelfactor=sqrt(x2ref*finaldeltaerr/deltaerror)
c
c expand solution vector
        imm=0
        do im=1,mod_n
          if (im.eq.mindex) then
            mdelta(im)=mweight(im)*modelfactor
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
        print *,'RESULTS (res_optsqr):'
        print *,'  testing model parameter ',mindex
        print 50,mpol-1,parname(1:index(parname,' ')),msec
        print 51,deltapar,deltanofiterror/x2ref
        print 52,deltaerror/x2ref
        print 55,deltamisfit/x2ref
        print 53,finaldeltaerr
        do im=1,mod_n
          call mod_identify(im, msec, mpol, mpar, parname)
          print 54,im,mpol-1,parname,msec,mdelta(im)
        enddo
      else
        result=.false.
      endif
c 
      res_optsqr=result
c 
      if (verb_subaction) print *,'LEAVE res_optsqr (result=',result,')'
c 
      return
   50 format(3x,'that is ord. ',i1,' of ',a,' in sec. ',i2)
   51 format(3x,'changing just this by ',g9.2,
     &  ' leads to relative square-error increase of ',g12.5)
   52 format(3x,' optimizing others in addition ',
     &  ' leads to relative square-error increase of ',g12.5)
   55 format(3x,' optimizing others in addition ',
     &  '       leads to relative misfit increase of ',g12.5)
   53 format(3x,'parameter changes to be applied',
     &  '    for a relative square-error increase of ',g12.5,':')
   54 format(5x,i2,': ord. ',i1,' of ',a14,' in sec. ',i2,
     &  ' to be changed by ',g12.5)
      end
c
c ----- END OF res_optsqr.f ----- 
