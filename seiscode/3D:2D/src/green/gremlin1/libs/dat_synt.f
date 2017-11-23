c this is <dat_synt.f>
c------------------------------------------------------------------------------
cS
c
c  $Id$
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
c
c calculate new set of synthetic data from a discrete model
c the green and travel times will be calculated as well
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    07/04/00   V1.1   do prefit after modify, when removing polynomial trend
c                      introduced new prefit mode
c
c==============================================================================
c
      subroutine dat_synt(ref)
c
c ref=.true.:   data should be stored in reference data array
c
      logical ref
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
cE
c dataspace to modify (result)
      integer imod
c
      if (verb_subaction) print *,'ENTER dat_synt(',ref,')'
c 
      if (verb_subaction) print *,'NOTICE (dat_synt): calculate synthetics'
      if ((ref).and.(verb_substrategy)) 
     &   print *,'NOTICE (dat_synt): they will be used as reference'
c
      if (ref) then
        imod=di_mref
      else
        imod=di_mcalc
      endif
c go for green
      call dat_cgreen
c notice: different order if datamode=3 or 4
      if ((datamode.eq.1).or.(datamode.eq.2)) then
        if (prefit_mode.eq.0) then
          call dat_fcamp
        elseif (prefit_mode.eq.1) then
          call dat_famp(di_read, di_mcalc)
        elseif (prefit_mode.eq.2) then
          call dat_fmamp(di_read, di_mcalc)
        elseif (prefit_mode.eq.3) then
          call dat_fmaxamp(di_read,di_mcalc)
        else
          call mod_panic('ERROR (dat_synt): unknown prefit mode')
        endif
        call dat_dcpc(di_mcalc, imod)
      else
        call dat_dcpc(di_mcalc, imod)
        if (prefit_mode.eq.0) then
          stop 'not implemented with dataspace index parameter: call dat_fcamp'
        elseif (prefit_mode.eq.1) then
          call dat_famp(di_mread,imod)
        elseif (prefit_mode.eq.2) then
          call dat_fmamp(di_mread,imod)
        elseif (prefit_mode.eq.3) then
          call dat_fmaxamp(di_mread,imod)
        else
          call mod_panic('ERROR (dat_synt): unknown prefit mode')
        endif
      endif
c go for travel times
      if (chop_finett.gt.0) then
        call dat_dctt(ref)
      else
        call dat_ctt(ref)
      endif
      if (dottprefit.gt.0) call dat_ftt(ref)
c
      if (verb_subaction) print *,'LEAVE dat_synt'
c 
      return
      end
c
c ----- END OF dat_synt.f -----
