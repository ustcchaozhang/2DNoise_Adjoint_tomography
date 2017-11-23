c this is <dat_mscprep.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c prepare mseisfk offset correction
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
cS
c ============================================================================
c
      subroutine dat_mscprep(onoff)
c
c onoff: .true.:  switch correction on
c        .false.: switch correction off
c
c This subroutine calculates the greens coefficients for a water halfspace.
c They have to be removed from the mseisfk solution.
c
      logical onoff
c
cE
      include 'glq_dim.inc'
      include 'glq_mseiscorr.inc'
      include 'glq_data.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
      double complex thisgreen
      integer islo,ifre
c
      msc_apply=onoff
c
      if (msc_apply) then
        if (verb_subaction) 
     &    print *,'ENTER (dat_mscprep): ',
     &            'mseisfk offset correction is ACTIVE'
c
      if (verb_subaction) print *,
     &   'NOTICE (dat_mscprep): calculate pure water green ONCE!'
c 
c set the model
c we are taking only the first two layers (top layer is top halfspace)
      call gr_dsetmod(dmodel(1, mi_depth), dmodel(1, mi_alpha),
     &               dmodel(1, mi_beta), dmodel(1, mi_density),
     &               dmodel(1, mi_Qalpha), dmodel(1, mi_Qbeta),
     &               src_type, src_depth, src_amp, 2)
c 
c calculate coefficients for all read data
      do islo=1,data_nslo
        if (islo.eq.1) then
          do ifre=1,data_nslo
            msc_green(islo,ifre)=(0.d0,0.d0)
          enddo
        endif
        call gr_prep(dble(dat_slo(islo)))
        do ifre=1,data_nslo
c          print *,islo,ifre,dat_slo(islo),dat_om(ifre)
          if (ifre.eq.1) then
            thisgreen=(0.d0,0.d0)
          else
            call gr_green(dble(dat_om(ifre)), thisgreen)
          endif
          msc_green(islo, ifre)=thisgreen
        enddo
      enddo
c
        if (verb_subaction) 
     &    print *,'LEAVE (dat_mscprep): ',
     &            'correction greens values are prepared'
      else
        if (verb_subaction) 
     &    print *,'NOTICE (dat_mscprep): ',
     &            'mseisfk offset correction is INACTIVE'
      endif
c
      return
      end
c
c ----- END OF dat_mscprep.f ----- 
