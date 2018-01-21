c this is <dat_ctt.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c calculate synthetic travel time data from discrete model
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
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    03/12/98   V1.1   added correct handling of low velocity channels
c    04/03/99   V1.2   we now allow to use a hard top section (think of
c                      asphalt) to be handled in a different way. For this
c                      section only the direct wave will be calculated. To
c                      lower section it will only contribute with a constant
c                      time offset.
c
      subroutine dat_ctt(ref)
c
c
c ref=.true.:   data should be stored in reference data array
c
      logical ref
c 
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
      include 'glq_para.inc'
c
cE
      integer i, j, data_index
c 
c first layer below asphalt, and index of first full traveltime
      integer firstlay, firstfull
c travel time through asphalt
      double precision asptime
c 
c some intermediate results
      double precision d(glqm_mlay), ti(glqm_mlay)
c to check for LVZ 
      double precision vmax
c 
      if (verb_subaction) print *,'ENTER dat_ctt(',ref,')'
c 
      if (src_depth.gt.0.d0) 
     & print *,'ERROR (dat_ctt): cannot handle subsurface source!'
c 
      if (verb_subaction) print *,
     &   'NOTICE (dat_ctt): calculate travel time synthetics'
      if ((ref).and.(verb_substrategy)) print *,
     &   'NOTICE (dat_ctt): they will be used as reference'
c 
      if (ref) then
        data_index=di_mref
      else
        data_index=di_mcalc
      endif
c 
      if (verb_debug) then
        print *,'DEBUG (dat_ctt):'
        do i=1,glqm_nlay
          print *,i,dmodel(i,mi_depth),dmodel(i,mi_alpha)
        enddo
      endif
c
c check array dimansion
      if (glqm_nlay.lt.3) stop 
     &  'ERROR (gin_ctt): at least one layer between halfspaces'
c 
c calculate layer thickness
      do i=2,glqm_nlay-1
        d(i)=dmodel(i+1, mi_depth)-dmodel(i, mi_depth)
      enddo
c 
c asphalt section:
c check whether we use the hard top option and calculate first set of
c travel times
      if (data_ttsplit.gt.0) then
c calculate tarvel times in the asphalt layer
        if (verb_substrategy) print *,'STARTEGY (dat_ctt): ',
     &    'consider asphalt'
        do i=1,data_ttsplit
c use layer index 2, as layer one is free air
          travt(i, data_index)=travx(i)/dmodel(2, mi_alpha)
        enddo
c        print *,'DEBUG: entered asphalt section'
c        print *,'DEBUG: glqm_lasec: ',glqm_lasas
        asptime=0.d0
        do i=2,glqm_lasas
          asptime=asptime+d(i)/dmodel(i,mi_alpha)
        enddo
c        print *,'DEBUG: asptime: ',asptime
        if (verb_subresult) 
     &    print *,'RESULT (dat_ctt): traveltime through asphalt layer: ',
     &    asptime
        firstfull=data_ttsplit+1
        firstlay=glqm_lasas+1
      else
c ok, no asphalt found
        firstfull=1
        firstlay=2
        asptime=0.d0
      endif
c 
c calculate intercept times
      if (verb_debug) print *,'DEBUG (dat_ctt): calculate intercept times'
      ti(1)=0.
      ti(2)=0.
      ti(firstlay)=0.d0
      vmax=0.
      do i=firstlay+1,glqm_nlay
        if (verb_debug) print *,'DEBUG (dat_ctt): ',i
c check whether we are in a low-velocity layer
        if (dmodel(i,mi_alpha).gt.vmax) then
          ti(i)=0.
          do j=firstlay,i-1
            if (verb_debug) print *,'DEBUG (dat_ctt): ',
     &        d(j),dmodel(j, mi_alpha),dmodel(i, mi_alpha)
            ti(i)=ti(i)+((d(j)/dmodel(j, mi_alpha))*
     &        sqrt(1.-(dmodel(j, mi_alpha)/dmodel(i, mi_alpha))**2))
            if (verb_debug) print *,'DEBUG (dat_ctt): ',i,j,ti(i)
          enddo
          ti(i)=2.*ti(i)
        else
c there is no true intercept time for this layer - just keep that of
c the above layer - that's no problem as the virtual head-wave is slower
          ti(i)=ti(i-1)
        endif
        vmax=max(vmax,dmodel(i, mi_alpha))
      enddo
c 
c calculate travel times
      if (verb_debug) print *,'DEBUG (dat_ctt): calculate travel times'
      do i=firstfull,data_ntts
        travt(i, data_index)=travx(i)/dmodel(1, mi_alpha)+ti(firstlay)
        do j=firstlay,glqm_nlay
          travt(i, data_index)=min((travx(i)/dmodel(j, mi_alpha)+ti(j)),
     &      travt(i, data_index))
        enddo
        travt(i, data_index)=travt(i, data_index)+asptime
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_ctt'
c 
      return
      end
c
c ----- END OF dat_ctt.f -----
