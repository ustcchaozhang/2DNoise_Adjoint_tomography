c this is <dat_dctt.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c calculate synthetic travel time data from my own discrete model
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
c the model information is taken from the remembered parent
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    02/12/98   V1.1   there was a confusion between mi and mi_alpha
c                      mi has not to be used with glqm_follow and glqm_npol
c    03/12/98   V1.2   added correct handling of low-velocity channels
c    13/01/99   V2.0   changed meaning of model coefficients (see
c                      comments in glq_model.inc for details) and follow
c                      flag
c    04/03/99   V2.1   handle top asphalt section
c
      subroutine dat_dctt(ref)
c
c calculate synthetic travel time data from my own discrete model
c the model information is taken from the remembered parent
c
c ref=.true.:   data should be stored in reference data array
c
      logical ref
c 
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
c 
c last layer in asphalt, and index of first full traveltime
      integer ilasas, firstfull
c index of first layer below asphal
      integer firstlay
c travel time through asphalt
      double precision asptime
c 
      integer i, j, data_index, nlay, ilay, id, isec, mi, npol
c some intermediate results
      double precision v(glqm_mlay), ti(glqm_mlay), d(glqm_mlay)
      double precision dref, z, zo, a(glqm_mpol)
      double precision sectop, secbot, zref, xt, xb
c 
      double precision vmax
c 
      if (verb_subaction) print *,'ENTER dat_dctt(',ref,')'
c 
      if (src_depth.gt.0.d0)
     & print *, 'ERROR (dat_ctt): cannot handle subsurface source!'
c 
      if (verb_subaction) print *,
     &   'NOTICE (dat_dctt): calculate travel time synthetics',
     &   ' from model ',glqm_parent
      if ((ref).and.(verb_subaction)) print *,
     &   'NOTICE (dat_dctt): they will be used as reference'
c 
      if (ref) then
        data_index=di_mref
      else
        data_index=di_mcalc
      endif
      mi=glqm_parent
c 
c the whole thing is organized in two sections:
c 1. we build a new fine sampled model
c 2. we calculate travel times
c
c 1. build model
c ==============
c
c   figure out required number of discrete leyers
      nlay=min(glqm_mlay,max(glqm_mpol*glqm_nsec, chop_finett))
c   calculate mean layer thickness
      dref=1.05d0*mdepth(glqm_nsec, mi)/(nlay-glqm_nsec)
c 
c initialize track-keepers
      id=0
      isec=1
c initialize first set of coefficients (top section)
      npol=glqm_npol(isec, mi_alpha)
c      if (glqm_follow(isec, mi_alpha)) npol=min(glqm_mpol, npol+1)
      do i=1,glqm_mpol
        if (i.le.npol) then
          a(i)=model(i, isec, mi_alpha, mi)
        else
          a(i)=0.d0
        endif
      enddo
c some more track-keepers
      ilay=0
      sectop=0.d0
      secbot=mdepth(isec, mi)
      zref=0.5d0*(secbot+sectop)
      z=sectop
c and go...
      do while(ilay.lt.(nlay-1))
c increase layer index
        ilay=ilay+1
c increase counter of standard thickness layers within section
        id=id+1
c top of this layer
        zo=z
        xt=zo-zref
c bottom of this layer
        z=id*dref+sectop
        xb=z-zref
        if (z.gt.mdepth(isec, mi)) then
c we are just about to leave this section 
c finish section - this layer ends at bottom of section
          z=mdepth(isec, mi)
          xb=z-zref
          d(ilay)=z-zo
c looks strange but is correct
          v(ilay)=a(1)+0.d50*a(2)*(xb+xt)+a(3)*(xb*xb+xt*xt+xb*xt)/3.d0
c 
c are we still in asphalt 
          if (isec.eq.1) ilasas=ilay
c enter next section if possible
          if (isec.lt.glqm_nsec) then
            isec=isec+1
c initialize values
            npol=glqm_npol(isec, mi_alpha)
c            if (glqm_follow(isec, mi_alpha)) npol=min(glqm_mpol, npol+1)
            do i=1,glqm_mpol
              if (i.le.npol) then
                a(i)=model(i, isec, mi_alpha, mi)
              else
                a(i)=0.d0
              endif
            enddo
c step track-keepers
            sectop=secbot
            secbot=mdepth(isec, mi)
            zref=0.5d0*(secbot+sectop)
            z=sectop
            id=0
            if (verb_substrategy) print *,'NOTICE (dat_dctt): ',
     &        'enter new section ',isec,' with layer from ',
     &         sectop,' to ',secbot,' first used layer ',ilay
          else
c ok - we reached the halfspace - use value at bottom of last section
            ilay=ilay+1
            d(ilay)=dref
            v(ilay)=a(1)+a(2)*(secbot-zref)+a(3)*(secbot-zref)**2
c that's it
            nlay=ilay
          endif
        else
c ok, still in old section - add standard thickness layers
c calculate mean velocity
          d(ilay)=dref
c looks strange but is correct
          v(ilay)=a(1)+0.5d0*a(2)*(xb+xt)+a(3)*(xb*xb+xt*xt+xb*xt)/3.d0
        endif
      enddo
c 
      nlay=ilay
      if ((isec.lt.glqm_nsec).and.(verb_allwarn))
     &  print *,'WARNING (dat_dctt): ',
     &    'discrete model does not touch halfspace ',
     &    'reaches only to section ',isec,
     &    ' and maximum depth (in layer ',nlay,') is ',z,' with v=',v(nlay)
      if (verb_substrategy) print *,'NOTICE (dat_dctt): ',
     &  'filled ',nlay,' layers down to ',z,' with v=',v(nlay)
c 
c work with km rather than m
      do i=1,nlay
        d(i)=d(i)*1.e-3
      enddo
c 
c 2. calculate tarvel times
c =========================
c 
c asphalt precalculation
c check whether we use the hard top option and calculate first set of
c travel times
      if (data_ttsplit.gt.0) then
        if (verb_substrategy) print *,'STRATEGY (dat_dctt): ',
     &    'consider asphalt'
c calculate tarvel times in the asphalt layer
        do i=1,data_ttsplit
c use layer index 2, as layer one is free air
          travt(i, data_index)=travx(i)/v(1)
        enddo
c        print *,'DEBUG: entered asphalt section'
c        print *,'DEBUG: ilasas: ',ilasas
        asptime=0.d0
        do i=1,ilasas
          asptime=asptime+d(i)/v(i)
        enddo
c        print *,'DEBUG: asptime: ',asptime
        if (verb_subresult) print *,'RESULT (dat_dctt): ',
     &    'travel time in asphalt ',asptime
        firstfull=data_ttsplit+1
        firstlay=ilasas+1
c        print *,'DEBUG: vel(firstlay)',v(firstlay)
      else
c ok, no asphalt found
        firstfull=1
        firstlay=1
        asptime=0.d0
      endif
c 
c calculate intercept times
      ti(1)=0.d0
      vmax=0.d0
      do i=firstlay+1,nlay
c check whether we are in a low-velocity channel
        if (v(i).gt.vmax) then
          ti(i)=0.d0
          do j=firstlay,i-1
            ti(i)=ti(i)+((d(j)/v(j))*sqrt(1.-(v(j)/v(i))**2))
          enddo
          ti(i)=2.*ti(i)
        else
c as there is no intercept time within a low velocity-channel, we just
c take that of the above layer - that's no problem as the (non-existing)
c virtual head-wave has a lower velocity
          ti(i)=ti(i-1)
        endif
        vmax=max(vmax,v(i))
      enddo
c 
c calculate travel times
      do i=firstfull,data_ntts
        travt(i, data_index)=travx(i)/v(firstlay)+ti(firstlay)
        do j=firstlay+1,nlay
          travt(i, data_index)=min((travx(i)/v(j)+ti(j)),
     &      travt(i, data_index))
        enddo
        travt(i, data_index)=travt(i, data_index)+asptime
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_dctt'
c 
      return
      end
c
c ----- END OF dat_dctt.f -----
