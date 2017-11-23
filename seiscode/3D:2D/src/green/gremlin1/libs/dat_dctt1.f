c this is <dat_dctt1.f>
c this was <dat_dctt.f>
c------------------------------------------------------------------------------
cS
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
c calculate synthetic travel time data from my own discrete model
c the model information is taken from the remembered parent
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    02/12/98   V1.1   there was a confusion between mi and mi_alpha
c                      mi has not to be used with glqm_follow and glqm_npol
c    03/12/98   V1.2   added correct handling of low-velocity channels
c    13/01/99   V1.3   just the old version to be kept in library
c
      subroutine dat_dctt1(ref)
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
      integer i, j, data_index, nlay, ilay, id, isec, mi, npol
c some intermediate results
      double precision v(glqm_mlay), ti(glqm_mlay), d(glqm_mlay)
      double precision dref, z, x, zo, a(glqm_mpol)
c 
      double precision vmax
c 
      if (verb_subaction) print *,'ENTER dat_dctt1(',ref,')'
c 
      if (src_depth.gt.0.d0)
     & stop 'ERROR (dat_ctt): cannot handle subsurface source!'
c 
      if (verb_subaction) print *,
     &   'NOTICE (dat_dctt1): calculate travel time synthetics',
     &   ' from model ',glqm_parent
      if ((ref).and.(verb_subaction)) print *,
     &   'NOTICE (dat_dctt1): they will be used as reference'
c 
      if (ref) then
        data_index=di_mref
      else
        data_index=di_mcalc
      endif
      mi=glqm_parent
c
c build model
      nlay=max(glqm_mpol*glqm_nsec,min(glqm_mlay-glqm_nsec, chop_finett))
      dref=1.05d0*mdepth(glqm_nsec, mi)/(nlay-glqm_nsec)
c 
      id=0
      isec=1
      npol=glqm_npol(isec, mi_alpha)
      if (glqm_follow(isec, mi_alpha)) npol=min(glqm_mpol, npol+1)
      do i=1,glqm_mpol
        if (i.le.npol) then
          a(i)=model(i, isec, mi_alpha, mi)
        else
          a(i)=0.d0
        endif
      enddo
      ilay=0
      z=0.d0
      do while(ilay.lt.(nlay-1))
        ilay=ilay+1
        id=id+1
        zo=z
        z=id*dref
        if (isec.lt.glqm_nsec) then
          if (z.gt.mdepth(isec+1, mi)) then
            d(ilay)=mdepth(isec+1, mi)-zo
            x=zo+d(ilay)/2.d0-mdepth(isec, mi)
            v(ilay)=a(1)+a(2)*x+a(3)*x*x
c 
            isec=isec+1
            npol=glqm_npol(isec, mi_alpha)
            if (glqm_follow(isec, mi_alpha)) npol=min(glqm_mpol, npol+1)
            if (verb_substrategy) print *,'NOTICE (dat_dctt1): ',
     &        'enter new section ',isec,' with layer from ',
     &         mdepth(isec, mi),' to ',z,' first used layer ',ilay
            do i=1,glqm_mpol
              if (i.le.npol) then
                a(i)=model(i, isec, mi_alpha, mi)
              else
                a(i)=0.d0
              endif
            enddo
c 
            ilay=ilay+1
            d(ilay)=z-mdepth(isec, mi)
            x=d(ilay)*0.5d0
            v(ilay)=a(1)+a(2)*x+a(3)*x*x
          else
            d(ilay)=dref
            x=z-dref*0.5d0-mdepth(isec, mi)
            v(ilay)=a(1)+a(2)*x+a(3)*x*x
          endif
        else
          d(ilay)=dref
          x=z-dref*0.5d0-mdepth(isec, mi)
          v(ilay)=a(1)+a(2)*x+a(3)*x*x
        endif
      enddo
      nlay=ilay
      if ((isec.lt.glqm_nsec).and.(verb_allwarn))
     &  print *,'WARNING (dat_dctt1): ',
     &    'discrete model does not touch halfspace ',
     &    'reaches only to section ',isec
      if (verb_substrategy) print *,'NOTICE (dat_dctt1): ',
     &  'filled ',nlay,' layers down to ',z,' with v=',v(nlay)
c 
c work with km rather than m
      do i=1,nlay
        d(i)=d(i)*1.e-3
      enddo
c 
c calculate intercept times
      ti(1)=0.d0
      vmax=0.d0
      do i=2,nlay
c check whether we are in a low-velocity channel
        if (v(i).gt.vmax) then
          ti(i)=0.d0
          do j=1,i-1
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
      do i=1,data_ntts
        travt(i, data_index)=travx(i)/v(1)+ti(1)
        do j=2,nlay
          travt(i, data_index)=min((travx(i)/v(j)+ti(j)),
     &      travt(i, data_index))
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE dat_dctt1'
c 
      return
      end
c
c ----- END OF dat_dctt.f -----
c
c ----- END OF dat_dctt1.f -----
