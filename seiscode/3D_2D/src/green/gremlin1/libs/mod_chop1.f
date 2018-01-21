c this is <mod_chop1.f>
c this was <mod_chop.f>
c------------------------------------------------------------------------------
cS
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
c build homogeneus layers from polynomial expansion
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    15/04/98   V1.1   introduced different master modes
c    14/01/99   V1.2   just keep old version as model definition changed
c
      subroutine mod_chop1(i)
c
c reduced algorithm for model chopping
c
c i: index of model to chop
c 
c 20/01/98: we expect to find slowness values and reciprocal Q-values
c in the polynomial model
c V2.1: return to velocity and Q
c 
c 15/04/98: we introduced master modes - that means that layer positions
c are not necessarily determined by all parameters
c
      integer i
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
cE
      integer sec, par, lay, npol
      double precision sdepth, tdepth, bdepth
      double precision det, a1, a2, a3, x0, xb, xs1, xs2, p0, ps, pb, quo
      double precision xe, d0, db, pm
      logical hot, trapped
c 
c Ensure that sections with polynomial expansions of order two
c are subdivided into at least 2 layers. This is necessary to apply
c constraints to higher order expansion coefficients. For the same
c reason section with polynomial orders of three have to be subdivided
c into at least 3 layers. We control this by two variables:
      integer ncut, mincut
c 
c mastermode count
      integer masterlimit
c
      if (verb_subaction) print *,'ENTER mod_chop1(',i,')'
c
      if (verb_subaction) print *,
     &  'NOTICE (mod_chop1): create discrete named model from ',i
c 
      if (verb_debug) call mod_write(6, i, 'DEBUG: (mod_chop1)')
c 
c set masterlimit
      masterlimit=glqm_mpar
      if ((chop_master.gt.0).and.(chop_master.lt.glqm_mpar)) 
     &  masterlimit=chop_master
c      print *,'DEBUG: masterlimit: ',masterlimit
c set top halfspace
      glqm_nlay=1
      dmodel(1, mi_depth)=0.
      dmodel(1, mi_alpha)=0.3318
      dmodel(1, mi_beta)=0.0001
      dmodel(1, mi_density)=0.0013
      dmodel(1, mi_Qalpha)=1000.
      dmodel(1, mi_Qbeta)=100.
c 
      glqm_parent=i
c
c start stack with first section
      dmodel(2, mi_depth)=mdepth(1, i)
      glqm_nlay=2
c 
c scan sections
      do sec=1,glqm_nsec
c how many layers for this section at least?
        mincut=glqm_npol(sec, 1)
c check all parameters even if masterlimit is set
        do par=1,glqm_mpar
c if we are in follow mode there is one more coefficient to use
          npol=glqm_npol(sec,par)
          if ((glqm_follow(sec,par)).and.(sec.lt.glqm_nsec)) npol=min(3, npol+1)
          mincut=max(mincut, npol)
        enddo
c start this sections cut counter
        ncut=1
c section reference depth: 
        sdepth=mdepth(sec, i)
c top of search range:
        tdepth=sdepth
        if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &    'section ',sec,' to be cut at least into ',mincut,' layers '
        if (glqm_nlay.lt.glqm_mlay) hot=.true.
        do while (hot)
          if (sec.eq.glqm_nsec) then
            bdepth=chop_hs
          else
            bdepth=mdepth(sec+1, i)
          endif
          if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &      'reference depth:',sdepth,
     &      ' top of range:',tdepth,
     &      ' bottom of range:',bdepth
c 
c look for maximum relative step between tdepth and bdepth for each
c parameter
          trapped=.false.
          x0=tdepth-sdepth
c check only parameters up to masterlimit
          do par=1,masterlimit
c if we are in follow mode there is one more coefficient to use
            npol=glqm_npol(sec,par)
            if ((glqm_follow(sec,par)).and.(sec.lt.glqm_nsec)) 
     &        npol=min(3, npol+1)
c there will be implications for expansions of order 1 or 2
            if (npol.eq.2) then
c work on order 1 expansion (linear)
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
              xb=bdepth-sdepth
              p0=a1+a2*x0
              pb=a1+a2*xb
              quo=abs((pb-p0)/p0)
c check whether there are implications on bounds (to be aware of
c dividing by small a2 values
              if (quo.gt.chop_step) then
c check for step up or down
                if (pb.gt.p0) then
                  ps=p0*(1.+chop_step)
                else
                  ps=p0*(1.-chop_step)
                endif
                xs1=sdepth+(ps-a1)/a2
                if ((xs1.gt.tdepth).and.(xs1.lt.bdepth)) then
                  trapped=.true.
                  if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &              'trapped by parameter ',par,' at ',xs1,
     &              ' a1, a2: ',a1,a2
                  bdepth=xs1
                else
                  print *,'ERROR (mod_chop1): ',
     &              'not trapped - why?'
                  print *,'INFO (mod_chop1): ',
     &              'we were solving the linear equation for expansion'
                  print *,'INFO (mod_chop1): ',
     &              'of order 1 in section ',sec,' for parameter ',par,
     &              'in model ',i
                  print *,'INFO (mod_chop1): ',
     &              'top of search range: ',tdepth,
     &              'bottom of search range: ',bdepth,
     &              'calculated trap depth: ',xs1
                  print *,'INFO (mod_chop1): ',
     &              'parameter at top of search range: ',p0,
     &              'parameter at bottom of search range: ',pb,
     &              'parameter value to find: ',ps
                  call mod_panic('ERROR (mod_chop1): not trapped - why?')
                endif
              endif
            elseif (npol.eq.3) then
c work on order 2 expansion
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
              a3=model(3, sec, par, i)
              xb=bdepth-sdepth
              d0=a2+2.d0*a3*x0
              db=a2+2.d0*a3*xb
c check for extrema within range to avoid division by small a3
              if ((db*d0).lt.0.) then
                xe=sdepth-a2/(2.d0*a3)
                if ((xe.lt.tdepth).or.(xe.gt.bdepth)) 
     &            call mod_panic('ERROR (mod_chop1): something wrong with xe')
c limit bottom depth to xe
                if (xe.eq.tdepth) then
                  if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &              'oops... found extreme value for parameter ',par,
     &              ' on top of search range - ignore that',
     &              ' (d0, db: ',d0,db,')'
                else
                  bdepth=xe
                  trapped=.true.
                  if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &              'trapped by extreme value of parameter ',par,' at ',xe,
     &              ' a1, a2, a3: ',a1,a2,a3
                endif
              endif
              xb=bdepth-sdepth
              p0=a1+a2*x0+a3*x0*x0
              pb=a1+a2*xb+a3*xb*xb
              quo=abs((pb-p0)/p0)
c check whether there are implications on bounds (to be aware of
c dividing by small a3 values
              if (quo.gt.chop_step) then
c check for step up or down
                if (pb.gt.p0) then
                  ps=p0*(1.+chop_step)
                else
                  ps=p0*(1.-chop_step)
                endif
c in case of very small a3 a linear approximation might be more stable
                if (abs(a3).lt.(abs(a2)*1.d-15)) then
                  xs1=sdepth+(ps-p0)*(xb-x0)/(pb-p0)+x0
                  if ((xs1.gt.tdepth).and.(xs1.lt.bdepth)) then
                    trapped=.true.
                    if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &                'trapped by linear approx. of parameter ',par,' at ',xs1,
     &                ' a1, a2, a3: ',a1,a2,a3
                    bdepth=xs1
                  else
                    call mod_panic(
     &                'ERROR (mod_chop1): missed linearized trap - why?')
                  endif
                else
c ok - solve the quadratic equation
                  det=a2*a2-4.*a3*(a1-ps)
                  if (det.lt.0.) then
                      print *,'ERROR (mod_chop1): ',
     &                  'oahh - check order 2 code!'
                      print *,'INFO (mod_chop1): ',
     &                  'we were solving the quadratic equation for expansion'
                      print *,'INFO (mod_chop1): ',
     &                  'of order 2 in section ',sec,' for parameter ',par,
     &                  ' in model ',i
                      print *,'INFO (mod_chop1): ',
     &                  'det=',det
                      print *,'INFO (mod_chop1): ',
     &                  'top of search range: ',tdepth,
     &                  ' bottom of search range: ',bdepth
                      print *,'INFO (mod_chop1): ',
     &                  'parameter at top of search range: ',p0,
     &                  ' parameter at bottom of search range: ',pb,
     &                  ' parameter value to find: ',ps
                    call mod_panic(
     &                'ERROR (mod_chop1): oahh - check order 2 code!')
                  endif
                  xs1=sdepth-(a2+sqrt(det))/(2.*a3)
                  xs2=sdepth-(a2-sqrt(det))/(2.*a3)
                  if ((xs1.gt.tdepth).and.(xs2.gt.tdepth)) then
                    xs1=min(xs1,xs2)
                  elseif (xs2.gt.tdepth) then
                    xs1=xs2
                  endif
                  if ((tdepth.lt.xs1).and.(bdepth.gt.xs1)) then
                    trapped=.true.
                    if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &                'trapped by quadratic parameter ',par,' at ',xs1,
     &                ' a1, a2, a3: ',a1,a2,a3
                    bdepth=xs1
                  else
                    if ((tdepth.eq.xs1).or.(bdepth.eq.xs1)) then
                      call mod_panic('ERROR (mod_chop1): on bound')
                    else
c give more info as program will terminate here
                      print *,'ERROR (mod_chop1): ',
     &                  'we missed the trap value'
                      print *,'INFO (mod_chop1): ',
     &                  'we were solving the quadratic equation for expansion'
                      print *,'INFO (mod_chop1): ',
     &                  'of order 2 in section ',sec,' for parameter ',par,
     &                  ' in model ',i
                      print *,'INFO (mod_chop1): ',
     &                  'top of search range: ',tdepth,
     &                  ' bottom of search range: ',bdepth,
     &                  ' calculated trap depth: ',xs1,
     &                  ' alt. calculated trap depth: ',xs2
                      print *,'INFO (mod_chop1): ',
     &                  'parameter at top of search range: ',p0,
     &                  ' parameter at bottom of search range: ',pb,
     &                  ' parameter value to find: ',ps
                      call mod_panic('ERROR (mod_chop1): missed trap value')
                    endif
                  endif
                endif
              endif
            endif
          enddo
c end of parameter loop
c check whether we should continue with this section
          hot=trapped
          glqm_nlay=glqm_nlay+1
          if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &      'another layer nlay: ',glqm_nlay,' at ',bdepth
          if (trapped) ncut=ncut+1
          dmodel(glqm_nlay, mi_depth)=bdepth
c          print *,'DEBUG: lay hot sec tdepth bdepth ',
c     &      glqm_nlay,hot,sec,tdepth,bdepth
c next serach range starts at bottom of last layer
          tdepth=bdepth
          if (glqm_nlay.eq.glqm_mlay) then
            hot=.false.
            if ((trapped).and.(verb_allwarn))
     &        print *,'WARNING (mod_chop1): ',
     &                'discrete model does not reach hs-depth'
            if ((sec.lt.glqm_nsec).and.(verb_allwarn))
     &        print *,'WARNING (mod_chop1): ',
     &                'discrete model does not contain all sections'
          endif
        enddo 
c end of while(hot) loop
c here is the point to check the cut counter
        do while (ncut.lt.mincut)
          ncut=ncut+1
          if (verb_substrategy) print *,'NOTICE (mod_chop1): ',
     &      'another extra layer nlay: ',glqm_nlay+1
c are there enough layer elements remaining?
          if (glqm_nlay.eq.glqm_mlay) then
            if (verb_allwarn) then
              print *,'WARNING (mod_chop1): can not build enough layers for'
              print *,'         section ',sec,' to ensure constraints on higher'
              print *,'         expansion coefficients of order ',ncut
              if (sec.lt.glqm_nsec) 
     &          print *,'WARNING (mod_chop1): ',
     &                'discrete model will not contain all sections'
            endif
          else
c ok - get another layer in between the last two ones
            glqm_nlay=glqm_nlay+1
c            print *,'DEBUG (mod_chop1): another extra layer sec ',sec,
c     &              ' ncut ',ncut,' nlay ',glqm_nlay
            dmodel(glqm_nlay, mi_depth)=dmodel(glqm_nlay-1, mi_depth)
            dmodel(glqm_nlay-1, mi_depth)=
     &        (dmodel(glqm_nlay-2, mi_depth)+dmodel(glqm_nlay, mi_depth))/2.
            do par=glqm_nlay-2,glqm_nlay
c              print *,'DEBUG (mod_chop1): lay depth ',par, dmodel(par,mi_depth)
            enddo
          endif
        enddo
c end of ncut-check loop
      enddo
c end of section loop
c 
c all interface depths are fixed now
c go and determine parameter values
      sec=1
      do lay=2,glqm_nlay-1
c set correct section index
        if (sec.lt.glqm_nsec) then
          do while ((dmodel(lay, mi_depth).ge.mdepth(sec+1, i)).and.
     &              (sec.lt.glqm_nsec))
            sec=sec+1
          enddo
        endif
        sdepth=mdepth(sec,i)
        x0=dmodel(lay, mi_depth)-sdepth
        xb=dmodel(lay+1, mi_depth)-sdepth
c calculate mean values for each parameter within layer
        do par=1,glqm_mpar
c if we are in follow mode there is one more coefficient to use
          npol=glqm_npol(sec,par)
          if ((glqm_follow(sec,par)).and.(sec.lt.glqm_nsec)) 
     &      npol=min(3, npol+1)
          if (npol.eq.2) then
            a1=model(1, sec, par, i)
            a2=model(2, sec, par, i)
            pm=a1+0.5*a2*(xb+x0)
          elseif (npol.eq.3) then
            a1=model(1, sec, par, i)
            a2=model(2, sec, par, i)
            a3=model(3, sec, par, i)
c this look strange but is correct to calculate the mean parameter value
c between x0 and xb (checked again 13/01/99)
            pm=a1+0.5*a2*(xb+x0)+a3*(xb*xb+x0*x0+x0*xb)/3.
          else
            pm=model(1, sec, par, i)
          endif
c take reciprocal values for velocities and Q
c V2.1: return to velocity and Q
c          if (par.eq.mi_density) then
            dmodel(lay, par)=pm
c          else
c            dmodel(lay, par)=1./pm
c          endif
        enddo
      enddo
c 
c constant parameter values for halfspace
      lay=glqm_nlay
      if (sec.lt.glqm_nsec) then
        do while ((dmodel(lay, mi_depth).ge.mdepth(sec+1, i)).and.
     &              (sec.lt.glqm_nsec))
          sec=sec+1
        enddo
      endif
      sdepth=mdepth(sec,i)
      x0=dmodel(lay, mi_depth)-sdepth
      do par=1,glqm_mpar
c if we are in follow mode there is one more coefficient to use
        npol=glqm_npol(sec,par)
        if ((glqm_follow(sec,par)).and.(sec.lt.glqm_nsec)) npol=min(3, npol+1)
        if (npol.eq.2) then
          a1=model(1, sec, par, i)
          a2=model(2, sec, par, i)
          dmodel(lay, par)=a1+a2*x0
        elseif (npol.eq.3) then
          a1=model(1, sec, par, i)
          a2=model(2, sec, par, i)
          a3=model(3, sec, par, i)
          dmodel(lay, par)=a1+a2*x0+a3*x0*x0
        else
          a1=model(1, sec, par, i)
          dmodel(lay, par)=a1
        endif
c reciprocal values for velocities and Q
c V2.1: return to velocity and Q
c        if (par.ne.mi_density) then
c          dmodel(lay, par)=1./dmodel(lay, par)
c        endif
      enddo
c 
c depth dimension should be km
      do lay=1,glqm_nlay
        dmodel(lay, mi_depth)=dmodel(lay, mi_depth)/1000.
      enddo
c
      if (verb_subaction) print *,'LEAVE mod_chop1'
c 
      return
      end
c
c ----- END OF mod_chop.f -----
c
c ----- END OF mod_chop1.f -----
