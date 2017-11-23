c this is <mod_chop.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c build homogeneus layers from polynomial expansion
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
c    15/04/98   V1.1   introduced different master modes
c    14/01/99   V2.0   model definition changed (see glq_model.inc)
c                      - reference depth is now mid of section
c                      - depth now means bottom of section
c                      - follow flag does not affect number of
c                        coefficients to be used
c                      - there is no chop_hs anymore
c    22/01/99   V2.1   - added number of layer report
c    24/01/99   V2.2   - improved stability of extreme value handling
c    04/03/99   V2.3   set glqm_lasas for asphalt handling
c    08/05/02   V2.4   as a result of resolution analysis we have to chop
c                      negative values too. adjusted the calculation of
c                      psearch for that case.
c
      subroutine mod_chop(i)
c
c reduced algorithm for model chopping
c 
c all work is done by solving polynomial equations  analytically
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
      logical hot, trapped
c some key variables:
c   top and bottom depth of section and reference depth within section
      double precision secbot, sectop, zref
c   top and bottom of present search range
      double precision tdepth, bdepth
c   polynomial coefficients
      double precision a1, a2, a3
c   coordinates relative to reference depth within section
      double precision xtop, xbot
c   parameter values at top and botttom of search range
      double precision ptop, pbot
c   values partial derivatives of parameter at top and botttom of search range
      double precision dtop, dbot
c   proposed interface depth
      double precision zint1, zint2
c   depth of parameter extreme value
      double precision zextreme
c   determinant of second order polynomial
      double precision det
c   relative parameter change from top to bottom of section
c   (to be compared with chop_step)
      double precision quo
c   parameter value proposed for next interface
      double precision psearch
c   factors for parameter to search value
      double precision finc,fdec
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
      if (verb_subaction) print *,'ENTER mod_chop(',i,')'
c
      if (verb_subaction) print *,
     &  'NOTICE (mod_chop): create discrete named model from ',i
c 
      if (verb_debug) call mod_write(6, i, 'DEBUG: (mod_chop)')
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
c bottom depth of top halfspace is
      secbot=0.d0
      dmodel(2, mi_depth)=secbot
      glqm_nlay=2
c 
c scan sections
      do sec=1,glqm_nsec
c crop marks for this section are:
        sectop=secbot
        secbot=mdepth(sec, i)
c reference depth:
        zref=0.5d0*(secbot+sectop)
c how many layers for this section at least?
        mincut=glqm_npol(sec, 1)
c check all parameters even if masterlimit is set
        do par=1,glqm_mpar
c if we are in follow mode there is one more coefficient to use
          mincut=max(mincut, glqm_npol(sec,par))
        enddo
        if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &    'section ',sec,' to be cut at least into ',mincut,' layers ',
     &    '  section top depth:',sectop,
     &    '  section bottom depth:',secbot,
     &    '  reference depth in section:',zref
c start this sections cut counter
        ncut=1
c top of search range: (will increase with each new layer)
        tdepth=sectop
c 
c here we start with the loop that does all layer cutting within the
c one section
        if (glqm_nlay.lt.glqm_mlay) hot=.true.
        do while (hot)
c initialize bottom of search range: (this is changed during search)
          bdepth=secbot
c 
c look for maximum relative step between tdepth and bdepth for each
c parameter
          trapped=.false.
c relative coordinate of top
          xtop=tdepth-zref
c check only parameters up to masterlimit
          do par=1,masterlimit
            if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &        '  check parameter:',par,
     &        '  top of search range:',tdepth,
     &        '  bottom of search range:',bdepth
c polynomial order
            npol=glqm_npol(sec,par)
c relative coordinate of bottom
            xbot=bdepth-zref
c there will be implications for expansions of order 1 or 2
            if (npol.eq.2) then
c work on order 1 expansion (linear)
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
c parameter values at top and bottom
              ptop=a1+a2*xtop
              pbot=a1+a2*xbot
              quo=abs((pbot-ptop)/ptop)
c check whether there are implications on bounds (to be aware of
c dividing by small a2 values
              if (quo.gt.chop_step) then
c check for step up or down
                if (ptop.lt.0.d0) then
                  finc=(1.-chop_step)
                  fdec=(1.+chop_step)
                else
                  finc=(1.+chop_step)
                  fdec=(1.-chop_step)
                endif
                if (pbot.gt.ptop) then
                  psearch=ptop*finc
                else
                  psearch=ptop*fdec
                endif
                zint1=zref+(psearch-a1)/a2
                if ((zint1.gt.tdepth).and.(zint1.lt.bdepth)) then
                  trapped=.true.
                  if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &              'trapped by parameter ',par,' at ',zint1,
     &              ' a1, a2: ',a1,a2
c next parameter search sould be done only down to this depth
                  bdepth=zint1
                else
                  print *,'ERROR (mod_chop): ',
     &              'not trapped - why?'
                  print *,'INFO (mod_chop): ',
     &              'we were solving the linear equation for expansion'
                  print *,'INFO (mod_chop): ',
     &              'of order 1 in section ',sec,' for parameter ',par,
     &              'in model ',i
                  print *,'INFO (mod_chop): ',
     &              'top of search range: ',tdepth,
     &              'bottom of search range: ',bdepth,
     &              'calculated trap depth: ',zint1
                  print *,'INFO (mod_chop): ',
     &              'parameter at top of search range: ',ptop,
     &              'parameter at bottom of search range: ',pbot,
     &              'parameter value to find: ',psearch
                  call mod_panic('ERROR (mod_chop): not trapped - why?')
                endif
              endif
            elseif (npol.eq.3) then
c work on order 2 expansion (quadratic)
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
              a3=model(3, sec, par, i)
c partial derivative at top and bottom
              dtop=a2+2.d0*a3*xtop
              dbot=a2+2.d0*a3*xbot
              if (abs(dtop).lt.abs(dbot)) then
                quo=dtop/dbot
              else
                quo=dbot/dtop
              endif
c check for extrema within range to avoid division by small a3
              if (quo.lt.-1.d-8) then
                zextreme=zref-a2/(2.d0*a3)
                if ((zextreme.lt.tdepth).or.(zextreme.gt.bdepth)) then
                  print *,'zextreme is ',zextreme
                  print *,'tdepth is   ',tdepth
                  print *,'bdepth is   ',bdepth
                  call mod_panic(
     &              'ERROR (mod_chop): something wrong with zextreme')
                endif
c limit bottom depth to zextreme
                if (zextreme.eq.tdepth) then
                  if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &              'oops... found extreme value for parameter ',par,
     &              ' on top of search range - ignore that',
     &              ' (dtop, dbot: ',dtop,dbot,')'
                else
                  bdepth=zextreme
                  trapped=.true.
                  if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &              'trapped by extreme value of parameter ',par,' at
     &              ',zextreme,
     &              ' a1, a2, a3: ',a1,a2,a3
                endif
              endif
c update bottom coordinate
              xbot=bdepth-zref
c parameter values
              ptop=a1+a2*xtop+a3*xtop*xtop
              pbot=a1+a2*xbot+a3*xbot*xbot
              quo=abs((pbot-ptop)/ptop)
c check whether there are implications on bounds (to be aware of
c dividing by small a3 values
              if (quo.gt.chop_step) then
c check for step up or down
                if (ptop.lt.0.d0) then
                  finc=(1.-chop_step)
                  fdec=(1.+chop_step)
                else
                  finc=(1.+chop_step)
                  fdec=(1.-chop_step)
                endif
                if (pbot.gt.ptop) then
                  psearch=ptop*finc
                else
                  psearch=ptop*fdec
                endif
c in case of very small a3 a linear approximation might be more stable
                if (abs(a3).lt.(abs(a2)*1.d-15)) then
                  zint1=zref+(psearch-ptop)*(xbot-xtop)/(pbot-ptop)+xtop
                  if ((zint1.gt.tdepth).and.(zint1.lt.bdepth)) then
                    trapped=.true.
                    if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &                'trapped by linear approx. of parameter ',par,' at '
     &                ,zint1,
     &                ' a1, a2, a3: ',a1,a2,a3
c next parameter's search should end here
                    bdepth=zint1
                  else
                    call mod_panic(
     &                'ERROR (mod_chop): missed linearized trap - why?')
                  endif
                else
c ok - solve the quadratic equation
                  det=a2*a2-4.*a3*(a1-psearch)
                  if (det.lt.0.d0) then
                      print *,'ERROR (mod_chop): ',
     &                  'oahh - check order 2 code!'
                      print *,'INFO (mod_chop): ',
     &                  'we were solving the quadratic equation for expansion'
                      print *,'INFO (mod_chop): ',
     &                  'of order 2 in section ',sec,' for parameter ',par,
     &                  ' in model ',i
                      print *,'INFO (mod_chop): ',
     &                  'det=',det
                      print *,'INFO (mod_chop): ',
     &                  'top of search range: ',tdepth,
     &                  ' bottom of search range: ',bdepth
                      print *,'INFO (mod_chop): ',
     &                  'parameter at top of search range: ',ptop,
     &                  ' parameter at bottom of search range: ',pbot,
     &                  ' parameter value to find: ',psearch
                    call mod_panic(
     &                'ERROR (mod_chop): oahh - check order 2 code!')
                  endif
                  zint1=zref-(a2+sqrt(det))/(2.*a3)
                  zint2=zref-(a2-sqrt(det))/(2.*a3)
c take upper interface depth
                  if ((zint1.gt.tdepth).and.(zint2.gt.tdepth)) then
                    zint1=min(zint1,zint2)
                  elseif (zint2.gt.tdepth) then
                    zint1=zint2
                  endif
                  if ((tdepth.lt.zint1).and.(bdepth.gt.zint1)) then
                    trapped=.true.
                    if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &                'trapped by quadratic parameter ',par,' at ',zint1,
     &                ' a1, a2, a3: ',a1,a2,a3
c next parameter's search should end here
                    bdepth=zint1
                  else
                    if ((tdepth.eq.zint1).or.(bdepth.eq.zint1)) then
                      call mod_panic('ERROR (mod_chop): on bound')
                    else
c give more info as program will terminate here
                      print *,'ERROR (mod_chop): ',
     &                  'we missed the trap value'
                      print *,'INFO (mod_chop): ',
     &                  'we were solving the quadratic equation for expansion'
                      print *,'INFO (mod_chop): ',
     &                  'of order 2 in section ',sec,' for parameter ',par,
     &                  ' in model ',i
                      print *,'INFO (mod_chop): ',
     &                  'top of search range: ',tdepth,
     &                  ' bottom of search range: ',bdepth,
     &                  ' calculated trap depth: ',zint1,
     &                  ' alt. calculated trap depth: ',zint2
                      print *,'INFO (mod_chop): ',
     &                  'parameter at top of search range: ',ptop,
     &                  ' parameter at bottom of search range: ',pbot,
     &                  ' parameter value to find: ',psearch
                      call mod_panic('ERROR (mod_chop): missed trap value')
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
          if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &      'another layer nlay: ',glqm_nlay,' at ',bdepth
          if (trapped) ncut=ncut+1
          dmodel(glqm_nlay, mi_depth)=bdepth
c          print *,'DEBUG: lay hot sec tdepth bdepth ',
c     &      glqm_nlay,hot,sec,tdepth,bdepth
c next serach range starts at bottom of last layer
          tdepth=bdepth
          if (glqm_nlay.eq.glqm_mlay) then
            hot=.false.
            if (verb_allwarn)
     &        print *,'WARNING (mod_chop): ',
     &                'discrete model does not reach lower halfspace'
            if ((sec.lt.glqm_nsec).and.(verb_allwarn))
     &        print *,'WARNING (mod_chop): ',
     &                'discrete model does not contain all sections'
          endif
        enddo 
c end of while(hot) loop
c here is the point to check the cut counter
        do while (ncut.lt.mincut)
          ncut=ncut+1
          if (verb_substrategy) print *,'NOTICE (mod_chop): ',
     &      'another extra layer nlay: ',glqm_nlay+1
c are there enough layer elements remaining?
          if (glqm_nlay.eq.glqm_mlay) then
            if (verb_allwarn) then
              print *,'WARNING (mod_chop): can not build enough layers for'
              print *,'         section ',sec,' to ensure constraints on higher'
              print *,'         expansion coefficients of order ',ncut
              if (sec.lt.glqm_nsec) 
     &          print *,'WARNING (mod_chop): ',
     &                'discrete model will not contain all sections'
            endif
          else
c ok - get another layer in between the last two ones
            glqm_nlay=glqm_nlay+1
c            print *,'DEBUG (mod_chop): another extra layer sec ',sec,
c     &              ' ncut ',ncut,' nlay ',glqm_nlay
            dmodel(glqm_nlay, mi_depth)=dmodel(glqm_nlay-1, mi_depth)
            dmodel(glqm_nlay-1, mi_depth)=
     &        (dmodel(glqm_nlay-2, mi_depth)+dmodel(glqm_nlay, mi_depth))/2.
            do par=glqm_nlay-2,glqm_nlay
c              print *,'DEBUG (mod_chop): lay depth ',par, dmodel(par,mi_depth)
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
      do lay=2,glqm_nlay
c set correct section index
        if (sec.lt.glqm_nsec) then
          do while ((dmodel(lay, mi_depth).ge.mdepth(sec, i)).and.
     &              (sec.lt.glqm_nsec))
            sec=sec+1
          enddo
        endif
c crop marks:
        if (sec.eq.1) then
          sectop=0.d0
c and we are still in asphalt section
          glqm_lasas=lay
        else
          sectop=mdepth(sec-1,i)
        endif
        secbot=mdepth(sec,i)
        zref=0.5d0*(secbot+sectop)
        xtop=dmodel(lay, mi_depth)-zref
        if (lay.lt.glqm_nlay) then
          xbot=dmodel(lay+1, mi_depth)-zref
c calculate mean values for each parameter within layer
          do par=1,glqm_mpar
c number of coefficients
            npol=glqm_npol(sec,par)
            if (npol.eq.2) then
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
              dmodel(lay, par)=a1+0.5*a2*(xbot+xtop)
            elseif (npol.eq.3) then
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
              a3=model(3, sec, par, i)
c this look strange but is correct to calculate the mean parameter value
c between x0 and xb (checked again 13/01/99)
              dmodel(lay, par)=a1+0.5*a2*(xbot+xtop)+
     &          a3*(xbot*xbot+xtop*xtop+xtop*xbot)/3.
            else
              dmodel(lay, par)=model(1, sec, par, i)
            endif
          enddo
        else
c reached halfspace
c set parameter to value at top of halfspace
          do par=1,glqm_mpar
            npol=glqm_npol(sec,par)
            if (npol.eq.2) then
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
              dmodel(lay, par)=a1+a2*xtop
            elseif (npol.eq.3) then
              a1=model(1, sec, par, i)
              a2=model(2, sec, par, i)
              a3=model(3, sec, par, i)
              dmodel(lay, par)=a1+a2*xtop+a3*xtop*xtop
            else
              a1=model(1, sec, par, i)
              dmodel(lay, par)=a1
            endif
          enddo
        endif
      enddo
c 
c depth dimension should be km
      do lay=1,glqm_nlay
        dmodel(lay, mi_depth)=dmodel(lay, mi_depth)/1000.
      enddo
c 
c as this is the main factor for runtime it might be interesting
c for a user to know:
      if (verb_medstrategy) 
     &  print *,'NOTICE (mod_chop): ',
     &    'created ',glqm_nlay,' homogenous layers'
c
      if (verb_subaction) print *,'LEAVE mod_chop'
c 
      return
      end
c
c ----- END OF mod_chop.f -----
