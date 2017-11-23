c this is <par_set.f>
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
c set parameters
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    07/04/98   V1.1   introduced green amplitude error estimate
c    20/08/98   V1.2   explain modes
c    13/01/99   V1.3   there is no chop_hs anymore (see glq_model.inc)
c    07/04/00   V1.4   introduced new data modification options
c                      and new prefit mode
c    09/04/00   V1.5   now use conord1 and conord2
c    10/04/00   V1.6   introduced model_interval
c    11/04/00   V1.7   introduced vptrackfactor
c    02/06/00   V1.8   introduced mweightcondition
c    04/07/02   V1.9   support offset correction for mseisfk
c
      subroutine par_set
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_data.inc'
      include 'glq_model.inc'
      include 'glq_invpara.inc'
      include 'glq_mseiscorr.inc'
c 
cE
      character*80 arg
      logical hot
c 
      print *,' '
      print *,'PARSET:'
c 
      call par_showmod
      call par_showsrc
      call par_showdat
      call par_showinv
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *, 'PARSET - your command:'
        read(5, '(a80)'), arg
        if (arg(1:5).eq.'help ') then
          print *,'quit exit show smod sdat sinv ssrc explain'
          print *,'(and any bracket-command to set values)'
        elseif (arg(1:5).eq.'quit ') then
          hot=.false.
        elseif (arg(1:5).eq.'exit ') then
          hot=.false.
        elseif (arg(1:8).eq.'explain ') then
          print *,' '
          call par_showmodes
          print *,' '
          call par_showindex
        elseif (arg(1:5).eq.'sdat ') then
          call par_showdat
        elseif (arg(1:5).eq.'sinv ') then
          call par_showinv
        elseif (arg(1:5).eq.'ssrc ') then
          call par_showsrc
        elseif (arg(1:5).eq.'smod ') then
          call par_showmod
        elseif (arg(1:5).eq.'show ') then
          call par_showmod
          call par_showsrc
          call par_showdat
          call par_showinv
        elseif (arg(1:5).eq.'sdep ') then
          read(arg(6:80), *, err=98, end=98) src_depth
        elseif (arg(1:5).eq.'samp ') then
          read(arg(6:80), *, err=98, end=98) src_amp
        elseif (arg(1:5).eq.'styp ') then
          read(arg(6:80), *, err=98, end=98) src_type
          src_type=min(2,max(1,src_type))
        elseif (arg(1:5).eq.'chst ') then
          read(arg(6:80), *, err=98, end=98) chop_step
c        elseif (arg(1:5).eq.'tohs ') then
c          read(arg(6:80), *, err=98, end=98) chop_hs
        elseif (arg(1:5).eq.'smin ') then
          read(arg(6:80), *, err=98, end=98) val_smin
        elseif (arg(1:5).eq.'smax ') then
          read(arg(6:80), *, err=98, end=98) val_smax
        elseif (arg(1:5).eq.'fmin ') then
          read(arg(6:80), *, err=98, end=98) val_fmin
        elseif (arg(1:5).eq.'fmax ') then
          read(arg(6:80), *, err=98, end=98) val_fmax
        elseif (arg(1:5).eq.'xmax ') then
          read(arg(6:80), *, err=98, end=98) val_xmax
        elseif (arg(1:5).eq.'bala ') then
          read(arg(6:80), *, err=98, end=98) balance
          balance=min(1.,max(0.,balance))
        elseif (arg(1:5).eq.'tter ') then
          read(arg(6:80), *, err=98, end=98) tterror
          tterror=max(1.e-6,tterror)
        elseif (arg(1:5).eq.'gerr ') then
          read(arg(6:80), *, err=98, end=98) gerror
          gerror=max(1.e-10,gerror)
        elseif (arg(1:5).eq.'gmod ') then
          read(arg(6:80), *, err=98, end=98) datamode
          datamode=min(35,max(1,datamode))
        elseif (arg(1:5).eq.'logs ') then
          read(arg(6:80), *, err=98, end=98) logstretch
          logstretch=max(0.1,logstretch)
        elseif (arg(1:5).eq.'poly ') then
          read(arg(6:80), *, err=98, end=98) polyord
          polyord=max(0,min(6,polyord))
        elseif (arg(1:5).eq.'cons ') then
          read(arg(6:80), *, err=98, end=98) conord1
          conord1=max(0.01,min(100.,conord1))
        elseif (arg(1:5).eq.'cono ') then
          read(arg(6:80), *, err=98, end=98) conord2
          conord2=max(0.01,min(100.,conord2))
        elseif (arg(1:5).eq.'cont ') then
          read(arg(6:80), *, err=98, end=98) conthresh
          conthresh=min(1.,max(0.,conthresh))
        elseif (arg(1:5).eq.'pwco ') then
          read(arg(6:80), *, err=98, end=98) mweightcondition
          mweightcondition=abs(mweightcondition)
        elseif (arg(1:5).eq.'pder ') then
          read(arg(6:80), *, err=98, end=98) pvar_pdev
        elseif (arg(1:5).eq.'mean ') then
          read(arg(6:80), *, err=98, end=98) lq_nmean
          lq_nmean=max(1,lq_nmean)
        elseif (arg(1:5).eq.'cmas ') then
          read(arg(6:80), *, err=98, end=98) chop_master
          chop_master=min(glqm_mpar,max(0,chop_master))
        elseif (arg(1:5).eq.'mste ') then
          read(arg(6:80), *, err=98, end=98) lq_msteps
          lq_msteps=max(1,lq_msteps)
        elseif (arg(1:5).eq.'mint ') then
          read(arg(6:80), *, err=98, end=98) model_interval
          model_interval=max(1,model_interval)
        elseif (arg(1:5).eq.'xlim ') then
          read(arg(6:80), *, err=98, end=98) lq_x2lim
        elseif (arg(1:5).eq.'reli ') then
          read(arg(6:80), *, err=98, end=98) lq_relimp
        elseif (arg(1:5).eq.'numi ') then
          read(arg(6:80), *, err=98, end=98) lq_numin
        elseif (arg(1:5).eq.'numa ') then
          read(arg(6:80), *, err=98, end=98) lq_numax
        elseif (arg(1:5).eq.'mido ') then
          read(arg(6:80), *, err=98, end=98) lq_mindown
          lq_mindown=min(int(glqm_mpts/2),lq_mindown)
          lq_mindown=max(1,lq_mindown)
        elseif (arg(1:5).eq.'fitt ') then
          read(arg(6:80), *, err=98, end=98) chop_finett
          chop_finett=max(0,min(chop_finett, glqm_mlay-glqm_nsec))
        elseif (arg(1:5).eq.'tpre ') then
          read(arg(6:80), *, err=98, end=98) dottprefit
          dottprefit=max(0, min(data_ntts, dottprefit))
        elseif (arg(1:5).eq.'cpre ') then
          read(arg(6:80), *, err=98, end=98) prefit_mode
          prefit_mode=min(3,max(0,prefit_mode))
        elseif (arg(1:5).eq.'vptr ') then
          read(arg(6:80), *, err=98, end=98) vptrackfactor
        elseif (arg(1:5).eq.'msco ') then
          call dat_mscprep(.not.(msc_apply))
        else
          print *,'WARNING (par_set): unknown command ',arg(1:index(arg, ' ')),
     &            ' (try ''help'')'
        endif
        goto 99
   98   print *,'ERROR (par_set): illegal value string'
   99   continue
      enddo
c
      call dat_dmode
c
      if (vptrackfactor.gt.0.d0) then
        call mod_track(mb_work)
        call mod_track(mb_ref)
      endif
c 
      return
      end
c
c ----- END OF par_set.f -----
