c this is <pg_invfix.f>
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
c do an optimizing iteration using the fixed stabilization nu
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    02/03/99   V1.1   introduced mode 3
c    03/03/99   V1.2   output new refrence model every ten steps
c    04/03/99   V1.3   provide model on extra window
c    02/07/99   V1.4   now updates monitors
c    10/04/00   V1.5   introduced model_interval
c
      subroutine pg_invfix(nu, mode)
c
c mode=1: plot only improvement
c mode=2: plot improvement, model and green
c 
c the iteration will proceed as long as none of the following criteria
c is met:
c
c   - the X2 limit is reached
c   - the maximum iteration step limit is reached
c   - the X2 value of the new reference model is below the X2 value
c     of the very first reference model
c   - a model feasibilty error occured
c   - an inversion error occured
c 
c calls inv_X2 to find improved model
c 
      real nu
      integer mode
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_pgpara.inc'
      include 'glq_inv.inc'
      include 'glq_invres.inc'
      include 'glq_invpara.inc'
      include 'glq_verbose.inc'
      include 'glq_pgmon.inc'
c 
cE
      integer nstep, steptracker,pgopen
      integer mydevice, maindevice, curvevp
      logical hot, inv_mat, mod_check, mod_pcheck
      logical inv_x2
      real x2start, thisx2, refx2
      real dat_x2
      character*100 title
      character*20 mydevname
      parameter(mydevname='/XSERVE')
c 
      if (verb_subaction) print *,'ENTER pg_invfix(',nu,',',mode,')'
c 
      if (verb_topstrategy) print *,'INV (pg_invfix): X2 limit: ',lq_x2lim,
     &  ' iter. limit: ',lq_msteps,' nu: ',nu
c 
c initialize improvement array
      nstep=0
      steptracker=0
      if (verb_model) call mod_write(6, mb_ref, 'that''s it now')
      lq_npts=0
      call pgs_addval(lq_npts, glqm_mpts, lq_parimp, lq_x2imp, 0., 0.)
      call dat_dmode
c 
      lq_moderror=.false.
      lq_notimprove=.false.
      lq_inverror=.false.
c 
c catch main device
c      print *,'catch main dev ID'
      call pgqid(maindevice)
      mydevice=-1
c open subdevice if desired
      if (verb_graph) then
c        print *,'open my dev'
c        mydevice=pgp_open(mydevname)
        mydevice=pgopen(mydevname)
        call par_pgapply
c        print *,'haemm?'
        if (mydevice.lt.1) print *,
     &    'ERROR (pg_opt): could not open graphics subdevice'
c        print *,'select main dev'
        call pgslct(maindevice)
      endif
c get very first x2
      hot=mod_pcheck(mb_ref)
      if (hot) then
        call mod_chop(mb_ref)
        hot=mod_check()
      else
        if (verb_allwarn) print *,'WARNING: (pg_invfix): ',
     &    'model failed polynomial parameter check'
      endif
      if ((verb_allwarn).and.(.not.(hot))) print *,'WARNING: (pg_invfix): ',
     &  'model failed parameter check'
      if (hot) then
        call dat_synt(.true.)
        x2start=dat_X2(.true.)
      else
        lq_moderror=.true.
        if (verb_allwarn) print *,'WARNING (pg_invfix): ',
     &      'very first (entry) model model failed'
      endif
      refx2=x2start
c 
c start iteration
      do while (hot)
c go
        if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &    'check next model ',nstep+1,'/',lq_msteps
c 
c check new model estimate for nu
        nstep=nstep+1
        steptracker=steptracker+1
        if (steptracker.gt.model_interval) then
          steptracker=0
          if (verb_model) call mod_write(6, mb_ref, 'that''s it now')
        endif
        hot=inv_mat()
        if (hot) then
          hot=inv_X2(nu, thisx2)
        else
          if (verb_allwarn) print *,'WARNING (pg_invfix): ',
     &      'matrix building failed'
        endif
        if (hot) then
          if (thisx2.le.x2start) then
            if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &        'catch improved model - X2: ',thisx2,'  (X2start: ',x2start,')'
            call mod_copy(mb_work, mb_ref)
          else
            lq_notimprove=.true.
            hot=.false.
            if (verb_allwarn) print *,'WARNING (pg_invfix): ',
     &        'new model offers a poorer estimate than the very first one'
          endif
        endif
c 
c in case we want to take that
        if (hot) then
c maintain plots
          if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &      'plot history'
          call pgpage 
          call pgmon_update(pgmon_l_inner)
          if (mydevice.gt.0) then
            call pgslct(mydevice)
            call pgask(.false.)
            call pg_fullset(.true.)
            curvevp=0
          else
            curvevp=4
            call pg_mod(mb_ref)
            call pg_green(5, di_mcalc)
          endif
          call pgslct(maindevice)
          if (mode.eq.2) then
            call pgs_addval(lq_npts, glqm_mpts, lq_parimp, lq_x2imp, 
     &        float(nstep), (x2start-thisx2))
            write (title, 51) nu, x2start
            call pgs_curve2(curvevp, lq_npts, lq_parimp , lq_x2imp,
     &        'iteration', '\gx\u2\d\dstart\u-\gx\u2',
     &        title, .true., .false.)
          elseif (mode.eq.3) then
            call pgs_addval(lq_npts, glqm_mpts, lq_parimp, lq_x2imp, 
     &        float(nstep), 
     &        (100.*(refx2-thisx2)/refx2))
            write (title, 51) nu, x2start
            refx2=thisx2
            call pgs_curve2(curvevp, lq_npts, lq_parimp , lq_x2imp,
     &        'iteration', '\gx\u2\d change / per cent',
     &        title, .true., .false.)
          else
            call pgs_addval(lq_npts, glqm_mpts, lq_parimp, lq_x2imp, 
     &        float(nstep), (x2start-thisx2))
            write (title, 50) nu, x2start, thisx2
            call pgs_curve2(0, lq_npts, lq_parimp , lq_x2imp,
     &        'iteration', '\gx\u2\d\dstart\u-\gx\u2',
     &        title, .true., .false.)
          endif
        endif
c are we still in?
        if (nstep.ge.lq_msteps) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &      'decided not to go on because the iteration step limit is reached'
        endif
        if ((hot).and.(thisx2.lt.lq_x2lim)) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &      'decided not to go on because the X2 limit is reached'
        endif
        if (lq_moderror) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &      'decided not to go on because of a model error'
        endif
        if (lq_inverror) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &      'decided not to go on because of an inversion error'
        endif
        if (lq_notimprove) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_invfix): ',
     &      'decided not to go on because model could not be improved'
        endif
c next
      enddo
c close extra device
      if (mydevice.gt.0) then
        call pgslct(mydevice)
        call pgclos
        call pgslct(maindevice)
      endif
c 
      if (verb_subaction) print *,'LEAVE pg_invfix'
c 
      return
   50 format('iterative improvement for \\gn=',g7.1,' (\gx\u2\d\dstart\u=',
     &  f10.7,', \gx\u2\d=',f10.7,')')
   51 format('iterative improvement for \gn=',g7.1,' (\gx\u2\d\dstart\u=',
     &  f6.3,')')
      end
c
c ----- END OF pg_invfix.f -----
