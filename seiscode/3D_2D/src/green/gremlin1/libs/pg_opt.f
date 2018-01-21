c this is <pg_opt.f>
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
c optimize model starting at nu=1. regarding all lq-paremeters
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    17/08/98   V1.1   introduced graph verbosity
c    24/01/99   V1.2   - introduced verb_changes option
c                      - apply pgpara settings to new device
c    03/03/99   V1.3   - display reference model every ten steps
c    02/07/99   V1.4   - now updates monitors
c    10/04/00   V1.5   introduced model_interval
c
      subroutine pg_opt
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
      integer nstep, i, npts, steptracker
      logical hot, inv_model
      real nsteps(glqm_mpts), x2step(glqm_mpts), x2start, mean
      real thisnu, para
      character*100 title
c pgplot device identifiers
      integer maindevice, mydevice, pgopen
      character*10 mydevname
      parameter(mydevname='/XSERVE')
c 
      para(thisnu)=-10*log10(thisnu)
c 
      if (verb_subaction) print *,'ENTER pg_opt'
c 
      if (verb_topstrategy) print *,'OPT (pg_opt): X2 limit: ',lq_x2lim,
     &  ' impr. limit: ',lq_relimp,' iter. limit: ',lq_msteps
c 
      nstep=0
      steptracker=0
      if (verb_model) call mod_write(6, mb_ref, 'that''s it now')
      npts=0
      hot=.true.
      thisnu=1.
      lq_notimprove=.false.
      call pgs_addval(npts, glqm_mpts, nsteps, x2step, 0., 0.)
      call dat_dmode
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
c 
      do while (hot)
c go
        if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &    'look for new minimum ',nstep+1,'/',lq_msteps
        steptracker=steptracker+1
        if (steptracker.gt.model_interval) then
          steptracker=0
          if (verb_model) call mod_write(6, mb_ref, 'that''s it now')
        endif
        call inv_min(thisnu)
        if (.not.((lq_inverror).or.(lq_moderror).or.(lq_notimprove))) then
          if (verb_topstrategy) then
            if (found_best) then
              print *,'NOTICE (pg_opt): ',
     &          'found best model'
            else
              print *,'NOTICE (pg_opt): ',
     &          'did not reach best model - take a new reference anyway'
            endif
          endif
c catch starting value (experience)
          thisnu=small_nu*2.
          nstep=nstep+1
c catch reference
          if (nstep.eq.1) x2start=lq_x2ref
c maintain plots
          call pgs_addval(npts, glqm_mpts, nsteps, x2step, 
     &      float(nstep), (x2start-best_x2))
          do i=1,lq_npts
            lq_parimp(i)=para(lq_parimp(i))
            lq_x2imp(i)=lq_x2ref-lq_x2imp(i)
          enddo
c plot
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'plot history'
c switch to main device an start curve plot
          call pgslct(maindevice)
          call pgpage 
          call pgmon_update(pgmon_l_inner)
          write (title, 50) x2start, best_x2
          call pgs_curve2(12, npts ,nsteps ,x2step ,
     &      'iteration', '\gx\u2\d\dstart\u-\gx\u2',
     &      title, .true., .false.)
          write (title, 51) lq_x2ref, best_x2
          call pgs_curve2(13, lq_npts ,lq_parimp ,lq_x2imp ,
     &      '-10*log\d10\u(\gn)', '\gx\u2\d\dref\u-\gx\u2\d(\gn)',
     &      title, .false., .false.)
c catch reference
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'catch improved model'
          hot=inv_model(small_nu)
          if (verb_changes) call inv_repchange
          if (hot) then
            call mod_parcor
            call mod_copy(mb_work, mb_ref)
c plot new reference
            if (mydevice.gt.0) then
              call pgslct(mydevice)
              call pgask(.false.)
              call pg_fullset(.true.)
              call pgslct(maindevice)
            endif
          else
            if (verb_allwarn) print *,'WARNING (pg_opt): ',
     &        'unexpected problems finding model - break'
          endif
        endif
c are we still in?
        if (nstep.ge.lq_msteps) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'decided not to go on because the iteration step limit is reached'
        endif
        if ((hot).and.(best_x2.lt.lq_x2lim)) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'decided not to go on because the X2 limit is reached'
        endif
        if ((hot).and.(lq_moderror)) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'decided not to go on because of a model error'
        endif
        if ((hot).and.(lq_inverror)) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'decided not to go on because of an inversion error'
        endif
        if ((hot).and.(lq_notimprove)) then
          hot=.false.
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'decided not to go on because model could not be improved'
        endif
c check improvement
        if ((hot).and.(npts.gt.2).and.(lq_nmean.gt.0)) then
          mean=(x2step(npts)-x2step(max(1,(npts-lq_nmean))))/
     &      (npts-max(1,(npts-lq_nmean)))/
     &      abs(x2start-lq_x2lim)
          if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &      'mean improvement is ',mean,' (limit:',lq_relimp,')'
          if (mean.lt.lq_relimp) then
            hot=.false.
            lq_notimprove=.true.
            if (verb_topstrategy) print *,'NOTICE (pg_opt): ',
     &        'decided not to go on because mean improvement is below limit'
          endif
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
      if (verb_subaction) print *,'LEAVE pg_opt'
c 
      return
   50 format('iterative improvement (\gx\u2\d\dstart\u=',
     &  f10.7,', \gx\u2\d\dbest\u=',f10.7,')')
   51 format('finding last minimum (\gx\u2\d\dref\u=',
     &  f10.7,', \gx\u2\d\dmin\u=',f10.7,')')
      end
c
c ----- END OF pg_opt.f -----
