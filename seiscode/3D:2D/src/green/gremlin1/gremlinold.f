c this is <gremlinold.f>
c------------------------------------------------------------------------------
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c This is the old gremlin code
c
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
c 05/12/97 by Thomas Forbriger (IfG Stuttgart)
c
c GREens Matrix Linearized INversion
c
c REVISIONS and CHANGES
c    05/12/97   V1.0   Thomas Forbriger
c    09/12/97   V1.1   first somehow running version
c    10/12/97   V1.2   - apply datamode and logstretch changes
c                      - use direct acces to glq common blocks now
c                      - no oldsub_parset anymore
c    12/12/97   V1.3   - included parameters for pg_inv
c                      - provides now inversion steps
c                      - new inversion modus
c    18/12/97   V1.4   - new auto function
c    14/01/99   V1.5   - this code is no further maintained to track changes
c                        model format and so on (chop_hs) - it was removed
c                        from gremlin binary code
c
c==============================================================================
c
      subroutine oldgremlin
c
      character*79 version
      parameter(version='GREMLINOLD   V1.5   GREens Matrix Linearized INversion')
c common blocks
      include 'libs/glq_dim.inc'
      include 'gremlin.inc'
c
c program loop
      logical hot
      character*80 argument
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *,'GREMLIN OLD - your command:'
        read(5, '(a80)') argument
c 
        if (argument(1:5).eq.'read ') then
          call oldsub_read(argument(6:80))
        elseif (argument(1:6).eq.'write ') then
          call oldsub_write(argument(7:80), version)
        elseif (argument(1:5).eq.'calc ') then
          call oldsub_calc(argument(6:80))
        elseif (argument(1:5).eq.'step ') then
          call oldsub_step(argument(6:80))
        elseif (argument(1:5).eq.'show ') then
          call oldsub_show(argument(6:80))
        elseif (argument(1:4).eq.'set ') then
          call oldsub_set(argument(5:80))
        elseif (argument(1:5).eq.'stop ') then
          hot=.false.
        elseif (argument(1:5).eq.'exit ') then
          hot=.false.
        elseif (argument(1:5).eq.'quit ') then
          hot=.false.
        elseif (argument(1:5).eq.'auto ') then
          call oldsub_auto(argument(6:80))
        elseif (argument(1:5).eq.'test ') then
          call oldsub_test(version)
        elseif (argument(1:5).eq.'help ') then
          call oldsub_help(version)
        elseif (argument(1:7).eq.'invers ') then
          call oldsub_inversion
        else
          print *,'NOTICE: unknown command ',argument(1:index(argument, ' '))
        endif
c
      enddo
c 
      return
      end
c
c======================================================================
c some subroutines
c
      subroutine oldsub_read(arg)
c
c read files
c 
      include 'libs/glq_dim.inc'
c 
      character arg*(*)
c 
      integer arglen
c 
      arglen=len(arg)
c
      if (arg(1:6).eq.'green ') then
        call dat_rgreen(arg(7:arglen))
      elseif (arg(1:3).eq.'tt ') then
        call dat_rtt(arg(4:arglen))
      elseif (arg(1:6).eq.'model ') then
        call mod_read(arg(7:arglen), mb_ref)
      elseif (arg(1:5).eq.'para ') then
        call oldsub_rpara(arg(6:arglen))
      else
        print *,'NOTICE: unknown read command'
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine oldsub_write(arg, version)
c
c read files
c 
      include 'libs/glq_dim.inc'
c 
      character arg*(*), version*(*)
c 
      integer arglen
c 
      arglen=len(arg)
c
      if (arg(1:6).eq.'model ') then
        if (arg(7:9).eq.' o ') then
          call mod_save(arg(10:arglen), mb_ref, .true., version)
        else
          call mod_save(arg(7:arglen), mb_ref, .false., version)
        endif
      else
        print *,'NOTICE: unknown write command'
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine oldsub_show(arg)
c
c show parameters and graphics
c
      include 'libs/glq_dim.inc'
c 
      character arg*(*)
c 
      integer i, arglen
c
      arglen=len(arg)
c 
      if (arg(1:6).eq.'green ') then
        read (arg(7:arglen), '(i10)') i
        call pgpage
        call pg_green(0, i)
      elseif (arg(1:4).eq.'ctt ') then
        read (arg(5:arglen), '(i10)') i
        call pgpage
        if (i.eq.1) then
          call pg_tt(0, .true.)
        else
          call pg_tt(0, .false.)
        endif
      elseif (arg(1:4).eq.'mod ') then
        read (arg(5:arglen), *) i
        call pgpage
        call mod_chop(i)
        call pg_mod(i)
      elseif (arg(1:4).eq.'ref ') then
        call pgpage
        call mod_chop(mb_ref)
        call pg_mod(mb_ref)
        call pg_green(5, di_mref)
        call pg_tt(4, .true.)
      elseif (arg(1:7).eq.'parcor ') then
        call mod_showparcor
      elseif (arg(1:5).eq.'para ') then
        call oldsub_showpara
      elseif (arg(1:6).eq.'index ') then
        call oldsub_showindex
      else
        print *,'NOTICE: unknown show command'
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine oldsub_step(arg)
c
c perform complete iteration steps
c
      include 'libs/glq_dim.inc'
c 
      character arg*(*)
c 
      integer arglen, nsteps
      logical mod_prep, inv_model, result, inv_part
      real thisnu
c 
      arglen=len(arg)
c 
      if (arg(1:5).eq.'cref ') then
        print *,'STEP: calculate reference synthetics'
        call mod_aclear
        if (mod_prep()) then
          call dat_synt(.true.)
        else
          print *,'WARNING: model preparation failed'
        endif
      elseif (arg(1:5).eq.'part ') then
        print *,'STEP: calculate partial derivatives'
        result=inv_part()
        if (.not.(result)) print *,'WARNING: calculation failed'
      elseif (arg(1:10).eq.'easystart ') then
        print *,'STEP: start easy...'
        call dat_dmode
        print *,'STEP: set all parameter weights to 1'
        call mod_weight(0)
        print *,'STEP: deactivate all anonymous parameters'
        call par_sano(0, mb_ref)
        print *,'STEP: activate p-velocity'
        call par_sano(mi_alpha, mb_ref)
        print *,'STEP: activate s-velocity'
        call par_sano(mi_beta, mb_ref)
      elseif (arg(1:4).eq.'inv ') then
        read(arg(5:arglen), *) thisnu, nsteps
        print *,'STEP: show inversion for nu ',thisnu,' with ',
     &          nsteps,' steps'
        call pgpage
        call pg_inv(thisnu, nsteps, 0)
      elseif (arg(1:4).eq.'mat ') then
        print *,'STEP: prepare all the matrix stuff'
        call inv_dss
        call inv_dssd
        call inv_dssdelta
      elseif (arg(1:7).eq.'takenu ') then
        read(arg(8:arglen), *) thisnu
        print *,'STEP: take model for nu ',thisnu
        if (inv_model(thisnu)) then
          call mod_parcor
          call mod_copy(mb_work, mb_ref)
        else
          print *,'WARNING: system of linear equations not solved'
        endif
      else
        print *,'NOTICE: unknown calc command'
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine oldsub_calc(arg)
c
c do calculations
c
      include 'libs/glq_dim.inc'
c 
      character arg*(*)
c 
      integer arglen, i
      logical mod_prep
c 
      arglen=len(arg)
c 
      if (arg(1:7).eq.'aclear ') then
        call mod_aclear
      elseif (arg(1:6).eq.'mprep ') then
        if (.not.(mod_prep())) print *,'WARNING: model preparation failed'
      elseif (arg(1:5).eq.'synt ') then
        read(arg(6:arglen), '(i10)') i
        if (i.eq.1) then
          call dat_synt(.true.)
        else
          call dat_synt(.true.)
        endif
      else
        print *,'NOTICE: unknown calc command'
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine oldsub_set(arg)
c
c set parameters 
c
      include 'libs/glq_dim.inc'
      include 'gremlin.inc'
      include 'libs/glq_para.inc'
c 
      character arg*(*)
c 
      integer iprefit, arglen, i
c 
      arglen=len(arg)
c 
      if (arg(1:5).eq.'smin ') then
        read(arg(6:arglen), '(f10)') val_smin
      elseif (arg(1:5).eq.'smax ') then
        read(arg(6:arglen), '(f10)') val_smax
      elseif (arg(1:5).eq.'fmin ') then
        read(arg(6:arglen), '(f10)') val_fmin
      elseif (arg(1:5).eq.'fmax ') then
        read(arg(6:arglen), '(f10)') val_fmax
      elseif (arg(1:5).eq.'xmax ') then
        read(arg(6:arglen), '(f10)') val_xmax
        val_xmax=val_xmax*0.001
      elseif (arg(1:6).eq.'gmode ') then
        read(arg(7:arglen), '(i10)') datamode
      elseif (arg(1:8).eq.'balance ') then
        read(arg(9:arglen), '(f10)') balance
      elseif (arg(1:6).eq.'tterr ') then
        read(arg(7:arglen), '(f10)') tterror
      elseif (arg(1:9).eq.'chopstep ') then
        read(arg(10:arglen), '(f10)') chop_step
      elseif (arg(1:7).eq.'chophs ') then
        read(arg(8:arglen), '(f10)') chop_hs
      elseif (arg(1:5).eq.'pder ') then
        read(arg(6:arglen), '(f10)') pvar_pdev
      elseif (arg(1:7).eq.'weight ') then
        read(arg(8:arglen), '(i10)') i
        call mod_weight(i)
      elseif (arg(1:12).eq.'ilogstretch ') then
        read(arg(13:arglen), '(i10)') i
        logstretch=10.**i
      elseif (arg(1:6).eq.'dmode ') then
        call dat_dmode
      elseif (arg(1:5).eq.'sano ') then
        read(arg(6:arglen), '(i10)') i
        call par_sano(i, mb_ref)
      elseif (arg(1:7).eq.'device ') then
        device=arg(8:arglen)
        call pgend
        call pgp_setdevice(device, 1, 1)
      elseif (arg(1:4).eq.'ask ') then
        read(arg(5:arglen), '(i10)') iprefit
        if (iprefit.eq.1) then
          call pgask(.true.)
        else
          call pgask(.false.)
        endif
      elseif (arg(1:7).eq.'prefit ') then
        read(arg(8:arglen), '(i10)') prefit_mode
        prefit_mode=min(2,max(0,prefit_mode))
      else
        print *,'NOTICE: unknown set command'
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine oldsub_test(arg)
c
c test code
c
      include 'libs/glq_dim.inc'
      include 'gremlin.inc'
c
      character arg*(*)
c 
      logical mod_prep
c 
      print *,arg
      print *,'do some actual testing'
      print *,' '
      print *,' '
      call pgpage
      call dat_dmode
      call pg_green(0, di_read)
      call pgpage
      call pg_green(0, di_mread)
      call pgpage
      if (.not.(mod_prep())) then
        print *,'WARNING (test): could not prepare model'
        call pg_mod(mb_work)
      else
        call dat_synt(.true.)
        call pg_mod(mb_work)
        call pg_tt(4, .true.)
        call pg_green(5, di_mref)
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine oldsub_help(arg)
c
c give help
c
      character arg*(*)
c 
      print *,arg
      print *,'finish program with command exit'
      print *,' '
      print *,'read'
      print *,'  green'
      print *,'  tt'
      print *,'  model'
      print *,'  para'
      print *,' '
      print *,'write'
      print *,'  model [o] filename'
      print *,' '
      print *,'step'
      print *,'  easystart'
      print *,'  cref'
      print *,'  part'
      print *,'  mat'
      print *,'  inv numax, steps'
      print *,'  takenu'
      print *,' '
      print *,'set'
      print *,'  smin'
      print *,'  smax'
      print *,'  fmin'
      print *,'  fmax'
      print *,'  xmax x                   maximum offset in m'
      print *,'  balance'
      print *,'  tterr'
      print *,'  sano'
      print *,'  weight'
      print *,'  device'
      print *,'  gmode'
      print *,'  prefit 0|1'
      print *,'  dmode'
      print *,'  ask 0|1'
      print *,'  chophs'
      print *,'  chopstep'
      print *,'  pder'
      print *,'  *srctype'
      print *,'  *srcdepth'
      print *,'  *srcamp'
      print *,' '
      print *,'calc'
      print *,'  aclear'
      print *,'  mprep'
      print *,'  synt'
      print *,' '
      print *,'show'
      print *,'  green'
      print *,'  ctt'
      print *,'  mod'
      print *,'  ref'
      print *,'  para'
      print *,'  index'
      print *,'  parcor'
      print *,' '
      print *,'stop'
      print *,' '
      print *,'quit'
      print *,' '
      print *,'exit'
      print *,' '
      print *,'test'
      print *,' '
      print *,'help'
      print *,' '
      print *,'invers         enter inversion modus'
      print *,' '
      print *,'auto limit'
      print *,' '
      print *,'inversion modus commands:'
      print *,'-------------------------'
      print *,'g numax, nstep       first step'
      print *,'n                    next step'
      print *,'tt                   take that nu'
      print *,'t nu                 take nu'
      print *,'break                exit inversion modus'
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine oldsub_rpara(filename)
c 
c read parameter file
c
      include 'libs/glq_dim.inc'
      include 'libs/glq_para.inc'
c
      character filename*(*)
c 
      integer lu, i
      parameter(lu=17)
c 
      print *,'NOTICE: read parameter file ',filename(1:index(filename, ' '))
      open(lu, file=filename, status='old', err=99)
      read(lu, 50, err=98, end=97)
      read(lu, *, err=98, end=97) val_fmin, val_fmax, val_smin, 
     &                            val_smax, val_xmax
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) balance, tterror, datamode, logstretch
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) prefit_mode
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) chop_hs, chop_step
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) src_depth, src_amp, src_type
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) (rng_mmin(i), i=1,glqm_mpar)
      read(lu, *, err=98, end=97) (rng_mmax(i), i=1,glqm_mpar)
      read(lu, 51, err=98, end=97)
      read(lu, *, err=98, end=97) pvar_pdev
      close(lu, err=96)
      print *,'NOTICE: file read and closed'
c 
      return
   50 format(//)
   51 format(/)
   99 stop 'ERROR (read para): opening file'
   98 stop 'ERROR (read para): reading file'
   97 stop 'ERROR (read para): reading file - unexpected end'
   96 stop 'ERROR (read para): closing file'
      end
c 
c----------------------------------------------------------------------
c 
      subroutine oldsub_showpara
c 
c display parameter set
c
      include 'libs/glq_dim.inc'
      include 'libs/glq_para.inc'
c 
      integer i
c
      print 50, val_fmin, val_fmax, val_smin, val_smax, val_xmax*1000.
      print 51, balance, tterror*1000., datamode, logstretch
      if (prefit_mode.eq.0) then
        print 52, 'do complex amplitude prefit'
      elseif (prefit_mode.eq.1) then
        print 52, 'do real amplitude prefit'
      elseif (prefit_mode.eq.2) then
        print 52, 'do mean real amplitude prefit'
      else
        call mod_panic('ERROR: unknown prefit mode')
      endif
      print 53, chop_hs, chop_step
      print 54, src_depth*1000., src_amp, src_type
      print 55, (rng_mmin(i), i=1,glqm_mpar)
      print 56, (rng_mmax(i), i=1,glqm_mpar)
      print 57, pvar_pdev
c 
      return
   50 format('parameter values:',//,
     &  'frequency:  ',f10.3,' Hz   - ',f10.3,' Hz',/
     &  'slowness:   ',f10.3,' s/km - ',f10.3,' s/km',/
     &  'offset:   - ',f10.3,' m')
   51 format('balance:      ',f10.7,/
     &       ' tt-error:    ',f10.3,' ms   ',/
     &       'green mode:   ',i2,/
     &       'log-stretch:  ',g10.2)
   52 format(a)
   53 format('top of halfspace:  ',f10.3,' m',/
     &       'chopping stepsize: ',f10.7)
   54 format('source depth:      ',f10.3,' m',/
     &       'source amplitude:  ',f10.3,/
     &       'source type:       ',i2)
   55 format('minimum values:',/
     &    t6,'alpha',t12,'beta',t24,'density',t36,'Qalpha',t48,'Qbeta',/
     &    5(f12.3))
   56 format('maximum values:',/
     &    t6,'alpha',t12,'beta',t24,'density',t36,'Qalpha',t48,'Qbeta',/
     &    5(f12.3))
   57 format('relative variation for partial derivative approx.: ',f10.6)
      end
c 
c----------------------------------------------------------------------
c 
      subroutine oldsub_showindex
c 
c set parameters
c
      include 'libs/glq_dim.inc'
c 
      print *,'Vp:        ',mi_alpha
      print *,'Vs:        ',mi_beta
      print *,'density:   ',mi_density
      print *,'Qalpha:    ',mi_Qalpha
      print *,'Qbeta:     ',mi_Qbeta
      print *,' '
      print *,'read data:            ',di_read
      print *,'modified data:        ',di_mread
      print *,'synthetics:           ',di_mcalc
      print *,'reference synthetics: ',di_mref
      print *,' '
      print *,'reference model       ',mb_ref
      print *,'work model            ',mb_work
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine oldsub_inversion
c
c handle inversion modus
c
      include 'libs/glq_dim.inc'
      include 'libs/glq_para.inc'
      include 'libs/glq_invres.inc'
      include 'libs/glq_model.inc'
      include 'libs/glq_pgpara.inc'
c 
      character argument*80
      logical hot, didrunonce, mod_prep, inv_model, inv_part, result
      integer nsteps, i
      real startnu
c 
      print *,'NOTICE: you entered inversion modus'
c 
      found_best=.false.
      didrunonce=.false.
c calc reference
      print *,'STEP: calculate reference synthetics'
      call dat_dmode
      call mod_aclear
      if (mod_prep()) then
        call dat_synt(.true.)
      else
        print *,'WARNING: model preparation failed'
      endif
c display reference
      call pgpage
      call mod_chop(mb_ref)
      call pg_mod(mb_ref)
      call pg_green(5, di_mref)
      call pg_tt(4, .true.)
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *,'your inversion command:'
        read(5, '(a80)') argument
c 
        if (argument(1:3).eq.'tt ') then
          if (found_best) then
            print *,'INVERSION: take model for nu ',best_nu
            found_best=.false.      
            didrunonce=.false.
c take the best one
            if (inv_model(best_nu)) then
              call mod_parcor
              call mod_copy(mb_work, mb_ref)
            else
              print *,'WARNING: system of linear equations not solved'
            endif
c calc reference
            print *,'STEP: calculate reference synthetics'
            do i=1,mod_n
              print *,'ESTIMATE ',i,mdelta(i)
            enddo
            call mod_aclear
            if (mod_prep()) then
              call dat_synt(.true.)
            else
              print *,'WARNING: model preparation failed'
            endif
c display reference
            call pgpage
            call mod_chop(mb_ref)
            call pg_mod(mb_ref)
            call pg_green(5, di_mref)
            call pg_tt(4, .true.)
          else
            print *,'WARNING: you did not find a best model'
          endif
        elseif (argument(1:2).eq.'t ') then
          read(argument(3:80), *) startnu
          print *,'INVERSION: take model for nu ',startnu
          found_best=.false.      
          didrunonce=.false.
c take the best one
          if (inv_model(startnu)) then
            call mod_parcor
            call mod_copy(mb_work, mb_ref)
          else
            print *,'WARNING: system of linear equations not solved'
          endif
c calc reference
          print *,'STEP: calculate reference synthetics'
          do i=1,mod_n
            print *,'ESTIMATE ',i,mdelta(i)
          enddo
          call mod_aclear
          if (mod_prep()) then
            call dat_synt(.true.)
          else
            print *,'WARNING: model preparation failed'
          endif
c display reference
          call pgpage
          call mod_chop(mb_ref)
          call pg_mod(mb_ref)
          call pg_green(5, di_mref)
          call pg_tt(4, .true.)
        elseif (argument(1:2).eq.'g ') then
          read(argument(3:80), *) startnu, nsteps
          print *,'INVERSION: start search from nu ',startnu,
     &            ' using ',nsteps,' steps'
          result=inv_part()
          if (result) then
            call inv_dss
            call inv_dssd
            call inv_dssdelta
            call pgpage
            call pg_inv(startnu, nsteps, 0)
            didrunonce=.true.
          else
            print *,'WARNING: calculation of partial derivatives failed'
          endif
        elseif (argument(1:2).eq.'n ') then
          print *,'INVERSION: next serach step'
          if (didrunonce) then
            if (found_best) then
              print *,'WARNING: deny - you found a best nu already'
            else
              startnu=small_nu*3
              call pgpage
              call pg_inv(startnu, nsteps, 0)
            endif
          else
            print *,'WARNING: start search with ''g'' first'
          endif
        elseif (argument(1:6).eq.'break ') then
          hot=.false.
        elseif (argument(1:5).eq.'help ') then
          call oldsub_help('you are in INVERSION MODUS')
        else
          print *,'NOTICE: unknown inversion command ',
     &      argument(1:index(argument, ' '))
        endif
c
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine oldsub_auto(arg)
c 
c auto minimize error
c
      real limit
      logical pg_min
      character*(*) arg
c 
      print *,'AUTOMATIC error minimization'
      read (arg, *) limit
      if (pg_min(limit)) then
        print *,'AUTOMATIC minimization succeded'
      else
        print *,'AUTOMATIC minimization fails for some reason'
      endif
      call sub_beep
c 
      return
      end
c 
c ----- END OF gremlinold.f -----
