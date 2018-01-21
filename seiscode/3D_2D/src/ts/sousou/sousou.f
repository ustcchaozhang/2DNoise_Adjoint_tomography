c this is <sousou.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c find SOUrce of air-coupled SOUnd wave
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
c    17/11/98   V1.0   Thomas Forbriger
c    23/11/98   V1.1   allow stacking to mid of spread
c    23/02/99   V1.2   a factor of two was missing in envelope calculations
c                      did correct that although it did not matter for the
c                      purpose of this program
c
c==============================================================================
c
      program sousou
c
      character*79 version
      parameter(version='SOUSOU   V1.2   find SOUrce of air-coupled SOUnd wave')
c 
      include 'sousou_options.inc'
      include 'sousou_para.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
c
c 
      integer pgp_open
      character*80 comment
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=17)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-D, 2h-v,2h-a,2h-d,2h-h,2h-l,2h-F,2h-E,2h-s,
     &  2h-S,2h-m,2h-M,2h-P,2h-t,2h-T,2h-c,2h-O/
      data opthasarg/3*.FALSE.,7*.TRUE.,3*.FALSE.,3*.TRUE.,.FALSE./
      data optarg/3*1h-,3hx11,6h300.,4,3*1h-,9h325.,345.,4*1h-,3h0.1,1h-,
     &  4hgray,1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: sousou [-D] [-v] [-a] [-d dev] [-P]'
        print *,'              [-h f,n] [-l f,n] [-m] [-M] [-c map]'
        print *,'              [-F file] [-E file] [-S file]'
        print *,'              [-s vel1,vel2] [-t fac] [-O] file ...'
        print *,'   or: sousou -help'
        if (iargc().lt.1) stop 'ERROR: missing arguments'
        print *,' '
        print *,'find SOUrce of air-coupled SOUnd wave'
        print *,' '
        print *,'This program uses a slant stack correlation analysis'
        print *,'of the air-coupled sound wave to find the real shot'
        print *,'location relative to the provided coordinate system'
        print *,'of the input profile.'
        print *,' '
        print *,'-D           enter debug mode'
        print *,'-v           be verbose'
        print *,'-a           ask before creating a new screen plot'
        print *,'-d dev       use graphics device ''dev'' '
        print *,'             default: dev=',optarg(4)(1:10)
        print *,'-P           enter pick-mode if available'
        print *,'-h f,n       apply phase free ''butterworth'' high pass'
        print *,'             of order n and eigenfrequency f'
        print *,'             default: f,n=',optarg(5)(1:10)
        print *,'-l f,n       apply inverse phase ''butterworth'' low pass'
        print *,'             of order n and eigenfrequency f'
        print *,'             this is used to remove phase delay times of'
        print *,'             anti-alias filters in the recorder'
        print *,'-m           calculate envelope of seismograms'
        print *,'-M           calculate envelope of tau-p traces'
        print *,'-F file      write result of filter stage to ''file'' '
        print *,'-E file      write result of envelope calculation to ''file'' '
        print *,'-S file      write result of slant stack to ''file'' '
        print *,'-s vel1,vel2 search in velocity range from vel1 to vel2'
        print *,'             default: vel1,vel2=',optarg(9)(1:10)
        print *,'-t fac       threshold for maximum search will be'
        print *,'             fac*(absolute maximum)'
        print *,'             default: fac=',optarg(14)(1:10)
        print *,'-T title     title to use for graymap plot'
        print *,'-c map       select type of colour map'
        print *,'             default: map=',optarg(16)(1:10)
        print *,'-O           stack to mid of spread'
        print *,'file ...     SFF input files'
        print *,' '
        print *,'For input files the usual t:-trace selection may be used.'
        print *,' '
        call pgp_showdevices
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      opt_debug=optset(1)
      if (optset(2)) opt_verbose=1
      opt_pgask=optset(3)
      opt_pgdevice=optarg(4)
      opt_hpfilter=.true.
      read(optarg(5), *, err=99) opt_hpfreq,opt_hpord
      if (optset(6)) then
        opt_lpfilter=.true.
        read(optarg(6), *, err=99) opt_lpfreq,opt_lpord
      endif
      if (optset(7)) then
        opt_filtout=.true.
        opt_filtoutfile=optarg(7)
      endif
      if (optset(8)) then
        opt_envout=.true.
        opt_envoutfile=optarg(8)
      endif
      read(optarg(9), *, err=99) opt_minvel,opt_maxvel
      if (optset(10)) then
        opt_slowout=.true.
        opt_slowoutfile=optarg(10)
      endif
      opt_inpenv=optset(11)
      opt_sloenv=optset(12)
      opt_pickmode=optset(13)
      read(optarg(14), *, err=99) opt_threshold
      opt_usetitle=optset(15)
      opt_title=optarg(15)
      opt_colmap=optarg(16)
      opt_midoff=optset(17)
c 
      if ((iargc()-lastarg).lt.1) stop 'ERROR: too few filenames'
c 
      para_version=version
c 
      print *,version
      if (opt_usetitle) print *,opt_title
c
c------------------------------------------------------------------------------
c go
c read data
      call readdata(lastarg)
c 
c check input data
      call inpchecks
c 
c transform to frequency domain
      call inpspectra
c 
c apply high pass filter
      if (opt_hpfilter) call hpfilter
c
c make anti alias filter acausal
      if (opt_lpfilter) call lpfilter
c 
c
      if (opt_filtout) then
        comment='traces after filter stage'
        call writedata(opt_filtoutfile, comment)
      endif
c 
c calculate envelope
      if (opt_inpenv) then
        call envelope
c 
c
        if (opt_envout) then
          comment='traces after calculating envelope'
          call writedata(opt_envoutfile, comment)
        endif
      endif
c 
c prepare trace scaling
      call ampscale
c 
c initialize graphics
      pg_maindevice=pgp_open(opt_pgdevice)
      if (pg_maindevice.eq.0) stop 'ERROR: opening graphics device'
      call pgask(opt_pgask)
c 
c
      call scanslow
c
c 
      call pgslowestim
c 
      if (opt_sloenv) then
        call sloenvelope
      endif
c 
c
      if (opt_slowout) then
        comment='slowness traces'
        call writeslow(opt_slowoutfile, comment)
      endif
c 
      call graytrans
      call scantau
      call pggraymap
c
c close graphics
      call pgclos
c
      stop
   99 stop 'ERROR: syntax error in command line option'
      end
c
c ----- END OF sousou.f -----
