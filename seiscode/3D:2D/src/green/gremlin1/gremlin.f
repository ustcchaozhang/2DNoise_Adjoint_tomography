c this is <gremlin.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c GREens Matrix Linearized INversion
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
c    05/12/97   V1.0   Thomas Forbriger
c    09/12/97   V1.1   first somehow running version
c    10/12/97   V1.2   - apply datamode and logstretch changes
c                      - use direct acces to glq common blocks now
c                      - no sub_parset anymore
c    12/12/97   V1.3   - included parameters for pg_inv
c                      - provides now inversion steps
c                      - new inversion modus
c    18/12/97   V1.4   - new auto function
c  ------------------    old routines are now in gremlinold.f
c    22/12/97   V2.0   - new concept
c    29/12/97   V2.1   - moved verbosity control to glq_parset.f
c                      - some new features (like sano,...)
c                      - added invmode
c    05/01/98   V2.2   - add new feature pg_x2
c    07/01/98   V2.3   - add a feature to take catch a given nu(para)
c    16/01/98   V2.4   - check linearized development of X2(nu)
c    20/01/98   V3.0   - search range and slowness version
c                      - add fixed step
c               V3.1   - return to velocity and Q
c                      - new npa
c    23/01/98   V3.2   - avoid unintentional program termination by using
c                        "term" to exit the main program
c                      - added manual model editing 
c    02/02/98   V3.3   - color plot options
c    25/03/98   V3.3a  - use calls to BLOCKDATA subroutines to force the
c                        linker to include them
c                      _ give more detailed help info
c               V3.4   - now able to write response file
c    06/04/98   V3.5   - new use new default setting routines
c    07/04/98   V3.6   - introduced file writing and reading for 
c                        model parameter weights
c                      - introduced resolution analysis
c    17/04/98   V3.7   calculate reference synthetics before calling inv_part
c    20/04/98   V3.8   now coming with sense
c    17/08/98   V3.9   different line styles available
c    20/08/98   V3.10  - new meaning of logstretch
c                      - more help-texts
c    02/12/98   V3.11  - called dat_dcpc in dat_dmode before calculating
c                        data_maxamp
c                      - there was an error in dat_dctt (see there)
c                      - travel time synthetics break down with decreasing
c                        p-velocity
c    03/12/98   V3.12  - added handling of low-velocity channels in
c                        dat_ctt and dat_dctt
c    11/12/98   V3.13  - allow line width setting
c    13/01/99   V4.0   - major changes to meaning of polynomial coefficients:
c                        coefficients refer now to mid of section, section
c                        depth refers to bottom of section and chop_hs is
c                        not used anymore
c                        see glq_model.inc for details and a list of changed
c                        subroutines
c    14/01/99   V4.1   - removed link to old gremlin code
c    20/01/99   V4.2   - add data plotting functions
c    24/01/99   V4.3   - changed colour initialization
c                      - call vim directly from fortran subroutine with
c                        function med
c    02/03/99          - use background rgb
c    03/03/99          - ask for comment when saving file
c    04/03/99   V4.4   - allow reading of second travel time data file
c    01/07/99   V4.5   - added pg monitor devices
c    02/07/99   V4.6   - allow reading of green weights
c    09/04/00   V4.7   - added some data modifying functionality
c    10/04/00   V4.8   introduced model_interval and did update documentation
c    24/05/00   V4.9   - introduced reso option wgpd
c                      - introduced reso option wense
c    25/05/00   V4.10  changed dat_fcamp to calculate weighted least-squares
c    02/06/00   V4.11  hat to correct follow reporting schemem in
c                      mod_showparcor
c    02/06/00   V4.12  added mweight condition in res_opt
c    19/04/02   V4.13  support square error ense-test by command swense
c    03/05/02   V4.13b only test selected parameters in resolution analysis
c    06/05/02   V4.13c - allow for sqr and rms scaling in sense and ense
c                      - allow to use preexisting partial derivatives
c    07/05/02   V4.14  new data weight routines
c    04/07/02   V4.15  support offset removal for mseisfk
c    07/08/02   V4.15a - found some old commands ;-)
c
c==============================================================================
c
      program gremlin
c
      character*79 version
      parameter(version=
     &  'GREMLIN   V4.15a  GREens Matrix Linearized INversion')
c common blocks
      include 'libs/glq_dim.inc'
      include 'libs/glq_para.inc'
      include 'libs/glq_pgpara.inc'
      include 'libs/glq_invpara.inc'
      include 'gremlin.inc'
c
c program loop
      logical hot, subs_exists
      integer ival
      real numin, numax, rval, rval2, rval3, rval4
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=13)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c misc
      integer doubles
      real megabytes
c here are the keys to our commandline options
      data optid/2h-D,2h-M,2h-G,2h-T,2h-P,2h-d,2h-A,2h-B,2h-a,2h-b,2h-l,
     &   2h-L,2h-t/
      data opthasarg/.FALSE.,12*.true./
      data optarg/1h-,1h-,1h-,1h-,1h-,3hx11,1h3,1h5,1h1,1h1,1h1,2h20,1h-/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: gremlin [-M file] [-G file] [-T file] [-P file]',
     &        ' [-d device] [-t file] [-A ci] [-B ci] [-a ls] [-b ls]',
     &        ' [-l lw] [-L lw]'
      print *,'   or: gremlin -help'
c
c      if (iargc().lt.1) stop 'ERROR: missing arguments'
      if (iargc().eq.1) then
        call getarg(1, argument)
        if (argument(1:5).eq.'-help') then
          print *,' '
          print *,'Sorry, that''s a monolith.'
          print *,' '
          print *,'This program will do anything you want to perform'
          print *,'linearized least squares inversions for the green.'
          print *,' '
          print *,'-G file    preload greens data from file'
          print *,'-M file    preload polynomial start model from file'
          print *,'-T file    preload travel time data from file'
          print *,'-P file    preload parameters from file'
          print *,'-t file    preload second travel time data for sections'
          print *,'           below asphalt'
          print *,'-d device  graphics output device to be used'
          print *,'-A ci      set alpha color index'
          print *,'-B ci      set beta color index'
          print *,'-a ls      set alpha line style'
          print *,'-b ls      set beta line style'
          print *,'-l lw      set line width'
          print *,'-L lw      set line width of offset range limit'
          print *,' '
          call pgp_showdevices
          print *,' '
          print *,'HOWTO use:'
          call gremhelp_main
          print *,' '
          print *,'This version is compiled for:'
          print *,'  number of polynomial model sections:       ',glqm_msec
          print *,'  number of free parameters:                 ',glqm_mano
          print *,'  number of discrete model layers:           ',glqm_mlay
          print *,'  number of slowness values:                 ',glqd_mslo
          print *,'  number of frequency values:                ',glqd_mfre
          print *,'  number of travel-time samples:             ',glqd_mtts
          print *,'  resulting number of anonymous data points: ',glqd_mano
          print *,'  number of history steps to remember:       ',glqm_mpts
          doubles=(glqd_mfre*glqd_mslo*4+
     &            2*glqd_mano*glqm_mano)*2
          megabytes=(8.*doubles*1.e-6)
          print *,' '
          write(6, '(af6.2a)') '   A rough estimate results in ',megabytes,
     &      'MByte of RAM that will be used!'
          print *,' '
          call par_showindex
          print *,' '
          call par_showmodes
          stop
        endif
      endif
c
c------------------------------------------------------------------------------
c some defaults
      doask=.false.
c
c set defaults
      call par_setdefinv
      call par_setdefpg
      call par_setdefverb
      call par_setdefpgmon
      call dat_mscprep(.false.)
c
c read command line arguments
c
      if (iargc().gt.0) then
        call tf_cmdline(1, lastarg, maxopt, optid,
     &                  optarg, optset, opthasarg)
        debug=optset(1)
        call par_setdebug(debug)
        if (optset(2)) call mod_read(optarg(2), 1)
        if (optset(3)) call dat_rgreen(optarg(3))
        if (optset(4)) call dat_rtt(optarg(4))
        if (optset(5)) call par_read(optarg(5))
        if (optset(13)) call dat_mtt(optarg(13))
      endif
      debug=optset(1)
      call par_setdebug(debug)
      device=optarg(6)
      read(optarg(7), *) pg_alphacol
      read(optarg(7), *) pg_alpharcol
      read(optarg(8), *) pg_betacol
      read(optarg(8), *) pg_betarcol
      read(optarg(9), *) pg_alphals
      read(optarg(10), *) pg_betals
      read(optarg(11), *) pg_lw
      read(optarg(11), *) pg_clw
      read(optarg(12), *) pg_bestlw
      pg_alpharcol=pg_alphacol+1
      pg_betarcol=pg_betacol+1
c 
c initialize rgb set
c      pg_rgbtable(1,0)=1.
c      pg_rgbtable(2,0)=1.
c      pg_rgbtable(3,0)=1.
c 
c      pg_rgbtable(1,1)=0.
c      pg_rgbtable(2,1)=0.
c      pg_rgbtable(3,1)=0.
c 
c      pg_rgbtable(1,2)=.5
c      pg_rgbtable(2,2)=.5
c      pg_rgbtable(3,2)=.5
c 
c      pg_rgbtable(1,3)=1.
c      pg_rgbtable(2,3)=0.
c      pg_rgbtable(3,3)=0.
c 
c      pg_rgbtable(1,4)=0.6
c      pg_rgbtable(2,4)=0.4
c      pg_rgbtable(3,4)=0.4
c 
c      pg_rgbtable(1,5)=1.
c      pg_rgbtable(2,5)=1.
c      pg_rgbtable(3,5)=0.
c 
c      pg_rgbtable(1,6)=.6
c      pg_rgbtable(2,6)=.4
c      pg_rgbtable(3,6)=.0
c
c------------------------------------------------------------------------------
c go
c
      call pgp_setdevice(device, 1, 1)
      call par_pgapply
      call pgask(doask)
      hot=.true.
      do while (hot)
        print *,' '
        print *,'your command:'
        read(5, '(a80)') argument
        write(6, '(1x,1h(,a76,1h))') argument
c 
        if (argument(1:5).eq.'reso ') then
          call sub_resan(version)
        elseif (argument(1:5).eq.'file ') then
          call sub_files(version)
        elseif (argument(1:4).eq.'imo ') then
          call sub_invmode(argument(5:80))
        elseif (argument(1:4).eq.'dev ') then
          call pgend
          device=argument(5:80)
          call pgp_setdevice(device, 1, 1)
          call pgask(doask)
          call par_pgapply
c        elseif (argument(1:4).eq.'old ') then
c          print *,'You now enter ancient and unmaintained code'
c          print *,'You do this on YOUR OWN RISK!'
c          call oldgremlin
        elseif (argument(1:4).eq.'mon ') then
          call par_pgmon
        elseif (argument(1:5).eq.'verb ') then
          call par_verbosity
        elseif (argument(1:7).eq.'weight ') then
          read(argument(8:80), *, err=98) ival
          if (((ival.ge.0).and.(ival.le.3)).or.(ival.ge.11))
     &      call mod_weight(ival)
        elseif (argument(1:5).eq.'sano ') then
          read(argument(6:80), *, err=98) ival
          ival=min(7,max(0,ival))
          call par_sano(ival, mb_ref)
        elseif (argument(1:4).eq.'opt ') then
          read(argument(5:80), *, err=98, end=98) lq_x2lim, lq_msteps
          lq_msteps=max(1,lq_msteps)
          call pg_opt
          call sub_beep
        elseif (argument(1:4).eq.'ofi ') then
          read(argument(5:80), *, err=98, end=98) 
     &      rval, lq_x2lim, lq_msteps, ival
          lq_msteps=max(1,lq_msteps)
          call pg_invfix(rval, ival)
          call sub_beep
        elseif (argument(1:4).eq.'lx2 ') then
          read(argument(5:80), *, err=98, end=98) numin, numax, ival
          numin=max(1.e-10,numin)
          numax=min(1.e10,max(0.5*numin,numax))
          call pg_linx2(numin, numax, ival)
        elseif (argument(1:4).eq.'x2c ') then
          read(argument(5:80), *, err=98, end=98) numin, numax, ival
          numin=max(1.e-10,numin)
          numax=min(1.e10,max(0.5*numin,numax))
          call pg_x2(numin, numax, ival)
        elseif (argument(1:4).eq.'npa ') then
          read(argument(5:80), *, err=98, end=98) ival
          call sub_nnu(ival)
        elseif (argument(1:4).eq.'dwo ') then
          call mod_write(6, mb_work, version)
        elseif (argument(1:4).eq.'dre ') then
          call mod_write(6, mb_ref, version)
        elseif (argument(1:4).eq.'dmi ') then
          read(argument(5:80), *, err=98, end=98) ival
          ival=max(1,min(ival,glqm_mmod))
          call pgpage
          call pg_mod(ival)
        elseif (argument(1:4).eq.'dgi ') then
          read(argument(5:80), *, err=98, end=98) ival
          ival=max(1,min(ival,glqd_mdat))
          call pgpage
          call pg_green(0,ival)
        elseif (argument(1:4).eq.'dti ') then
          read(argument(5:80), *, err=98, end=98) ival
          ival=max(1,min(ival,glqd_mdat))
          call pgpage
          if ((ival.eq.di_read).or.(ival.eq.di_mread)) then
            call pg_tdata
          elseif (ival.eq.di_mref) then
            call pg_tt(0, .true.)
          else
            call pg_tt(0, .false.)
          endif
        elseif (argument(1:4).eq.'tpa ') then
          read(argument(5:80), *, err=98, end=98) ival
          call sub_tpa(ival)
        elseif (argument(1:4).eq.'dpa ') then
          call par_showmod
          call par_showsrc
          call par_showdat
          call par_showinv
        elseif (argument(1:4).eq.'med ') then
          call sub_med(version)
        elseif (argument(1:4).eq.'dpc ') then
          call par_parcor(.false.)
        elseif (argument(1:4).eq.'tpc ') then
          call mod_showparcor
        elseif (argument(1:6).eq.'pgpar ') then
          call par_pgset
        elseif (argument(1:4).eq.'dpg ') then
          call par_showpg
        elseif (argument(1:4).eq.'spa ') then
          call par_set
        elseif (argument(1:4).eq.'spc ') then
          call par_parcor(.true.)
        elseif (argument(1:4).eq.'dgr ') then
          call pg_gdata
        elseif (argument(1:4).eq.'dtt ') then
          call pg_tdata
        elseif (argument(1:4).eq.'dda ') then
          call pg_fullset(.true.)
        elseif (argument(1:4).eq.'dmo ') then
          if (subs_exists(argument(5:80), .true.)) then
            call mod_read(argument(5:80), mb_work)
            call pg_fullset(.false.)
          endif
        elseif (argument(1:8).eq.'weights ') then
          read(argument(9:80), *, err=98, end=98) 
     &      rval, rval2,rval3,rval4
          call dat_mmweights(rval,rval2,rval3,rval4)
        elseif (argument(1:5).eq.'term ') then
          hot=.false.
        elseif (argument(1:9).eq.'morehelp ') then
          call gremhelp_main
c        elseif (argument(1:5).eq.'exit ') then
c          hot=.false.
c        elseif (argument(1:5).eq.'quit ') then
c          hot=.false.
        elseif (argument(1:5).eq.'help ') then
          print *,'file spc spa opt imo term'
          print *,'dpa dpc tpc dgr dtt dev dmo dda verb'
          print *,'sano weight x2c lx2 tpa npa ofi dre med'
          print *,'reso dwo dgi dti pgpar dmi mon'
          print *,'weights'
          print *,' '
          print *,'extra help: morehelp'
        else
          print *,'NOTICE: unknown command ',argument(1:index(argument, ' ')),
     &    ' (try ''help'')'
        endif
        goto 99
   98   print *,'ERROR: invalid argument string'
   99   continue
c
      enddo
      call pgend
c 
      stop
      end
c
c======================================================================
c some subsets
c
      subroutine sub_files(version)
c
c read and write files
c 
      include 'libs/glq_dim.inc'
c 
      character version*(*)
c 
      character arg*(80), comment*(80)
      logical hot, subs_exists
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *,'FILES - your command:'
        read(5, '(a80)') arg
        write(6, '(1x,1h(,a76,1h))') arg
c 
        if (arg(1:6).eq.'green ') then
          if (subs_exists(arg(7:80), .true.)) call dat_rgreen(arg(7:80))
        elseif (arg(1:5).eq.'wwrt ') then
          if (.not.(subs_exists(arg(6:80), .false.)))
     &      call dat_wtweight(arg(6:80), .true.)
        elseif (arg(1:5).eq.'wwct ') then
          if (.not.(subs_exists(arg(6:80), .false.)))
     &      call dat_wtweight(arg(6:80), .false.)
        elseif (arg(1:5).eq.'wwrg ') then
          if (.not.(subs_exists(arg(6:80), .false.)))
     &      call dat_wgweight(arg(6:80), .true.)
        elseif (arg(1:5).eq.'wwcg ') then
          if (.not.(subs_exists(arg(6:80), .false.)))
     &      call dat_wgweight(arg(6:80), .false.)
        elseif (arg(1:4).eq.'mtt ') then
          if (subs_exists(arg(5:80), .true.)) call dat_mtt(arg(5:80))
        elseif (arg(1:3).eq.'tt ') then
          if (subs_exists(arg(4:80), .true.)) call dat_rtt(arg(4:80))
        elseif (arg(1:7).eq.'gwread ') then
          if (subs_exists(arg(8:80), .true.)) then
            call dat_rweight(arg(8:80))
          endif
        elseif (arg(1:6).eq.'wread ') then
          if (subs_exists(arg(7:80), .true.)) then
            call par_wread(arg(7:80))
            call par_parcor(.false.)
          endif
        elseif (arg(1:6).eq.'model ') then
          if (subs_exists(arg(7:80), .true.)) call mod_read(arg(7:80), mb_ref)
        elseif (arg(1:5).eq.'resp ') then
          if (.not.(subs_exists(arg(6:80), .false.))) call dat_wresp(arg(6:80))
        elseif (arg(1:5).eq.'para ') then
          if (subs_exists(arg(6:80), .true.)) call par_read(arg(6:80))
        elseif (arg(1:6).eq.'wsave ') then
          if (.not.(subs_exists(arg(7:80), .false.)))
     &      call par_wsave(arg(7:80), .true.)
        elseif (arg(1:5).eq.'save ') then
          if (.not.(subs_exists(arg(6:80), .false.))) then
            print *,'comment?'
            read(5, '(a80)') comment
            call mod_save(arg(6:80), mb_ref, .true., comment)
          endif
        elseif (arg(1:5).eq.'help ') then
          call gremhelp_files
        elseif (arg(1:5).eq.'exit ') then
          hot=.false.
        elseif (arg(1:5).eq.'quit ') then
          hot=.false.
        else
          print *,'NOTICE: unknown read command (try ''help'')'
        endif
c 
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine sub_resan(version)
c
c do resolution analysis
c 
      character version*(*)
c
      include 'libs/glq_dim.inc'
      include 'libs/glq_model.inc'
c 
      character arg*(80), bn*80
      logical hot, inv_part, inv_mat, dat_cref
      integer ival, ival2
      real rval,rval2
c 
      hot=.true.
      do while (hot)
        print *,' '
        print *,'RESOLUTION ANALYSIS - your command:'
        read(5, '(a80)') arg
        write(6, '(1x,1h(,a76,1h))') arg
c 
        if (arg(1:7).eq.'parder ') then
          call res_part
        elseif (arg(1:7).eq.'qsense ') then
          read(arg(8:80), *, err=98, end=98) ival,ival2,rval,rval2
          call pg_sresmod(ival, ival2, rval, rval2, .true.)
        elseif (arg(1:8).eq.'qssense ') then
          read(arg(9:80), *, err=98, end=98) ival,ival2,rval,rval2
          call pg_sresmod(ival, ival2, rval, rval2, .false.)
        elseif (arg(1:8).eq.'qsqense ') then
          read(arg(9:80), *, err=98, end=98) ival,ival2,rval,rval2
          call pgpage
          call pg_resmod(ival, ival2, rval, rval2,.false.)
        elseif (arg(1:6).eq.'qense ') then
          read(arg(7:80), *, err=98, end=98) ival,ival2,rval,rval2
          call pgpage
          call pg_resmod(ival, ival2, rval, rval2,.true.)
        elseif (arg(1:6).eq.'sense ') then
          read(arg(7:80), *, err=98, end=98) ival,ival2,rval,rval2
          if (inv_mat()) then
            call pg_sresmod(ival, ival2, rval, rval2, .true.)
          else
            print *,'calculation of partial derivatives failed'
          endif
        elseif (arg(1:7).eq.'ssense ') then
          read(arg(8:80), *, err=98, end=98) ival,ival2,rval,rval2
          if (inv_mat()) then
            call pg_sresmod(ival, ival2, rval, rval2, .false.)
          else
            print *,'calculation of partial derivatives failed'
          endif
        elseif (arg(1:6).eq.'wense ') then
          read(arg(7:80), *, err=98, end=98) ival,ival2,rval,rval2,bn
          if (inv_mat()) then
            call res_wresmod(ival, ival2, rval, rval2, bn, .true.)
          else
            print *,'calculation of partial derivatives failed'
          endif
        elseif (arg(1:7).eq.'swense ') then
          read(arg(8:80), *, err=98, end=98) ival,ival2,rval,rval2,bn
          if (inv_mat()) then
            call res_wresmod(ival, ival2, rval, rval2, bn, .false.)
          else
            print *,'calculation of partial derivatives failed'
          endif
        elseif (arg(1:7).eq.'sqense ') then
          read(arg(8:80), *, err=98, end=98) ival,ival2,rval,rval2
          if (inv_mat()) then
            call pgpage
            call pg_resmod(ival, ival2, rval, rval2,.false.)
          else
            print *,'calculation of partial derivatives failed'
          endif
        elseif (arg(1:5).eq.'ense ') then
          read(arg(6:80), *, err=98, end=98) ival,ival2,rval,rval2
          if (inv_mat()) then
            call pgpage
            call pg_resmod(ival, ival2, rval, rval2,.true.)
          else
            print *,'calculation of partial derivatives failed'
          endif
        elseif (arg(1:5).eq.'orth ') then
          if (dat_cref()) then
            if (inv_part()) then
              call res_orth
            else
              print *,'calculation of partial derivatives failed'
            endif
          else
            print *,'calculation of reference synthetics failed'
          endif
        elseif (arg(1:5).eq.'gpda ') then
          if (dat_cref()) then
            if (inv_part()) then
              call inv_ds
              do ival=1,mod_n
                call pgpage
                call pg_gpart(0, ival)
              enddo
            else
              print *,'calculation of partial derivatives failed'
            endif
          else
            print *,'calculation of reference synthetics failed'
          endif
        elseif (arg(1:5).eq.'wgpd ') then
          if (dat_cref()) then
            if (inv_part()) then
              call inv_ds
              do ival=1,mod_n
                call res_wgpart(arg(6:), ival)
              enddo
            else
              print *,'calculation of partial derivatives failed'
            endif
          else
            print *,'calculation of reference synthetics failed'
          endif
        elseif (arg(1:5).eq.'qgpd ') then
          read(arg(6:80), *, err=98, end=98) ival
          call pgpage
          call pg_gpart(0, ival)
        elseif (arg(1:4).eq.'gpd ') then
          read(arg(5:80), *, err=98, end=98) ival
          if (dat_cref()) then
            if (inv_part()) then
              call inv_ds
              call pgpage
              call pg_gpart(0, ival)
            else
              print *,'calculation of partial derivatives failed'
            endif
          else
            print *,'calculation of reference synthetics failed'
          endif
        elseif (arg(1:5).eq.'tpda ') then
          if (dat_cref()) then
            if (inv_part()) then
              call inv_ds
              do ival=1,mod_n
                call pgpage
                call pg_tpart(0, ival)
              enddo
            else
              print *,'calculation of partial derivatives failed'
            endif
          else
            print *,'calculation of reference synthetics failed'
          endif
        elseif (arg(1:5).eq.'qtpd ') then
          read(arg(6:80), *, err=98, end=98) ival
          call pgpage
          call pg_tpart(0, ival)
        elseif (arg(1:4).eq.'tpd ') then
          read(arg(5:80), *, err=98, end=98) ival
          if (dat_cref()) then
            if (inv_part()) then
              call inv_ds
              call pgpage
              call pg_tpart(0, ival)
            else
              print *,'calculation of partial derivatives failed'
            endif
          else
            print *,'calculation of reference synthetics failed'
          endif
        elseif (arg(1:4).eq.'dpc ') then
          call par_parcor(.false.)
        elseif (arg(1:4).eq.'tpc ') then
          call mod_showparcor
        elseif (arg(1:4).eq.'spa ') then
          call par_set
        elseif (arg(1:4).eq.'spc ') then
          call par_parcor(.true.)
        elseif (arg(1:4).eq.'dgr ') then
          call pg_gdata
        elseif (arg(1:4).eq.'dtt ') then
          call pg_tdata
        elseif (arg(1:4).eq.'dda ') then
          call pg_fullset(.true.)
        elseif (arg(1:5).eq.'help ') then
          call gremhelp_resan
        elseif (arg(1:5).eq.'exit ') then
          hot=.false.
        elseif (arg(1:5).eq.'quit ') then
          hot=.false.
        else
          print *,'NOTICE: unknown read command (try ''help'')'
        endif
        goto 99
   98   print *,'ERROR: invalid argument string'
   99   continue
c 
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine sub_med(version)
c
c edit model manually
c 
      include 'libs/glq_dim.inc'
c 
      character version*(*)
c 
      character*10 editname
      parameter(editname="edit.p.mod")
c 
      call mod_save(editname, mb_ref, .true., version)
      print *,'reference model is saved to ',editname
      print *,"callling editor: 'vim ",editname,"'"
      call system('vim edit.p.mod')
      call mod_read(editname, mb_ref)
      print *,'reference model is read from ',editname
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine sub_invmode(arg)
c
c set inversion mode
c 
      character arg*(*)
c 
      include 'libs/glq_dim.inc'
c 
      integer arglen
c 
      arglen=len(arg)
c 
      if (arg(1:6).eq.'sgrad ') then
        call par_sano(0, mb_ref)
        call par_sano(2, mb_ref)
        call mod_weight(0)
        call mod_weight(1)
        call mod_weight(1)
      elseif (arg(1:5).eq.'pvel ') then
        call par_sano(0, mb_ref)
        call par_sano(1, mb_ref)
        call mod_weight(0)
      else
        print *,'WARNING: unknown inversion mode: ',
     &    arg(1:index(arg, ' '))
        print *,'use:'
        print *,'sgrad pvel'
      endif
c
      call par_parcor(.false.)
c 
      return
      end
c
c======================================================================
c some subroutines
c 
      subroutine sub_beep
c
      print *,''
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine sub_tpa(para)
c
c fetch given nu(para)
c assume that there are valid partial derivatives existing
c
      include 'libs/glq_dim.inc'
c 
      integer para
c
      real fnu, thisnu
      logical result, inv_model
c 
      fnu(para)=10**(-0.1*para)
c 
      thisnu=fnu(para)
c 
      result=inv_model(thisnu)
      if (result) then
        call mod_parcor
        call mod_copy(mb_work, mb_ref)
      else
        print *,'WARNING: ',
     &    'unexpected problems finding model - break'
      endif
c 
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine sub_nnu(para)
c
      integer para
c 
      logical result, inv_mat
c
      result=inv_mat()
      if (result) then
        call sub_tpa(para)
        call pg_fullset(.true.)
      else
        print *,'WARNING: problem when building matrix'
      endif
c 
      return
      end
c 
c----------------------------------------------------------------------
c
      logical function subs_exists(filename, f)
c
      character*(*) filename
      logical result, f
      integer lu
      parameter(lu=11)
c
      if (f) then
        open(lu, file=filename, status='old', err=90)
        close(lu)
        result=.true.
      else
        open(lu, file=filename, status='new', err=91)
        close(lu, status='delete', err=92)
        result=.false.
      endif
c 
      subs_exists=result
      return
c 
   90 print *,'ERROR: file ',filename(1:index(filename, ' ')),
     &        'does not exist'
      result=.false.
      subs_exists=result
      return
c 
   91 print *,'ERROR: file ',filename(1:index(filename, ' ')),
     &        'exists already'
      result=.true.
      subs_exists=result
      return
c 
   92 print *,'WARNING: could not delete testfile ',
     &        filename(1:index(filename, ' '))
      result=.true.
      subs_exists=result
      return
      end
c 
c ----- END OF gremlin.f -----
