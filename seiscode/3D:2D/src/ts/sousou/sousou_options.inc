c this is <sousou_options.inc>
c------------------------------------------------------------------------------
c
c 18/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c PURPOSE
c
c REVISIONS and CHANGES
c    18/11/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      logical opt_debug
      integer opt_verbose
      character opt_pgdevice*80, opt_filtoutfile*80
      integer opt_hpord, opt_lpord
      real opt_hpfreq, opt_lpfreq
      logical opt_hpfilter, opt_lpfilter, opt_pgask, opt_filtout
      logical opt_envout
      character opt_envoutfile*80
      logical opt_slowout
      character opt_slowoutfile*80
      real opt_minvel, opt_maxvel
      logical opt_inpenv, opt_sloenv, opt_pickmode
      real opt_threshold
      character*72 opt_title
      logical opt_usetitle
      character*20 opt_colmap
      logical opt_midoff
      common /options/ opt_debug, opt_verbose, opt_pgdevice,
     & opt_hpord, opt_lpord, opt_hpfreq, opt_lpfreq, opt_hpfilter,
     & opt_lpfilter, opt_pgask, opt_filtout, opt_filtoutfile,
     & opt_envout, opt_envoutfile, opt_minvel, opt_maxvel,
     & opt_slowout, opt_slowoutfile, opt_inpenv, opt_sloenv,
     & opt_pickmode, opt_threshold, opt_title, opt_usetitle, opt_colmap,
     & opt_midoff

      
c
c ----- END OF sousou_options.inc -----
