c this is <refract_cmdopt.f>
c------------------------------------------------------------------------------
cS
c
c 24/05/2000 by Thomas Forbriger (IfG Stuttgart)
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
c
c read command line options
c
c REVISIONS and CHANGES
c    24/05/2000   V1.0   Thomas Forbriger
c    29/07/2000   V1.1   introduced annotations option
c    11/01/2001   V1.2   introduced plflag_hypoffset
c    18/01/2001   V1.3   corrected braces around units
c    17/06/2003   V1.4   introduced new option plflag_tracenum
c    09/09/2004   V1.5   introduced new option plflag_tracename
c    04/12/2009   V1.6   use correct DIN notation for units
c    26/11/2010   V1.7   provide means to select file formats
c    14/11/2011   V1.8   remember whether minoff is forced
c    12/11/2012   V1.9   new option -Sn
c    13/11/2012   V1.10  new option -Ef and -TL
c    20/11/2012   V1.11  new option -Eu
c    24/10/2013   V1.12  - new option -So
c                        - new option -S3
c    18/11/2013   V1.13  new option -TF
c    21/03/2104 thof:    new option -TR
c    07/12/2015 thof:    new option -SN
c
c==============================================================================
c
      subroutine refract_cmdopt(version, device, lastarg)
c
c declare parameters
c
c version:  refract version string (input)
c device:   selected PGPLOT device (output)
c lastarg:  last argument index returned by tf_cmdline (output)
c 
      character*(*) version, device
      integer lastarg
c 
      include 'refract_dim.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_opt.inc'
      include 'refract_model.inc'
c
cE
c declare local variables
      integer i
c commandline
      integer maxopt
      parameter(maxopt=69)
      character*3 optid(maxopt)
      character*120 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
c   old command line options
      data (optid(i), i=1,15)/'-D','-d','-v','-p','-C','-L',
     &  '-M','-e','-R','-O','--',
     &  '--','--','--','--'/
      data (opthasarg(i), i=1,15)/.FALSE.,.TRUE.,4*.FALSE.,9*.TRUE./
      data (optarg(i),i=1,15)    /'-','x11',4*'-','1','0.',7*'-1.'/
c titles, labels, legends
      data (optid(i),     i=16,22) /'-Tt','-Tx','-Ty','-Tm','-Tl',
     &                              '-Ts','-TM'/
      data (opthasarg(i), i=16,22) /7*.TRUE./
      data (optarg(i),    i=16,22) /3*'-',2*'T','1.','0.,0.'/
      data optid(63)               /'-TL'/
      data opthasarg(63)           /.TRUE./
      data optarg(63)              /'5'/
      data optid(67)               /'-TF'/
      data opthasarg(67)           /.FALSE./
      data optarg(67)              /'-'/
      data optid(68)               /'-TR'/
      data opthasarg(68)           /.FALSE./
      data optarg(68)              /'-'/
c line options and color options
      data (optid(i),     i=23,30) /'-Lw','-Lc','-Lm','-Cb','-Cf',
     &                              '-CW','-Cc','-Cm'/
      data (opthasarg(i), i=23,30) /.TRUE.,.FALSE.,3*.TRUE.,2*.FALSE.,.TRUE./
      data (optarg(i),    i=23,30) /'1','-','4','1.,1.,1.','0.,0.,0.',
     &                              2*'-','5'/
c elements and style options
      data (optid(i),     i=31,39) /'-Eg','-Ev','-Eb','-EP','-ES','-ET',
     &                              '-Ew','-Et','-Ep'/
      data (opthasarg(i), i=31,39) /3*.FALSE.,6*.TRUE./
      data (optarg(i),    i=31,39) /3*'-',6*'T'/
      data optid(58)               /'-En'/
      data opthasarg(58)           /.FALSE./
      data optarg(58)              /'-'/
      data optid(59)               /'-Es'/
      data opthasarg(59)           /.FALSE./
      data optarg(59)              /'-'/
      data optid(62)               /'-Ef'/
      data opthasarg(62)           /.FALSE./
      data optarg(62)              /'-'/
      data optid(64)               /'-Eu'/
      data opthasarg(64)           /.FALSE./
      data optarg(64)              /'-'/
c file reading
      data (optid(i),     i=40,43) /'-Fp','-Fa','-Ft','-Fm'/
      data (opthasarg(i), i=40,43) /4*.TRUE./
      data (optarg(i),    i=40,43) /4*'-NONE-'/
      data optid(60)               /'-ty'/
      data opthasarg(60)           /.TRUE./
      data optarg(60)              /'sff'/
c seismograms scaling
      data (optid(i),     i=44,54) /'-Sx','-St','-Se','-Sa','-Sc','-Sm',
     &                              '-Sr','-Si','-SM','-SO','-SR'/
      data (opthasarg(i), i=44,54) /7*.TRUE.,2*.FALSE.,2*.TRUE./
      data (optarg(i),    i=44,54) /2*'-','0.',2*'-1.','1','3.',2*'-',
     &                              '0.1','-1.'/
      data optid(57)               /'-Sh'/
      data opthasarg(57)           /.FALSE./
      data optarg(57)              /'-'/
      data optid(61)               /'-Sn'/
      data opthasarg(61)           /.FALSE./
      data optarg(61)              /'-'/
      data optid(65)               /'-So'/
      data opthasarg(65)           /.TRUE./
      data optarg(65)              /'0'/
      data optid(66)               /'-S3'/
      data opthasarg(66)           /.TRUE./
      data optarg(66)              /'0.,10.'/
      data optid(69)               /'-SN'/
      data opthasarg(69)           /.TRUE./
      data optarg(69)              /'0.,10.'/
c
c additionals
      data (optid(i),     i=55,56) /'-Lt','-Ta'/
      data (opthasarg(i), i=55,56) /2*.TRUE./
      data (optarg(i),    i=55,56) /'4','NSP'/
c
c------------------------------------------------------------------------------
c go
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
c 
c set defaults before evaluating command line
      call setdefaults
c
c process traditional options
c ===========================
      debug=optset(1)
      device=optarg(2)
      verbose=optset(3)
      flag_pick=optset(4)
      plflag_color=optset(5)
      plflag_linestyle=optset(6)
      if (optset(7)) read (optarg(7), '(i10)') plpar_mode
      if (optset(8)) read (optarg(8), '(f10.3)') plpar_expo
      if (debug) print *,'DEBUG is active'
      read(optarg(9), *) plpar_radius
      if (optset(10)) read(optarg(10), *) plpar_minoff
c 
c process options
c ===============
c 
c line and color options
      read(optarg(23), *) opt_Lwidth
      opt_Lcycle=optset(24)
      read(optarg(25), *) opt_Lmax
      read(optarg(26), *) (opt_Cbgrgb(i),i=1,3)
      read(optarg(27), *) (opt_Cfgrgb(i),i=1,3)
      opt_Cswap=optset(28)
      opt_Ccycle=optset(29)
      read(optarg(30), *) opt_Cmax
      read(optarg(55), *) opt_Lttwidth
c
c elements and style
      opt_Egrid=optset(31)
      opt_Ewiggle=optset(32)
      opt_Ebubble=optset(33)
      read(optarg(34), *) opt_ECpicks
      read(optarg(35), *) opt_ECwave
      read(optarg(36), *) opt_ECtt
      read(optarg(37), *) opt_Ewave
      read(optarg(38), *) opt_Ett
      read(optarg(39), *) opt_Epicks
      plflag_tracenum=optset(58)
      plflag_tracename=optset(59)
      elem_vpframe=optset(62)
      plflag_subscale=optset(64)
c 
c file reading
      opt_Fpicks=optarg(40)      
      opt_Farrival=optarg(41)      
      opt_Ftaper=optarg(42)      
      opt_Fmodel=optarg(43)      
      opt_Fformat=optarg(60)
c
c seismogram scaling
      opt_Sxrange=optset(44)
      if (opt_Sxrange) read(optarg(44), *) opt_Sxmin,opt_Sxmax
      opt_Strange=optset(45)
      if (opt_Strange) read(optarg(45), *) opt_Stmin,opt_Stmax
      read(optarg(46), *) opt_Sexp
      read(optarg(47), *) opt_Samp
      read(optarg(48), *) opt_Sclip
      read(optarg(49), *) opt_Smode
      opt_Sreduce=optset(50)
      read(optarg(50), *) opt_Svel
      opt_Sinv=optset(51)
      opt_Savg=optset(52)
      read(optarg(53), *) opt_Sminoff
      read(optarg(54), *) opt_Sradius
      plflag_hypoffset=optset(57)
      opt_Sosnoreduce=optset(61)
      read(optarg(65), *) opt_Sordinate
      if ((opt_Sordinate.lt.0).or.(opt_Sordinate.gt.3))
     &  stop 'ERROR: argument to -So is out of range'
      opt_Savgref=optset(66).or.optset(69)
      read(optarg(69), *), opt_Savgrefxmin, opt_Savgrefxmax
      if (optset(66)) 
     &  read(optarg(66), *), opt_Savgrefxmin, opt_Savgrefxmax
      if ((opt_Savgrefxmin.ge.opt_Savgrefxmax)
     &    .or.(opt_Savgrefxmin.lt.0.)
     &    .or.(opt_Savgrefxmax.lt.0.))
     &  stop 'ERROR: inappropriate values of argument to -S3'
c 
c titles, label, legends
      opt_Ttitle=version
      opt_Txlabel='time / s'
      if (opt_Sordinate.eq.0) then
        if (opt_Sradius.gt.0.) then
          opt_Tylabel='offset / ^'
        else
          opt_Tylabel='offset / m'
        endif
      else
        opt_Tylabel='x? coordinate / m'
        write (opt_Tylabel(2:2), '(i1)') opt_Sordinate
      endif
      if (optset(16)) opt_Ttitle=optarg(16)
      if (optset(17)) opt_Txlabel=optarg(17)
      if (optset(18)) opt_Tylabel=optarg(18)
      opt_Tannotate=optarg(56)
      read(optarg(19), *) opt_Tmode
      read(optarg(20), *) opt_Tlegend
      read(optarg(21), *) opt_Tscale
      opt_Tmodel=optset(22)
      read(optarg(22), *) opt_Tmodt, opt_Tmodx
      read(optarg(63), *) pg_nam_maxlines
      if (pg_nam_maxlines.lt.1) 
     &  stop 'ERROR: at least one line must be provided for file names'
      opt_Tfilename=optset(67)
      opt_Treverselegend=optset(68)
c
c override traditional options
c ============================
c
      if (optset(24)) plflag_linestyle=opt_Lcycle
      if (optset(29)) plflag_color=opt_Ccycle
      if (optset(46)) plpar_expo=max(0.,opt_Sexp)
      plpar_forceminoff=optset(53)
      if (plpar_forceminoff) plpar_minoff=opt_Sminoff
      if (optset(54)) plpar_radius=opt_Sradius
      if (optset(66)) opt_Smode=3
      plpar_mode=opt_Smode
      print *,plpar_mode
c
c evaluate options as far as possible
c ===================================
c
      pg_title=opt_Ttitle
      pg_xlabel=opt_Txlabel
      pg_ylabel=opt_Tylabel
      elem_filenames=opt_Tlegend
      elem_modbox=opt_Tmodel
      elem_annot=opt_Tmode
      pg_std_ch=opt_Tscale
c
      plpar_remav=opt_Savg
      plflag_invers=opt_Sinv
      plflag_reduce=opt_Sreduce
      plflag_osnoreduce=opt_Sosnoreduce
      plpar_vred=opt_Svel
c
      plpar_avgrefxmin=opt_Savgrefxmin
      plpar_avgrefxmax=opt_Savgrefxmax
c 
      plflag_grid=opt_Egrid
      plflag_vara=opt_Ewiggle
      plflag_bubbles=opt_Ebubble
      plflag_seistyle=opt_ECwave
      plflag_ttstyle=opt_ECtt
      elem_data=opt_Ewave
      elem_syntt=opt_Ett
      elem_picks=opt_Epicks
c
      plpar_colcyc=opt_Cmax
      plpar_lscyc=opt_Lmax
      pg_std_lw=opt_Lwidth
      do i=1,3
        if (opt_Cswap) then
          pg_std_bgrgb(i)=opt_Cfgrgb(i)
          pg_std_fgrgb(i)=opt_Cbgrgb(i)
        else
          pg_std_fgrgb(i)=opt_Cfgrgb(i)
          pg_std_bgrgb(i)=opt_Cbgrgb(i)
        endif
      enddo
      mod_boxx=opt_Tmodt
      mod_boxy=opt_Tmodx
      pg_syntt_lw=opt_Lttwidth
c
      return
      end
c
c ----- END OF refract_cmdopt.f -----
