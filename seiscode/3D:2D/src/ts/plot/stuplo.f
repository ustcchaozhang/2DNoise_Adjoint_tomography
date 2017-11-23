c this is <stuplo.f> by Thomas Forbriger 1996
c
c Copyright 1996, 2010, 2015, 2016 by Thomas Forbriger
c
c This is a simple plotting tool for seismic time series in
c SFF format
c
c If linked against libfapidxx this program is able to read file formats
c other than SFF too (e.g. seife, MiniSEED, GSE, PDAS, ASC, TSOFT,
c SU, ...). The binary executable linked against libfapidxx and
c libdatrwxx commonly is called stuplox.
c
c ----
c stuplo is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c stuplo is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
c
c (sff is the Stuttgart File Format and is defined in sff.doc)
c 
c BE SHURE YOUR COMPUTER USES 4 BYTES OF MEMORY TO STORE
c INTEGER AND REAL VARIABLES 
c (look at the definition of idata and data in the subroutine checkmem)
c
c REVISIONS and CHANGES
c  V1.0   14/11/96   first running version
c  V1.1   15/11/96   still under major construction
c  V1.2   19/11/96   first complete release
c  V1.3   20/11/96   correct labelling of time axis
c  V1.4   26/11/96   added array traceinfile
c  V1.5   03/12/96   changed control output when reading traces
c                    and added keep option
c  V1.6   13/12/96   set correct file status on open command
c  V1.7   07/01/97   corrected timeax-calculation and timeax min/max
c  V1.8   21/01/97   correct nsamp parameter to libstuff V1.07 reading rouintes
c  V1.9   23/01/97   fixed problem with traces of zero amplitude
c  V1.10  03/02/97   * inlcuded chaining for overlay plots
c                    * included scaling factor for y- and x-size
c                    * included y-centering
c  V1.11  09/07/98   introduced time marker plotting
c  V1.12  15/07/98   * do not provide usage information in case of correct
c                      commandline parameters
c                    * introduced verbose flag
c  V1.13  10/02/99   * allow non-black colour for first set
c                    * removed old tflib calls (changed to tf_ calls)
c  V1.14  23/04/99   * linewidth and character height may be set
c                      from command line
c                    * write y-axis numeric labels perpendicular to axis
c                      if desired
c  V1.15  05/11/99   * use correct TBOX option when using full time x-labels
c  V1.16  19/07/01   * descriptive help text for time marker
c  V1.17  09/10/01   * allow long postscript file names
c  V1.18  04/04/02   new options:
c                    -T set title
c                    -X set x label
c                    -S shift time axis
c  V1.19             new options:
c                    -E force pgenv and pglab
c                    -x select time interval
c                    -y select amplitude interval
c  V1.19a 05/04/02   -E character scaling was incomplete
c  V1.19b 17/06/02   longer annotation string
c  V1.20  26/06/02   new option -W
c  V1.21  28/12/03   new skipdata subroutine
c  V1.21a 13/01/04   improved time scale
c  V1.21b 27/01/04   increased title length
c  V1.22  04/03/04   new option -c1
c  V1.23  26/09/05   new option -oS (stops line style cycling)
c  V1.24  29/11/05   provide new option for clean captions
c  V1.25  29/06/07   provide station location in caption
c  V1.26  02/07/07   provide option to set tick intervals
c  V1.27  18/01/08   do not use write statement for annotations
c                    just assign to character variable line
c  V1.28  04/12/09   use correct DIN notation for units
c  V1.29  13/01/11   program is prepared fro libfapidxx interface
c  V1.30  21/04/11   rename data->fdata, maxval->maxsval,
c                    minval->minsval
c                    removed ampfac from sffread
c                    correct determination of extrema
c  V1.31  30/09/11   call sff_close upon closing a data file
c  V1.32  19/07/15   introduce winplot options -ra, -py, -n1, and -n2
c  V1.33  16/11/16   introduce option -st to set time axis relative to
c                    source
c 
c======================================================================
      program stuplo
c 
c variable declaration
c --------------------
c 
c version
      character*77 version, creator
      parameter(version=
     &  'STUPLO  V1.33   plot seismic time series')
      parameter(creator='1996, 2016 by Thomas Forbriger (IfG Stuttgart)')
c parameter definitions
      integer maxsamples, maxselect, lu, maxtraces, maxchain, maxstyle
      parameter(maxsamples=50 000 000)
      parameter(maxtraces=200)
      parameter(maxselect=200)
      parameter(maxchain=10)
      parameter(maxstyle=5)
      parameter(lu=10)
      real capheight, labheight, numheight
      parameter(capheight=1.5, labheight=1.5, numheight=2.)
c chaining of traces
      integer nchain, ichain, npanels, ipanel, istyle
      integer firstic(maxchain), tracesic(maxchain)
c counters
      integer trace, filep, ftrace
c variables
      logical debug, verbose, goahead
      character line*100
      integer ntrim, ntraces, i, n, ierr
c here is the one big array to contain all seismic traces
c and this big array will hold real and integer data together
c the way we do this both real and integer type variables
c must allocate 4 bytes of memory each! 
      real fdata(maxsamples)
      integer idata(maxsamples)
      equivalence (idata, fdata)
c arras to hold information on data
      integer nsamples(maxtraces), firstsample(maxtraces)
      integer traceinfile(maxtraces)
      character*80 filename(maxtraces), firstfree(maxtraces)
      character*80 informat
      character*10 date(maxtraces)
      character*12 time(maxtraces)
      character*5 station(maxtraces)
      character*3 channel(maxtraces)
      character*4 auxid(maxtraces)
      character*6 instype(maxtraces)
      character*1 loccs(maxtraces)
      real locc1(maxtraces), locc2(maxtraces)
      real sectime(maxtraces), dt(maxtraces) 
      real maxsval(maxtraces), average(maxtraces), minsval(maxtraces)
c file reading variables
      character*200 infile
      logical moretraces
      integer srcedate(7)
c using selections
      logical useselect
      logical selection(maxselect)
c plot caption
      character captionsel*20, captionstr*250, unitslabel*40
      character annotation*100
      integer caplen
c marker
      logical optmarker
      real xmark
c plot style
      logical optgrid, optscalex, optscaley, optinline, optabstime
      logical optsrcetime
      logical opttbox, ftoplab, fbotlab, optfixed, optcolor, optcenter
      logical optblack, optvertlab, opttitle, optxlabel, optpgenv
      logical optnoxlabels, optsetxrange, optsetyrange, optwhitepaper
      logical optcapfromfirst, optcyclestyle, optcleancaprect
      real chf_std,chf_xlab,chf_ylab,chf_cap
      real lwf_std,lwf_txt,lwf_cur
      integer lw_std,lw_txt,lw_cur
      real ch_std,ch_xlab,ch_ylab,ch_cap
      character*200 partitle, parxlabel
      real partimeoff, parxmin, parxmax, parymin, parymax
      real majorxticks,minorxticks
      integer nminorxticks,nmajorxticks
c cursor action
      logical optkeep
      integer curresult, pgcurs
      character curchar*2
      real cursx, cursy
c plot range
      real xmaxp, xminp, ymaxp, yminp, xmaxt, xmint, ymaxt, ymint
      real nlabels, Yfac, yamp, ymid, xlfac, xrfac, xamp
      real labelheight,fractop, fracbot, vptop, vpbot, boxratio
      real yvalheight
      logical optpositiveonly
c data manipulation
      logical optremoveaverage
      integer discardn1, discardn2
c box coordinates
      real boxxcoor(4), boxycoor(4)
c variables to do some intermediate calculations
      integer ix1, ix2
      real jx1,jx2,jx3,jx4,jx5
      double precision vx1
c variables related to pgplot
      character*200 device, botlabel
      real timeax(maxsamples)
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=42)
      character*3 optid(maxopt)
      character*200 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/'-d','-D','-g','-c','-u','-s','-i','-a','-t','-f','-A','-k',
     &  '-C','-Y','-R','-L','-z','-m','-v','-N','-l','-h','-V','-X',
     &  '-T','-S','-E','-nT','-x','-y','-W','-Fc','-oS','-wc','-xt',
     &  '-nx','-ty','-py','-ra','-n1','-n2','-st'/
      data opthasarg/.TRUE.,2*.FALSE.,3*.TRUE.,4*.FALSE.,.TRUE.,2*.FALSE.,
     &  3*.TRUE.,.FALSE.,.TRUE.,2*.FALSE.,2*.TRUE.,.FALSE.,3*.TRUE.,
     &  2*.FALSE.,2*.TRUE.,4*.FALSE.,3*.TRUE.,2*.false.,2*.true.,
     &  .FALSE./
      data optarg/'x11',2*'-','fTt',' ','y',4*'-','*',2*'-','1.',2*'0.','-',
     &  '0.',2*'-','1.,1.,1.','1.,1.,1.,1.','-',2*'-','0.',2*'-',
     &  2*'0.,1.',4*'-','0.,0','0,0.','sff','-','-','0','0','-'/
c----------------------------------------------------------------------
c give basic information and help
      line=' '
      if (iargc().eq.1) call getarg(1, line)
c----------------------------------------------------------------------
c give help information
      if (line(1:6).eq.'-xhelp') then
        call sff_help_details
        stop
      elseif ((line(1:5).eq.'-help').or.(iargc().lt.1)) then
        print 80,version
        print *,creator
        print 80,'Usage: stuplo [-d dev] [-g] [-a|-st] [-i] [-t] [-k] [-N]'
        print 80,'              [-Y fac] [-R fac] [-L fac] [-z] [-C]'
        print 80,'              [-c options] [-s x|y|xy] [-u units]'
        print 80,'              [-f] [-A comment] [-m time] [-v] [-V]'
        print 80,'              [-l std,lab,cur] [-h std,xlab,ylab,cap]'
        print 80,'              [-X label] [-T title] [-S sec] [-E]'
        print 80,'              [-nT] [-x f,t] [-y f,t] [-W] [-Fc] [-oS]'
        print 80,'              [-wc] [-xt i,n] [-nx n,i] [-ty f]'
        print 80,'              [-n1 n] [-n2 n] [-ra] [-py]'
        print 80,'              file [list] [nc:] ...'
        print 80,'or:    stuplo -help'
        print 80,'or:    stuplo -xhelp'
        if (iargc().lt.1) stop 'ERROR: missing parameters\n'
        print *,' '
        print *,'STUPLO plots seismic time series that are stored'
        print *,'in SFF format (Stuttgart File Format).'
        print *,' '
        print *,'Its variant stuplox is linked against a library'
        print *,'supporting input of further file formats. A list'
        print *,'of supported formats is given below. Use the option'
        print *,'-xhelp to receive instructions.'
        print *,' '
        print *,'-ty f        select input file format (see below)'
        print *,'-d dev       This option selects the pgplot output'
        print *,'             device. Information on available ouput'
        print *,'             devices is given below.'
        print *,'-v           be verbose'
        print *,'-k           keep program and window active'
        print *,'             This is useful for opening more than one'
        print *,'             pgplot window. Stop action by pressing x'
        print *,'             or the right mouse button.'
        print *,' '
        print *,'markers'
        print *,'-------'
        print *,' '
        print *,'-g           plot grid'
        print *,'-m time      plot a marker at specified time'
        print *,'             ''time'' is fed directly into the PGPLOT'
        print *,'             coordinate system. It has to be given in'
        print *,'             seconds on the abscissa-scale.'
        print *,' '
        print *,'labels'
        print *,'------'
        print *,' '
        print *,'-t           use full time labels for x-axis (tbox style)'
        print *,'             rather than counting seconds'
        print *,'-u units     define label for y-axis'
        print *,'-V           write numeric labels along left or right'
        print *,'             viewport boundary perpendicular to axis'
        print *,'             (vertically orientated) rather than parallel'
        print *,'             to the axis.'
        print *,'-c options   define caption string'
        print *,'             the options string may consist of the'
        print *,'             following letters:'
        print *,'             f   filename'
        print *,'             t   time'
        print *,'             d   date'
        print *,'             n   nsamples'
        print *,'             i   sampling interval'
        print *,'             s   station'
        print *,'             I   instrument'
        print *,'             c   channel'
        print *,'             a   GSE2.0 auxid'
        print *,'             m   minimum and maximum value'
        print *,'             F   first line of FREE block'
        print *,'             T   number of trace'
        print *,'             A   annotation string given with -A option'
        print *,'             L   location of station'
        print *,'             default is: fTt'
        print *,'-wc          print caption on white box, if inside panel'
        print *,'-Fc          create captions only from first trace in panel'
        print *,'-A comment   annotate comment to caption string'
        print *,'-i           plot caption inside box'
        print *,'-T title     use this string instead of the automatically'
        print *,'             generated title'
        print *,'-X label     use this label for the time axis'
        print *,'-nT          use this to ommit any time scale'
        print *,' '
        print *,'scales and ranges'
        print *,'-----------------'
        print *,' '
        print *,'-a           use absolute time scale'
        print *,'-st          adjust time axis to source time'
        print *,'-s x|y|xy    use same scale for all boxes'
        print *,'             x   all x-axis will have the same range'
        print *,'             y   all y-axis will have the same range'
        print *,'             xy  all x-axis and y-axis will have'
        print *,'                 the same range'
        print *,'-Y fac       The calculated maximum amplitude (rule is'
        print *,'             given by -s option) will fill the fraction'
        print *,'             fac of the plot panel (default is fac=1.).'
        print *,'-R fac       The fraction of the seismogram to cut off'
        print *,'             at the end (default fac=0.).'
        print *,'-L fac       The fraction of the seismogram to cut off'
        print *,'             at the beginning (default fac=0.).'
        print *,'-z           Set zero-line always to the center of the'
        print *,'             plot panel.'
        print *,'-S sec       add sec (in seconds) to the time values'
        print *,'-x f,t       set time scale from f to t (s)'
        print *,'-y f,t       set amplitude scale from f to t'
        print *,'-py          display positive values only'
        print *,'-xt i,n      set major tickmark interval of time axis'
        print *,'             to ''i'' and the number minor tick marks'
        print *,'             between major tick marks to ''n'' '
        print *,'-nx n,i      use ''n'' major tick marks with a minor'
        print *,'             tick marks with a minor tick mark'
        print *,'             interval of ''i'' '
        print *,'             This options overrides option -xt'
        print *,'-n1 n        set first sample to plot to n''th sample'
        print *,'             for each input time series'
        print *,'-n2 n        set last sample to plot to n''th sample'
        print *,'             for each input time series'
        print *,'             n=0 means: last sample read from file'
        print *,'-ra          remove average of each input time series'
        print *,' '
        print *,'styles'
        print *,'------'
        print *,' '
        print *,'-W           plot black on white (also on screen)'
        print *,'-C           use different colours instead of different'
        print *,'             linestyles for marking different chains.'
        print *,'-oS          use only one style for all curves (no'
        print *,'             cycling of line style or colour).'
        print *,'-N           use non-black colour (or linestyle) for'
        print *,'             first dataset.'
        print *,'-l std,txt,cur'
        print *,'             The default linewidth for the following'
        print *,'             elements is scaled by the given factor:'
        print *,'             txt   text strings'
        print *,'             cur   time series curve'
        print *,'             std   all others'
        print *,'-h std,xlab,ylab,cap'
        print *,'             The default character height for the following'
        print *,'             elements is scaled by the given factor:'
        print *,'             xlab  y-axis labels'
        print *,'             ylab  x-axis labels'
        print *,'             cap   caption string'
        print *,'             std   all others'
        print *,'-f           used fixed character size'
        print *,'-E           This is a workaround for difficult cases in'
        print *,'             which the -h option does not lead to the'
        print *,'             desired result. This option forces stuplo'
        print *,'             to use a single panel and pgenv and pglab.'
        print *,' '
        print *,'Each datafile name may be followed by a list of'
        print *,'traces. This list selects a range of traces in'
        print *,'the file which will be processed. This list may'
        print *,'contain no blank (which is the separator to the'
        print *,'next filname). The traces will always be processed'
        print *,'in the order they appear in the data file.'
        print *,' '
        print *,'Examples:'
        print *,'  t:2           will select only trace 2'
        print *,'  t:4-6,2,4     will select traces 2, 4, 5 and 6'
        print *,'  t:9,8,10,14   will select traces 8, 9, 10 and 14'
        print *,' '
        print *,'For comparison purposes it is usefull to do overlay plots.'
        print *,'This may be achieved be building severl chains of seismic'
        print *,'traces. To start a new chain in the list of files use'
        print *,'''nc:'' as a separator. Using this feature the first'
        print *,'panel will contain all first files of each chain, the'
        print *,'second one all second files, etc. Each trace will be plotted'
        print *,'with a different linestyle within each panel. Using the'
        print *,'option -C there will be a different color for each trace.'
        print *,' '
        print *,'Example for building two chains of different length with'
        print *,'the datafiles ''data1'' and ''data2'':'
        print *,' '
        print *,'    stuplo data1 t:1,2 data2 t:5 nc: data1 t:4-7'
        print *,' '
        print *,'  This is what the plotting panels will contain:'
        print *,' '
        print *,'    panel  file.trace'
        print *,'        1  data1.1 data1.4'
        print *,'        2  data1.2 data1.5'
        print *,'        3  data2.5 data1.6'
        print *,'        4  data1.7'
        print *,' '
        call pgp_showdevices
        print *,' '
        print *,'Compiled array dimensions are:'
        print *,'                 total number of samples:',maxsamples
        print *,'                 (all traces together)'
        print *,'      maximum number of traces to select:',maxselect
        print *,'        maximum number of traces to hold:',maxtraces
        print *,'       maximum number of chains to build:',maxchain
        print *,' maximum number of different plot styles:',maxstyle
        print *,' '
        call sff_help_formats
        stop
      endif
c----------------------------------------------------------------------
c set standard options
      device='x11'
      captionsel='fTt'
      unitslabel=' '
      optinline=.false.
      optscalex=.false.
      optscaley=.false.
      optabstime=.false.
      opttbox=.false.
c first read the commandline
      call tf_cmdline(1, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      if (optset(1)) device=optarg(1)
      debug=optset(2)
      optgrid=optset(3)
      if (optset(4)) captionsel=optarg(4)(1:len(captionsel))
      if (optset(5)) unitslabel=optarg(5)(1:len(unitslabel))
      if (optset(6)) then
        if (index(optarg(6),'x').gt.0) optscalex=.true.
        if (index(optarg(6),'y').gt.0) optscaley=.true.
      endif
      optinline=optset(7)
      optabstime=optset(8)
      opttbox=optset(9)
      optfixed=optset(10)
      annotation=optarg(11)(1:len(annotation))
      optkeep=optset(12)
      optcolor=optset(13)
      read(optarg(14), *, end=96, err=97) Yfac
      if (Yfac.lt.0.01) then
        print *,'WARNING: Y-fac is set to 1. (was too small: ',Yfac,')'
        Yfac=1.
      endif
      read(optarg(15), *, end=96, err=97) xrfac
      read(optarg(16), *, end=96, err=97) xlfac
      optcenter=optset(17)
      if ((xrfac+xlfac).ge.1.) 
     &  stop 'ERROR: check setting of -R and -L (nothing will be left)'
      if (debug) print *,'DEBUG: debug messages are switched on'
      if (debug) call checkmem(idata, fdata, maxsamples)
      optmarker=optset(18)
      if (optmarker) read(optarg(18), *, end=96, err=97) xmark
      verbose=optset(19)
      optblack=optset(20)
      read(optarg(21), *, end=96, err=97) lwf_std,lwf_txt,lwf_cur
      read(optarg(22), *, end=96, err=97) 
     &  chf_std,chf_xlab,chf_ylab,chf_cap
      optvertlab=optset(23)
      optxlabel=optset(24)
      parxlabel=optarg(24)
      opttitle=optset(25)
      partitle=optarg(25)
      read(optarg(26), *, end=96, err=97) partimeoff
      optpgenv=optset(27)
      optnoxlabels=optset(28)
      optsetxrange=optset(29)
      read(optarg(29), *, end=96, err=97) parxmin,parxmax
      optsetyrange=optset(30)
      read(optarg(30), *, end=96, err=97) parymin,parymax
      optwhitepaper=optset(31)
      optcapfromfirst=optset(32)
      optcyclestyle=(.not.optset(33))
      optcleancaprect=optset(34)
      read(optarg(35), *, end=96, err=97) majorxticks, nminorxticks
      read(optarg(36), *, end=96, err=97) nmajorxticks, minorxticks
      informat=optarg(37)(1:len(informat))
      optpositiveonly=optset(38)
      optremoveaverage=optset(39)
      read(optarg(40), *, end=96, err=97) discardn1
      read(optarg(41), *, end=96, err=97) discardn2
c      device='/krm3'
      optsrcetime=optset(42)
c----------------------------------------------------------------------
c initialize
      do ichain=1,maxchain
        firstic(ichain)=-1
        tracesic(ichain)=0
      enddo
c----------------------------------------------------------------------
c 
c enter main program body
c -----------------------
c
c first of all read in the time series
c
      trace=0
      nchain=1
      moretraces=.false.
      filep=lastarg+1
      goahead=.true.
      if (iargc().lt.filep) stop 'ERROR: missing data file\n'
c start file loop
    1 continue
c open new file 
        call getarg(filep, infile)
c is there a new chain to start
        if (infile(1:3).eq.'nc:') then
          nchain=nchain+1
          if (nchain.gt.maxchain) stop 'ERROR: opened too many chains'
          goto 4
        endif
        ntrim=index(infile,' ')-1
        if (verbose) print 81,'  opening data file ',infile(1:ntrim)
        call sff_select_input_format(informat, ierr)
        if (ierr.ne.0) stop 'ERROR: selecting input file format'
        call sffopen(lu, filep, useselect, selection, maxselect, 
     &    srcedate, debug)
        ftrace=0
c start trace loop
    2   continue
          ftrace=ftrace+1
          if (((.not.(useselect)).or.(selection(ftrace))).and.(goahead)) then
            if (trace.lt.maxtraces) then
              trace=trace+1
              if (firstic(nchain).lt.0) then
c start a new chain with this trace
                firstic(nchain)=trace
              endif
              tracesic(nchain)=tracesic(nchain)+1
              traceinfile(trace)=ftrace
              if (verbose) print 82,'    reading trace no.',ftrace
              call sffread(lu, infile, trace, moretraces, debug,
     &                     maxsamples, maxtraces, filename, dt, timeax,
     &                     firstsample, nsamples, firstfree, date,
     &                     time, sectime, station, channel, auxid,
     &                     instype, fdata, idata, maxsval, average, minsval,
     &                     optabstime, verbose, partimeoff,
     &                     loccs, locc1, locc2, optsrcetime, srcedate)
              if (debug) then
                print *,'DEBUG: dataset parameters:'
                write(6,'(" DEBUG:",6(2h >,a,1h<))')
     &            date(trace),time(trace),station(trace),
     &            channel(trace),auxid(trace),instype(trace)
                print *,'DEBUG: dt:',dt(trace),' sectime:',sectime(trace)
                print *,'DEBUG: firstfree: ',firstfree(trace)
                print *,'DEBUG: file name: ',filename(trace)
                print *,'DEBUG: first sample:',firstsample(trace),
     &                  ' last sample:',(firstsample(trace)-1+
     &                                   nsamples(trace)),
     &                  ' samples:',nsamples(trace)
                print *,'DEBUG: maxval:',maxsval(trace),
     &                  ' minval:',minsval(trace),
     &                  ' average:',average(trace)
              endif
            else
              print *,'WARNING: reached limit of',maxtraces,' traces'
              goahead=.false.
            endif
          else
            if (verbose) print 82,'    skipping trace no.',ftrace
            call skipdata(lu, moretraces)
          endif
          if (moretraces) goto 2
c close data file
        if (verbose) print 81,'  closing data file ',infile(1:ntrim)
        call sff_close(lu, ierr)
        if (ierr.ne.0) goto 98
    4   filep=filep+1
        if (filep.le.iargc()) goto 1
      ntraces=trace
c----------------------------------------------------------------------
c maximum number of plot panels will be:
      npanels=0
      do ichain=1,nchain
        npanels=max(npanels,tracesic(ichain))
      enddo
c----------------------------------------------------------------------
c all traces are read
c----------------------------------------------------------------------
c
c prepare data for plotting now
c =============================
c
c manipulate data
c ---------------
c
      if (optremoveaverage.or.(discardn1.gt.0).or.(discardn2.gt.0)) then
        do trace=1,ntraces
c discard samples if requested
c   all samples up to and including discardn1
c   and all samples from and including discard2
c   will be discarded
c   limits are adjusted to make sure that at least two samples remain
          if ((discardn1.gt.0).or.(discardn2.gt.0)) then
c   ix1: first sample in remaining section
c   ix2: last sample in remaining section
            ix1=1
            ix2=nsamples(trace)
            if (discardn1.gt.0) then
              ix1=min(discardn1,nsamples(trace)-1)
            endif
            if (discardn2.gt.0) then
              ix2=min(discardn2,nsamples(trace))
              ix2=max(ix1+1,ix2)
            endif
c adjust data pointers accordingly
            firstsample(trace) = firstsample(trace) + ix1 - 1
            nsamples(trace) = ix2 - ix1 + 1
          endif
c recalculate average
          vx1=0.
          do i=0,nsamples(trace)-1
            vx1 = vx1 + fdata(firstsample(trace)+i)
          enddo
          vx1 = vx1 / nsamples(trace)
          average(trace) = sngl(vx1)
          if (optremoveaverage) then
            do i=0,nsamples(trace)-1
              fdata(firstsample(trace)+i)=
     &          fdata(firstsample(trace)+i) - sngl(vx1)
            enddo
            maxsval(trace) = maxsval(trace) - sngl(vx1)
            minsval(trace) = minsval(trace) - sngl(vx1)
          endif
        enddo
      endif
c
c calculate axis range
c --------------------
c
      xminp=timeax(firstsample(1))
      xmaxp=timeax(firstsample(1)-1+nsamples(1))
      yminp=minsval(1)
      ymaxp=maxsval(1)
      do trace=1,ntraces
        xminp=min(xminp,timeax(firstsample(trace)))
        xmaxp=max(xmaxp,timeax(firstsample(trace)-1+nsamples(trace)))
        yminp=min(yminp,minsval(trace))
        ymaxp=max(ymaxp,maxsval(trace))
      enddo
c 
c determine fractioning of surface
c --------------------------------
c 
c the way of labeling:
c
c   sx: optscalex
c   il: optinline
c   cp: caption
c   nu: numeric labels
c   la: bottom label
c
c   sx    il       cp        nu        la
c
c    x     x        -      last      last
c    -     x        -       all       all
c    x     -      all       all      last
c    -     -      all       all       all
c
c we have to check headers and scale labels
      if ((optscalex).and.(optinline)) then
c no header and only one scale label line at the bottom
        nlabels=numheight+labheight
      elseif (optinline) then
c each trace has a scale label and the last one has a label at the bottom
        nlabels=npanels*(numheight+labheight)
      elseif (optscalex) then
c each trace has a caption and numeric labels and
c the last one has a label at the bottom
        nlabels=npanels*(capheight+numheight)+labheight
      else
c each trace has caption, numeric labels and bottom label
        nlabels=npanels*(capheight+numheight+labheight)
      endif
      boxratio=40./npanels
      labelheight=1./(nlabels+npanels*boxratio)
      yvalheight=labelheight*20.
      if (optfixed) then
        labelheight=1./40.
        boxratio=((1/labelheight)-nlabels)/npanels
      endif
c no viewport up to now --> set last viewport value to top
      vpbot=0.99
c 
c----------------------------------------------------------------------
c plot style
c   line width values had to be read into floating point variables
      lw_txt=int(lwf_txt)
      lw_cur=int(lwf_cur)
      lw_std=int(lwf_std)
c   character height
      if (optpgenv) then
        ch_std=chf_std
        ch_xlab=chf_xlab
        ch_ylab=chf_ylab
        ch_cap=chf_cap
      else
        ch_std=chf_std*labelheight*30.
        ch_xlab=chf_xlab*labelheight*30.
        ch_ylab=chf_ylab*yvalheight
        ch_cap=chf_cap*labelheight*30.
      endif
c
c----------------------------------------------------------------------
c 
c do plot
c
      call pgp_setdevice(device,1,1)
      call pgask(.false.)
      call pgscr(3, 0.,0.,1.)
      call pgscr(4, 0.,1.,0.)
      call pgslw(lw_std)
      call pgsch(ch_std)
c 
c prepare background
c ------------------
c
      if (optwhitepaper) then
        call pgscr(0, 1.,1.,1.)
        call pgscr(1, 0.,0.,0.)
      endif
c
      do ipanel=1,npanels
c
c build caption string
c
        caplen=1
        call sff_TrimLen(captionsel,n)
        do ichain=1,nchain
c is there a trace to plot in this chain?
          if (ipanel.gt.tracesic(ichain)) goto 5
c or skip if only first is selcted
          if ((ichain.ne.1).and.(optcapfromfirst)) goto 5
            trace=firstic(ichain)-1+ipanel
            do i=1,n
              if (captionsel(i:i).eq.'f') then
                line=filename(trace)
              elseif(captionsel(i:i).eq.'t') then
                line=time(trace)
                if (debug) print *,'DEBUG: time ',time(trace)
              elseif(captionsel(i:i).eq.'d') then
                line=date(trace)
              elseif(captionsel(i:i).eq.'T') then
                write(line,'("(",i2,")")') traceinfile(trace)
              elseif(captionsel(i:i).eq.'n') then
                write(line,'("n=",i8)') nsamples(trace)
                if (debug) print *,'DEBUG: nsamples ',nsamples(trace)
              elseif(captionsel(i:i).eq.'s') then
                line=station(trace)
              elseif(captionsel(i:i).eq.'i') then
                write(line,'("dt=",e12.3)') dt(trace)
              elseif(captionsel(i:i).eq.'I') then
                line=instype(trace)
              elseif(captionsel(i:i).eq.'c') then
                line=channel(trace)
              elseif(captionsel(i:i).eq.'a') then
                line=auxid(trace)
              elseif(captionsel(i:i).eq.'A') then
                line=annotation
              elseif(captionsel(i:i).eq.'L') then
                if (loccs(trace).eq.'S') then
                  write(line,'(f8.3,"°N; ",f8.3,"°E")') 
     &              locc1(trace), locc2(trace)
                elseif (loccs(trace).eq.'C') then
                  write(line,'("x="f8.3,"m; y=",f8.3,"m")') 
     &              locc1(trace), locc2(trace)
                else
                  line='no location'
                endif
              elseif(captionsel(i:i).eq.'m') then
                write(line,'("min/max:",e12.3,"/",e12.3)') 
     &          minsval(trace),maxsval(trace)
              elseif(captionsel(i:i).eq.'F') then
                line=firstfree(trace)
              else
                print *,'WARNING: unknown caption character: ',captionsel(i:i)
                line='*'
              endif
              call sff_TrimLen(line, ntrim)
              if (debug) print *,'DEBUG: ',captionsel(i:i)
     &          ,'>',line(1:ntrim),'<', line
              if ((caplen+ntrim).lt.len(captionstr)) then
                captionstr(caplen:ntrim+caplen-1)=line(1:ntrim)
                caplen=caplen+ntrim
                if ((caplen+ntrim+3).lt.len(captionstr)) then
                  captionstr(caplen:caplen+2)='   '
                  caplen=caplen+3
                endif
              else
                print *,'WARNING: caption is truncated'
              endif
c enddo i
            enddo
    5     continue
c enddo ichain
        enddo
        if (opttitle) then
          captionstr=partitle
        else
          do i=caplen,len(captionstr)
            captionstr(i:i)=' '
          enddo
        endif
c 
c determine range of box and open plot space
c
        ichain=0
    6   continue
          ichain=ichain+1
          if (ipanel.gt.tracesic(ichain)) goto 6
        trace=firstic(ichain)-1+ipanel
        xmint=timeax(firstsample(trace))
        xmaxt=timeax(firstsample(trace)-1+nsamples(trace))
        ymint=minsval(trace)
        ymaxt=maxsval(trace)
        do ichain=1,nchain
c is there a trace to plot in this chain?
          if (ipanel.le.tracesic(ichain)) then
            trace=firstic(ichain)-1+ipanel
            xmint=min(xmint,timeax(firstsample(trace)))
            xmaxt=max(xmaxt,timeax(firstsample(trace)-1+nsamples(trace)))
            ymint=min(ymint,minsval(trace))
            ymaxt=max(ymaxt,maxsval(trace))
          endif
c enddo ichain
        enddo
c may be we have to use global scales
        if (optscalex) then
          xmint=xminp
          xmaxt=xmaxp
        endif
        if (optscaley) then
          ymint=yminp
          ymaxt=ymaxp
        endif
c modify scales by selected factors
        if (optcenter) then
          ymaxt=max(abs(ymaxt),abs(ymint))
          ymint=-max(abs(ymaxt),abs(ymint))
        endif
        yamp=(ymaxt-ymint)/Yfac
        ymid=(ymaxt+ymint)/2.
        ymint=ymid-0.5*yamp
        ymaxt=ymid+0.5*yamp
        xamp=(xmaxt-xmint)
        xmint=xmint+xamp*xlfac
        xmaxt=xmaxt-xamp*xrfac
        if (optsetxrange) then
          xmint=parxmin
          xmaxt=parxmax
        endif
c if ordinate scale explicitely set on command line
        if (optsetyrange) then
          ymint=parymin
          ymaxt=parymax
        endif
c plot only positive values if requested
        if (optpositiveonly) then
          ymint=0.
          ymaxt=max(0.,ymaxt)
        endif
c be aware of zero amplitudes
        if (ymaxt.eq.ymint) then
          if(ymaxt.eq.0.) then
            ymaxt=.5e-30
            ymint=-.5e-30
            print *,'NOTICE: trace ',trace,' has amplitude 0.'
          else
            ymaxt=ymint+abs(.5*ymint)
            ymint=ymint-abs(.5*ymint)
          endif
        endif
        if (debug) print *,'DEBUG: trace, ymaxt, ymint ',trace,ymaxt,ymint
        call pgswin(xmint, xmaxt, ymint, ymaxt)
c 
c calc values for new vp
c 
c start at botom of last box
        vptop=vpbot
c is there a caption
        fractop=vptop-(capheight*labelheight)
        if (optinline) fractop=vptop
c get bottom of box
        fracbot=fractop-boxratio*labelheight
c is there a numeric label
        vpbot=fracbot
        if (.not.((optscalex).and.(optinline))) then
          vpbot=vpbot-(numheight*labelheight)
        elseif (trace.eq.ntraces) then
          vpbot=vpbot-(numheight*labelheight)
        endif
        if (.not.(optscalex)) then
          vpbot=vpbot-(labheight*labelheight)
        elseif (trace.eq.ntraces) then
          vpbot=vpbot-(labheight*labelheight)
        endif
        call pgsch(ch_std)
        if (optpgenv) then
          call pgvstd
        else
          call pgsvp(chf_ylab*labelheight*2.5,0.99,fracbot,fractop)
        endif
c
c determine labelling style
c
        fbotlab=.true.
        ftoplab=(.not.(optinline))
        if ((optinline).and.(optscalex)) then
          fbotlab=.false.
          ftoplab=.false.
        endif
        if (trace.eq.ntraces) fbotlab=.true.
        botlabel=' '
        if ((trace.eq.ntraces).or.(.not.(optscalex))) then
          if (optsrcetime) then
            botlabel='time since source event / sec'
            if (opttbox) botlabel='time since source event'
          elseif (optabstime) then
            botlabel='time since midnight / sec'
            if (opttbox) botlabel='time since midnight'
          else
            botlabel='time since first sample / sec'
            if (opttbox) botlabel='time since first sample'
          endif
          if (optxlabel) botlabel=parxlabel
        endif
c 
c determine box style and plot box
c
        if (nmajorxticks.ne.0) then
          if (minorxticks.lt.((xmaxt-xmint)/1.e3))
     &      stop 'ERROR: minor tick mark interval too small!'
          nminorxticks=int((xmaxt-xmint)/(nmajorxticks*minorxticks))
          majorxticks=nminorxticks*minorxticks
        endif
        if (opttbox) then
          if (fbotlab.and.(.not.optnoxlabels)) then
            call pgtbox('ZABCTSHO',majorxticks,nminorxticks,'BCTS',0.0,0)
            call pgslw(lw_txt)
            call pgsch(ch_xlab)
            call pgtbox('ZNHO',majorxticks,nminorxticks,' ',0.0,0)
            call pgslw(lw_std)
            call pgsch(ch_std)
          else
            call pgtbox('ZABCTSHO',majorxticks,nminorxticks,'BCTS',0.0,0)
          endif
        else
          if (fbotlab.and.(.not.optnoxlabels)) then
            call pgtbox('ABCTSYHO',majorxticks,nminorxticks,'BCTS',0.0,0)
            call pgslw(lw_txt)
            call pgsch(ch_xlab)
            call pgtbox('NY',majorxticks,nminorxticks,' ',0.0,0)
            call pgslw(lw_std)
            call pgsch(ch_std)
          else
            call pgtbox('ABCTSYHO',majorxticks,nminorxticks,'BCTS',0.0,0)
          endif
        endif
        call pgsch(yvalheight)
        call pgslw(lw_txt)
        call pgsch(ch_ylab)
        if (optvertlab) then
          call pgtbox(' ',majorxticks,nminorxticks,'NV',0.0,0)
        else
          call pgtbox(' ',majorxticks,nminorxticks,'N',0.0,0)
        endif
        call pgslw(lw_std)
        call pgsch(ch_std)
        call pgsch(labelheight*30.)
        if (optgrid) then
          call pgsls(4)
          if (opttbox) then
            call pgtbox('ZG',majorxticks,nminorxticks,'G',0.0,0)
          else
            call pgtbox('G',majorxticks,nminorxticks,'G',0.0,0)
          endif
          call pgsls(1)
        endif
c 
c plot a marker at specified time if desired
c 
      if (optmarker) then
        call pgsave
        call pgsch(ch_std)
        call pgpt1(xmark, ymaxt, -31)
        call pgpt1(xmark, ymint, -31)
        call pgslw(3)
        call pgsls(4)
        call pgmove(xmark,ymint)
        call pgdraw(xmark,ymaxt)
        call pgunsa
      endif
c 
c plot labels
c 
        call pgslw(lw_txt)
        if (optpgenv) then
          call pgsch(ch_xlab)
          call pglab(botlabel, ' ', ' ')
          call pgsch(ch_ylab)
          call pglab(' ', unitslabel, ' ')
          call pgsch(ch_cap)
          if (ftoplab) then
            call pglab(' ', ' ', captionstr)
          else
            if (.not.optcleancaprect) then
              call pgmtxt('LV',-1.,0.9,0.0,captionstr)
            endif
          endif
        else
          call pgsch(ch_xlab)
          call pgmtxt('B',(2.9-(chf_xlab-1.)),0.5,0.5,botlabel)
          call pgsch(ch_ylab)
          call pgmtxt('L',(3.2-(chf_ylab-1.)),0.5,0.5,unitslabel)
          call pgsch(ch_cap)
          if (ftoplab) then
            call pgmtxt('T',0.7,0.5,0.5,captionstr)
          else
            if (.not.optcleancaprect) then
              call pgmtxt('LV',-1.,(0.9-(chf_cap-1.)*0.025),0.0,captionstr)
            endif
          endif
        endif
        call pgslw(lw_std)
        call pgsch(ch_std)
c 
c plot time series
c
        call pgslw(lw_cur)
        istyle=0
        if (optblack) istyle=1
        do ichain=1,nchain
          istyle=istyle+1
          if (istyle.gt.maxstyle) istyle=1
          if (optcyclestyle) then
            if (optcolor) then
              call pgsci(istyle)
            else
              call pgsls(istyle)
            endif
          else
            istyle=1
            if (optblack) istyle=2
            call pgsci(istyle)
          endif
c is there a trace to plot in this chain?
          if (ipanel.le.tracesic(ichain)) then
            trace=firstic(ichain)-1+ipanel
            call pgline(nsamples(trace), 
     &        timeax(firstsample(trace)),
     &        fdata(firstsample(trace)))
            call pgupdt
          endif
c enddo ichain 
        enddo
c if optcleancaprect print caption here
        if (optcleancaprect) then
c   world coordinate ranges
          jx1=xmaxt-xmint
          jx2=ymaxt-ymint
c   world coordinate offsets
          jx3=0.02*jx1
          jx4=0.02*jx2
c   first read size of box
          call pgqtxt(xmint+jx3,ymint+jx4,0.,0.,captionstr,
     &      boxxcoor, boxycoor)
          jx5=boxycoor(2)-boxycoor(1)
          call pgqtxt(xmint+jx3,ymaxt-jx4-jx5,0.,0.,captionstr,
     &      boxxcoor, boxycoor)
          call pgsave
          call pgsfs(1)
          call pgsci(0)
c   clear background of cation's box
          call pgpoly(4,boxxcoor,boxycoor)
          call pgunsa
c   write caption
          call pgptxt(xmint+jx3,ymaxt-jx4-jx5,0.,0.,captionstr)
        endif
        call pgsls(1)
        call pgsci(1)
        call pgslw(lw_std)
c enddo ipanel
      enddo
c 
c keep alive
c
      if (optkeep) then
        cursx=0.
        cursy=0.
    3   curresult=pgcurs(cursx, cursy, curchar)  
        if ((curresult.eq.1).and.(curchar.ne.'x').and.(curchar.ne.'X'))
     &    goto 3
      endif
      call pgend
      stop 
c----------------------------------------------------------------------
c formats
   80 format(a)
   81 format(2a)
   82 format(a,i4)
c stop sequences
   98 stop 'ERROR: closing data file!'
   97 stop 'ERROR: in command line option arguments!'
   96 stop 'ERROR: too few command line option arguments!'
      end
      
c======================================================================
c 
c some subroutines
c
c----------------------------------------------------------------------
c
c checkmem
c
c check whether integer and real variables use the same
c amount of bytes in memory
c
      subroutine checkmem(idata, fdata, maxsamples)
c declare parameters
      integer maxsamples
      real fdata(maxsamples)
      integer idata(maxsamples)
c declare variables
      integer i,n
c go
      n=min(maxsamples,30)
      print *,'CHECKMEM: ----- START -----'
      do i=1,n
        idata(i)=i
      enddo
      write(6,'(6(i10,1x))') (idata(i), i=1,n)
      print *,' '
      do i=1,n
        fdata(i)=float(idata(i))
      enddo
      write(6,'(6(f10.2,1x))') (fdata(i), i=1,n)
      print *,' '
      write(6,'(6(i10,1x))') (idata(i), i=1,n)
      print *,' '
      do i=1,n
        idata(i)=int(fdata(i))
      enddo
      write(6,'(6(i10,1x))') (idata(i), i=1,n)
      print *,'CHECKMEM: ------ END ------'
      return
      end
c 
c----------------------------------------------------------------------
c
c sffopen
c
c open sff file 
c evaluate selection
c read file header
c
      subroutine sffopen(lu, filep, useselect, selection, maxselect, 
     &  srcedate, debug)
c declare parameters
      integer lu, filep, maxselect
      integer srcedate(7)
      logical useselect, selection(maxselect), debug
c declare variables
      integer ierr
      real sffversion
      character timestamp*13, code*20, line*80, filename*80
      character srcetype*20, date*6, time*10, cs
      real c1, c2, c3

c go
c evaluate trace selections
      call getarg(filep, filename)
      call getarg(filep+1, line)
      if (debug) print *,'(DEBUG) line: ',line
      if (line(1:2).eq.'t:') then
        filep=filep+1
        useselect=.true.
        if (debug) print *,'(DEBUG): use selection list'
        call tf_listselect(maxselect, selection, 3, line, ierr)
        if (ierr.eq.1) then
          print *,'WARNING: selection exceeds possible range',
     &            ' from 1 to',maxselect
          print *,'         selecting only up to no.',maxselect
        elseif (ierr.eq.2) then
          print *,'WARNING: missing selection list - selecting ',
     &            'all traces'
          useselect=.false.
        elseif (ierr.ne.0) then
          print *,'WARNING: unknown error code by tf_listselect'
          print *,'         selecting all traces'
          useselect=.false.
        endif
      else
        useselect=.false.
      endif
c read file header and ignore optional blocks
      call sff_ROpenS(lu, filename, sffversion,timestamp,code,
     &  srcetype, cs, c1, c2, c3, date, time,
     &  ierr)
      if (ierr.ne.0) stop 'ERROR: opening file'
      call sffu_timesrce(date, time, srcedate)
      return
      end
c 
c----------------------------------------------------------------------
c 
c sffread
c
c read one sff trace an skip optional blocks
c
      subroutine sffread(lu, infile, trace, moretraces, debug,
     &                    maxsamples, maxtraces, filename, dt, timeax,
     &                    firstsample, nsamples, firstfree, date,
     &                    time, sectime, station, channel, auxid,
     &                    instype, fdata, idata, maxsval, average, minsval,
     &                    optabstime, verbose, timeshift,
     &                    loccs, locc1, locc2, optsrcetime, srcedate)
c declare parameters
      character infile*80
      logical moretraces, debug, optabstime, optsrcetime
      integer maxsamples, lu, trace, maxtraces
      integer nsamples(maxtraces), firstsample(maxtraces)
      character*80 filename(maxtraces), firstfree(maxtraces)
      character*10 date(maxtraces)
      character*12 time(maxtraces)
      character*5 station(maxtraces)
      character*3 channel(maxtraces)
      character*4 auxid(maxtraces)
      character*6 instype(maxtraces)
      character*1 loccs(maxtraces)
      real locc1(maxtraces), locc2(maxtraces)
      real sectime(maxtraces), dt(maxtraces)
      real maxsval(maxtraces), average(maxtraces), minsval(maxtraces)
      real fdata(maxsamples), value, minv, maxv, timeax(maxsamples)
      integer idata(maxsamples), nsamp
      double precision avg
      logical verbose
      real timeshift
      integer srcedate(7)
c declare variables
      integer sample, ierr, ntrim, nstack
      character wid2line*132, code*20
      real stime, c3
      logical last
      integer maxfree, nfree, mfreelen
      parameter(maxfree=50)
      character*80 freelines(maxfree)
      integer tracedate(7), toffset(7)
      real sffu_seconds
      integer time_compare
c go
      call sff_TrimLen(infile,ntrim)
      filename(trace)=infile(1:ntrim)
      if (trace.eq.1) then
        firstsample(trace)=1
      else
        firstsample(trace)=firstsample(trace-1)+nsamples(trace-1)
      endif
c read trace
      moretraces=.false.
      nsamp=maxsamples-firstsample(trace)
      if (debug) print *,'(DEBUG): call sff_RTraceI'
      call sff_RTraceFI(lu, sectime(trace), dt(trace),
     &                 wid2line, nsamp,
     &                 fdata(firstsample(trace)),
     &                 idata(firstsample(trace)),
     &                 code, last, 
     &                 nfree, freelines, maxfree, mfreelen,
     &                 loccs(trace),locc1(trace),locc2(trace),
     &                 c3,nstack,ierr)
      if (debug) print *,'(DEBUG): returned from sff_RTraceI'
      if (ierr.ne.0) stop 'ERROR: reading trace' 
      nsamples(trace)=nsamp
      if ((nsamples(trace)+firstsample(trace)-1).gt.maxsamples) 
     &  stop 'ERROR: too many samples'
      moretraces=(.not.last)
      firstfree(trace)=' '
      if (nfree.gt.0) firstfree(trace)=freelines(1)
c translate data from integer to real
      minv=fdata(firstsample(trace))
      maxv=minv
      avg=0.d0
      if (optsrcetime) then
        call sffu_timewid2(wid2line, tracedate)
        call time_sub(tracedate, srcedate, toffset)
        stime=sffu_seconds(toffset)
        if (time_compare(tracedate, srcedate).lt.0) then
          stime=-stime
        endif
      elseif (optabstime) then
        stime=sectime(trace)
      else
        stime=0.
      endif
      if (debug) print *,'DEBUG: minv:',minv,' maxv:',maxv,' avg:',avg
      do sample=firstsample(trace),(firstsample(trace)+nsamples(trace)-1)
        value=fdata(sample)
        timeax(sample)=stime+dt(trace)*float(sample-firstsample(trace))+
     &                 timeshift
        avg=avg+dble(value)
        maxv=max(maxv,value)
        minv=min(minv,value)
      enddo
      average(trace)=sngl(avg)/nsamples(trace)
      maxsval(trace)=maxv
      minsval(trace)=minv
      if (debug) print *,'DEBUG: minv:',minv,' maxv:',maxv,' avg:',avg
c extract information from wid2line
      date(trace)=wid2line(6:15)
      time(trace)=wid2line(17:28)
      station(trace)=wid2line(30:34)
      channel(trace)=wid2line(36:38)
      auxid(trace)=wid2line(40:43)
      instype(trace)=wid2line(89:94)
      return
      end
c
c----------------------------------------------------------------------
c
c introduced new interface to sff_SkipData
c 28/12/2003
c
      subroutine skipdata(lu, moretraces)
c
      integer lu
      logical moretraces
c
      logical last
      character*10 code
      integer ierr
      call sff_SkipData(lu, code, last, ierr)
      moretraces=(.not.(last))
      if (ierr.ne.0)
     &  stop 'ERROR: skipping trace'
      return
      end
c 
c ---- END OF stuplo.f ----
