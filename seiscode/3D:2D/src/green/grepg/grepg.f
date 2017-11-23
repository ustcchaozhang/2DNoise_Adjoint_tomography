c  this is file <grepg.f>
c======================================================================
c
c  Copyright 1997,2010 by Thomas Forbriger (IfG Stuttgart)
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
c  show amplitude plot of Fourier-Bessel expansion matrix
c
c  25/06/97   V1.0   first running version (from amppg.f)
c
c  REVISIONS and CHANGES
c     V1.0   25/06/97   Thomas Forbriger
c     V1.1   02/07/97   added real/imaginary part plotting
c     V1.2   03/07/97   include aliasing pick
c     V1.3   31/07/97   added new linear plotting
c     V1.4   20/10/97   tell type of plot in title
c     V1.5   31/10/97   working on correct linear interpolation with
c                       strange greda files (set maxvalue)
c     V1.6   15/01/98   plot cross-sections
c     V1.7   18/08/98   now coming with -m option
c     V1.8   07/11/98   set contour levels according to 
c                       max and min for isolines
c     V1.9   09/11/98   there was a bug setting min/max for cross-section
c                       ordinate
c     V2.0   25/11/98   several changes...
c                       * umin and fmin were obsolete and even not initialized
c                         we now use mins and minf
c                       * introduced some picking of dispersion curves
c                       * changed some mouse hotkeys
c     V2.1   19/01/99   introduced output for GMT plot data
c     V2.2   22/01/99   character height option
c     V2.3   24/01/99   option to read pick-file upon invocation
c     V2.4   14/05/99   activate scalingslim for frequency individual 
c                       scaling too
c     V2.5   14/06/99   reactivated suppress mode
c     V2.6   24/06/99   allow normalizing when scaling frquencies
c                       individually
c     V2.7   01/07/99   allow reading of taper files
c     V2.8   07/07/99   handle resolution matrix output from grereso
c     V2.9   25/08/99   * allow setting of labels and titles
c                       * wedge annotations
c                       * global normalize
c                       * logarithmic wedge
c     V2.10  27/08/99   * introduced normlimit 
c     V2.11  03/09/99   pick flexural wave dispersion curve
c     V2.12  14/10/99   optional colorfull plot with phase information
c     V2.13  22/10/99   Johanna's birthday
c                       introduced phase plotting mode
c     V2.14  23/10/99   * removed seperate section for raw plotting
c                         plotting is now done with the same commands for
c                         raw or smooth plots
c                       * introduced contour labelling
c                       * reorganized help section
c     V2.15  16/11/99   smooth frequency dependent scaling
c     V2.16  07/04/00   * added polynomial trend removal 
c                       * and average removal
c                       * and contrast manipulation 
c     V2.17  10/04/00   switching of real and imag part polynomial fit
c     V2.18  12/04/00   introduced option -d (pick curve background)
c     V2.19  23/05/00   * introduced RGB-color foreground
c                       * only use pgimag now
c                       * you may select either HLS interpolation or RGB
c                         interpolation
c                       * you may specify a background for color ramp
c     V2.20  25/05/00   improved moving average of scaling function
c     V2.21  01/06/00   scaling factor moving average: had to maintain cosine
c                       weighting symmetry
c     V2.21a            huhh - bad type in algorithm removed
c     V2.22             improved tho cosine taper again
c     V2.23  16/07/00   introduced new limit parameter maxilim (-e)
c     V2.24  17/07/00   added option -E
c     V2.25  09/08/00   default of plpar_bgcurvewidth changed to integer type
c                       (required by g77)
c     V2.26  05/12/00   use unformatted internal I/O to read setlinewidth
c     V2.27  23/02/01   prepare to use gabor matrix files
c     V2.28  26/02/01   fixed gabor time scale
c     V2.29  05/03/01   color dispersion curve
c     V2.30  05/03/01   wedge label height
c     V2.31  16/10/01   comment on file formats
c     V2.32  19/11/01   allow subtitle annotations
c     V2.32a 21/01/02   allow more picks to be read
c     V2.33  22/01/02   allow color and linestyle cycle switching
c     V2.33a 02/04/02   give more specific error messages
c     V2.34  19/05/02   allow plots matching different Fourier transform sign
c                       conventions
c     V2.35  13/09/02   phasor walkout from Fourier data
c     V2.35a            now provide phasor walkout pick
c     V2.36             pick-response in verbose mode
c     V2.37             new handle wavenumber spectra
c     V2.37a 05/09/03   move array dimensions to include file
c                       create a master and copy array dimensions include file
c                       only the master is in CVS, but the copy is loaded here
c                       so we may change the current dimension without
c                       affecting the CVS
c     V2.38             provide adaptive scaling along time axis
c                       (option -ts, flag adaptivetimescaling)
c     V2.39  04/12/09   use correct DIN notation for units
c     V2.40  13/11/10   do not expose the term "green" to the user
c     V2.41  03/11/11   resolved a fatal error when addressing the data array
c                       for scaling gabor matrix values
c     V2.42  06/02/17   support colored dispersion curve on white
c                       background (implemented in grepg_dopicks.f)
c
c======================================================================
      program grepg
c----------------------------------------------------------------------

      include 'grepg_dim.inc'
      include 'grepg_para.inc'
      include 'grepg_picks.inc'
c 
      logical isgrereso, isgabor,iskspec
      character titleformat*130
c  program version:
      character*79 version
      parameter(version='GREPG V2.42  plot Fourier-Bessel coefficients')

c  declare variables for io
      character*200 filename
      integer plotslo, plotfreq
      integer nslo, nfreq
      include 'grepg_ardim.xxx.inc'
      real df
      real weight(maxslo, maxfreq)
      real scaling(maxfreq)
      real avgsrc(0:maxfreq+1)
c      complex indata(maxfreq, maxslo)
      complex data(maxslo, maxfreq)
      real plotdata(maxslo, maxfreq)
      real plotphase(maxslo, maxfreq)
      real om(maxfreq), slo(maxslo)
c contour labels
      character*20 cl_contlabel
      integer cl_intval,cl_minint,cl_charinlab
      integer cl_labmm,cl_labpp,cl_signif
c cross-sections
      real xcross(maxslo), ycross(maxslo), mincross, maxcross
c picks
      character*79 pickhint, hints
c  declare internal variables
      real value, maxvalue, minvalue, rvalue, ivalue, coslen
      real contour(10)
      real du, pi2
      character*200 title, xlabel, ylabel
      real transform(6)
c   number of elements used in scaling array
      integer nscaling
c lower limit for plot values
      double precision lowlimit
      parameter(lowlimit=1.e-20)
c      parameter(lowlimit=0.)
c interpolation
      real fr1, fr2, slo1, slo2, frp, slp, dfre, dslo
      real val1, val2, val3, val4
      real rval1, rval2, rval3, rval4
      real ival1, ival2, ival3, ival4
      integer rfre, rslo
      logical newrect
      real tf_rectint
c cursor routine
      real cux, cuy
      character cub
      real pickdx
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=51)
      character*3 optid(maxopt)
      character*200 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
      character*200 outdevice,fourierfile
      logical isolines, color, resolution, smooth
      logical grid, scale, normalize
      logical implot, replot, aliaspick, plinear, suppress, gmtoutput
      logical readpickfile, verbose, whitepaper
      logical deftitle, defxlab, defylab
      logical phasecolor,phaseswitch,conlab,moveavg,polytrend,avgtrend
      logical inccontr, setforeground, setbackground, hlsinterpol
      logical omsqrscale, phasereverse,fourierinput
      logical adaptivetimescaling
      real linelength, tracedx, charheight, contrth, contrord
      real tenpower, scalingslim, normvalue, normlimit
      real phaseshift,grayify, rgb_red, rgb_green, rgb_blue
      real rgbb_red, rgbb_green, rgbb_blue, maxilim
      real par_wedgeheight, wedgewidth
      integer setlinewidth, avglength, npolytrend, npolymode
      character*200 gmtxyzfile, pickfile, titlestring, subtitlestring
      character*200 xlabelstring, ylabelstring, wedgestring, phasestring
c pgplot
      integer maindev, extdev, pgp_open
      real pg_vpl,pg_vpr,pg_vpt,pg_vpb
c   counters and limits
      integer islo, ifreq, i, ic1, ic2
c   resolution
      real startfreq
      integer npoints
      parameter(npoints=100)
      real x(npoints), y(npoints)
c   debugging
      logical debug
c phase function pi constant
      real pi
      parameter(pi=3.1415926535897931159979)
c here are the keys to our commandline options
      data optid/'-d','-i','-c','-r','-q','-p','-g','-l','-D','-s','-I',
     &  '-R','-P','-L','-S','-m','-G','-H','-F','-v','-N','-T','-W',
     &  '-X','-Y','-n','-A','-M','-C','-B','-a','-O','-f','-t','-Q',
     &  '-K','-b','-x','-y','-h','-e','-E','-Fc','-Ah','-Ta','-FC',
     &  '-Fl','-Pr','-Ww','-ff', '-st'/
      data opthasarg/.TRUE.,2*.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,
     &    7*.FALSE.,4*.TRUE.,.FALSE.,2*.TRUE.,.FALSE.,5*.TRUE.,.FALSE.,
     &    5*.TRUE.,.FALSE.,4*.TRUE.,.FALSE.,.TRUE.,.FALSE.,5*.TRUE.,
     &    .FALSE.,2*.TRUE.,.false./
      data optarg/'x11',2*'-','1.,1.','-','2.5','-','1',7*'-','0.',
     &    'xyz.out','1.',2*'-','5.,4.','title','-','xlabel','ylabel',
     &    '-1.','amplitude','0.','-','phase (°)','0.,10.','20,10,2','9',
     &    '6,1','-','4.,0.3','3',2*'0.,0.,0.','-','0.','-','0.,0.,0.,1',
     &    '1.','NSP','5','4','-','3.','nil','-'/

c======================================================================
c  give basic information
      print *,version
      print *,
     &'Usage: grepg filename [-d dev] [-i] [-c] [-r L,dx] [-R|I] [-L]'
      print *,'               ',
     &'       [-q] [-p p] [-g] [-l width] [-s] [-P] [-S]'
      print *,'               ',
     &'       [-m lim] [-G file] [-H ch] [-F file] [-v]'
      print *,'               ',
     &'       [-N c,w] [-T title] [-X label] [-Y label] [-W]'
      print *,'               ',
     &'       [-n value] [-A label] [-M lim] [-C] [-B label]'
      print *,'               ',
     &'       [-O i,m,s] [-a a,g] [-f n] [-t n,m] [-Q] [-e lim]'
      print *,'               ',
     &'       [-K o,t] [-b width] [-x R,G,B] [-y R,G,B] [-h]'
      print *,'               ',
     &'       [-Fc r,g,b,w] [-Ah h] [-t n,m] [-Ta anno]'
      print *,'               ',
     &'       [-FC n] [-Fl n] [-Pr] [-Ww w]'
      print *,'               ',
     &'       [-ff filename] [-st]'
      print *,
     &'or:    grepg -help'
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, filename)
      if (filename(1:5).eq.'-help') then
        print *,' '
        print *,'Plot amplitudes of Fourier-Bessel coefficients.'
        print *,' '
        print *,'  filename    any tf-type file'
        print *,'              you may read files with the following'
        print *,'              ''magic'' signatures:'
        print *,'             ',
     &   ' ''1234'' complex omega,p-spectra'
        print *,'             ',
     &   ' ''123S'' real weight values to be used as'
        print *,'             ',
     &   ' inversion weights or taper factors'
        print *,'             ',
     &   ' ''123P'' complex grereso coefficients which'
        print *,'             ',
     &   ' use a test-slowness as abscissa'
        print *,'             ',
     &   ' ''123G'' complex gabor matrix coefficients'
        print *,'                     which use a time as abscissa'
        print *,' '
        print *,'data modification options'
        print *,'-------------------------'
        print *,' '
        print *,'  -S          suppress all values for zero frequency'
        print *,'              and values for zero slowenss'
        print *,'              (usefull for strange greda methods)'
        print *,'  -I          use just imaginary part'
        print *,'  -R          use just real part'
        print *,'  -t n,m      remove polynomial trend from data using'
        print *,'              Legendre polynomials up to order ''n'' '
        print *,'              the mode value ''m'' is a sum of:'
        print *,'             ',
     &   ' 1: remove polynomial trend from real part'
        print *,'             ',
     &   ' 2: remove polynomial trend from imag part'
        print *,'  -Q          remove moving average'
        print *,'  -K o,t     ',
     &   ' increase contrast by order o and threshold t'
        print *,'  -E          scale all coefficients with 1/omega**2'
        print *,' '
        print *,'  normalize data'
        print *,' '
        print *,'  -s          scale frequencies individually'
        print *,'              using maximum amplitude value'
        print *,'  -st         apply adaptive scaling along time axis'
        print *,'              rather than frequency axis'
        print *,'              (useful for gabor plots)'
        print *,'  -N c,w      Use a gaussian taper when calculating'
        print *,'              normalization factors in order to scale'
        print *,'              frequencies individually. c will be the'
        print *,'              center of the taper in s/km. w is the'
        print *,'              distance from the center to the value'
        print *,'              where the taper is 0.1 (also in s/km).'
        print *,'  -n value    normalize maximum amplitude to value'
        print *,'  -M lim      use only coefficients for slowness being'
        print *,'             ',
     &   ' greater than lim to find normalizing value'
        print *,'             ',
     &   ' that is used to rescale data per frequency'
        print *,'              (all coefficients will be scaled)'
        print *,'              (default is ',optarg(28)(1:3),').'
        print *,'  -f n       ',
     &   ' apply a smooth weighting function when'
        print *,'             ',
     &   ' scaling frequencies individually - this is'
        print *,'              done by applying a moving average to the'
        print *,'             ',
     &   ' the weight, where ''n'' is the filter length'
        print *,' '
        print *,'  scaling'
        print *,' '
        print *,'  -e lim     ',
     &   ' use coefficients for slowness values being'
        print *,'             ',
     &   ' greater than ''lim'' to find maximum value'
        print *,'              for amplitude plot scale'
        print *,'             ',
     &   ' using this feature may result in amplitude'
        print *,'              clipping'
        print *,'             ',
     &   ' the value of ''lim'' defaults to the value'
        print *,'              given by ''-M'' '
        print *,'  -m lim     ',
     &   ' use only coefficients for slowness being'
        print *,'             ',
     &   ' greater than ''lim'' to find maximum value'
        print *,'             ',
     &   ' that ist used to evaluate the normalizing'
        print *,'              factor for the whole dataset'
        print *,'              (default is ',optarg(16)(1:3),').'
        print *,'  -p p       ',
     &   ' the depth of the amplitude grayscale will be'
        print *,'              p powers of ten'
        print *,'  -L         ',
     &   ' use linear amplitude scaling rather than log10'
        print *,'  -Pr         plot phase for different Fourier sign'
        print *,'              convention'
        print *,'              greda and other define the Fourier'
        print *,'              transformation to have a positive'
        print *,'              imaginary factor in the transformation'
        print *,'              from frequency to time domain'
        print *,'              this switch changes the sign convention'
        print *,'              of the data to the opposite'
        print *,' '
        print *,'define data aspect'
        print *,'------------------'
        print *,' '
        print *,'  -C          colorfull plot with phase information'
        print *,'  -a a,g      just plot phase information as grayscale'
        print *,'             ',
     &   ' a: (real value; 0.<= a <=360.) phase angle to'
        print *,'                 shift grayscale (given in degrees)'
        print *,'             ',
     &   ' g: (real value; 0.<= g <=1.) grayify factor'
        print *,'             ',
     &   ' values differing from 1. will result in'
        print *,'             ',
     &   ' a grayscale that does not use all values'
        print *,'                 from black to white'
        print *,'  -i          plot isolines'
        print *,'              amplitude values are still available to'
        print *,'              isoline contour plots even if you plot'
        print *,'              the signal phase'
        print *,'  -O i,m,s    label isoline contour'
        print *,'              i: INTVAL integer value for PGCONL'
        print *,'                 will set the spacing between labels'
        print *,'              m: MININT integer value for PGCONL'
        print *,'                 will set minimum contour length to be'
        print *,'                 labelled'
        print *,'              s: integer setting number of significant'
        print *,'                 digits to be given in label'
        print *,' '
        print *,'appearance of plot'
        print *,'------------------'
        print *,' '
        print *,'  -q          quick-mode does not smooth the data'
        print *,'  -g          plot grid'
        print *,'  -l width    set line width'
        print *,'  -H ch       character height for plot'
        print *,'  -W          plot black on white'
        print *,'              ATTENTION: be sure to use this option'
        print *,'             ',
     &   ' together with a postscript output device'
        print *,'             ',
     &   ' as those don''t know about ''white on black'''
        print *,'  -b width   ',
     &   ' for grayscale plots: plot all picked curves'
        print *,'             ',
     &   ' with color index 1 - place them on top of a'
        print *,'             ',
     &   ' curve of width ''width'' and color index 0'
        print *,'  -Fc r,g,b,w',
     &   ' plot dispersion curves with width ''w'' and'
        print *,'              color (r,g,b,).'
        print *,'  -c          plot colored resolution marks and'
        print *,'              and colored grid'
        print *,' '
        print *,'  -T title    define title string for plot'
        print *,'  -X label    define x-label for plot'
        print *,'  -Y label    define y-label for plot'
        print *,'  -A label   ',
     &   ' define amplitude wedge annotation label for plot'
        print *,'  -Ta anno   ',
     &   ' define subtitle annotations'
        print *,'  -Ah h      ',
     &   ' set amplitude wedge label character height'
        print *,'              to ''h'' '
        print *,'  -B label   ',
     &   ' define phase wedge annotation label for plot'
        print *,'  -x R,G,B    set foreground color to RGB-value'
        print *,'  -y R,G,B   ',
     &   ' set background color for color ramp to RGB-value'
        print *,'  -h          use HLS interpolation for color ramp'
        print *,'  -FC n       cycle curve color with period n'
        print *,'  -Fl n       cycle curve linestyle with period n'
        print *,'  -Ww w       plot wedges of width w (default is 3.)'
        print *,' '
        print *,'additional information to be plotted'
        print *,'------------------------------------'
        print *,' '
        print *,'  -r L,dx     plot resolution marks according'
        print *,'              to a geophone-trace-stepwidth of'
        print *,'              dx and a total length of the'
        print *,'              geophone-line of L (in meters)'
        print *,'              (this leads to bad results with'
        print *,'              isolines)'
        print *,' '
        print *,'input/output'
        print *,'------------'
        print *,' '
        print *,'  -d dev      select output-device (see below for'
        print *,'              a list of possible devices)'
        print *,'  -G file     writes one-column xyz-data to file'
        print *,'             ',
     &   ' this file may be converted using xyz2grd.sh'
        print *,'  -F file     read dispersion curve picks from file'
        print *,'  -ff filename'
        print *,'              read Fourier data for phasor walkout'
        print *,'              from file ''filename'' '
        print *,' '
        print *,'user interaction'
        print *,'----------------'
        print *,' '
        print *,'  -v          verbose mode'
        print *,'  -P          pick aliasing (see below)'
        print *,' '
        print *,'some inside-information:'
        print *,'------------------------'
        print *,' '
        print *,'  After modifying the read',
     &   ' dataset the complex coefficients'
        print *,'  my be smoothed. The raw or',
     &   ' smoothed data is split into'
        print *,'  a set of amplitude values',
     &   ' (linear or logarithmic) and a set'
        print *,'  of phase values. In case',
     &   ' you decide to plot the phase or'
        print *,'  the full colored complex',
     &   ' information the amplitude values'
        print *,'  are still available and',
     &   ' may be used for a contour map.'
        print *,' '
        print *,'picking:'
        print *,'  <left button>, <P>, <p>: ',
     &   ' pick one point of active curve'
        print *,'  <mid button>, <D>, <d>:  ',
     &   ' delete nearest pint of active curve'
        print *,'  <H>, <h>:                ',
     &   ' pick a u/f-pair for hyperbola'
        print *,'  <B>, <b>:                ',
     &   ' pick a u/f-pair for flexural wave'
        print *,'                            dispersion'
        print *,'  <SPACE>, <r>:             replot'
        print *,'  <right button>, <X>, <x>: exit'
        print *,'  <c>:                      plot a cross-section'
        print *,'  <w>:                      plot a phasor walkout'
        print *,'  <l>:                      read dispersion curves'
        print *,'  <s>:                      save dispersion curves'
        print *,'  <1>, <2>, <3>, <4>, <5>,'
        print *,'  <6>, <7>, <8>, <9>, <0>: ',
     &   ' select dispersion curve to pick'
        print *,' '
        print *,'=================================================='
        print *,'possible output devices: '
        call pgp_showdevices
        print *,' '
        print *,'=================================================='
        print *,' '
        print *,'array dimensions:'
        print *,'     maximum number of frequencies: ',maxfreq
        print *,'  maximum number of slowness steps: ', maxslo
        print *,' '
        call grepg_filecomment
        stop
      endif

c----------------------------------------------------------------------
c  get commandline
      outdevice='x11'
      isolines=.FALSE.
      color=.FALSE.
      resolution=.FALSE.
      smooth=.TRUE.
      tenpower=2.5
      grid=.FALSE.
      call getarg(1, filename)
c
c get commandline
c
      call tf_cmdline(2, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      debug=optset(9)
      verbose=optset(20)
      if (debug) print *,'evaluate command line...'
      if (optset(1)) outdevice=optarg(1)
      if (optset(2)) isolines=.TRUE.
      if (optset(3)) color=.TRUE.
      if (optset(4)) then
        resolution=.TRUE.
        read(optarg(4), *, err=99, end=98) linelength, tracedx
        linelength=linelength*1.e-3
        tracedx=tracedx*1.e-3
      endif
      if (optset(5)) smooth=.FALSE.
      if (optset(6)) read(optarg(6), *, err=99, end=98) tenpower
      if (optset(7)) grid=.TRUE.
      read(optarg(8), *, err=99, end=98) setlinewidth
c option -s
      scale=optset(10)
      implot=optset(11)
      replot=optset(12)
      aliaspick=optset(13)
      plinear=optset(14)
      suppress=optset(15)
      read(optarg(16), *, err=99, end=98) scalingslim
c campatibility to version prior V2.23
      maxilim=scalingslim
      gmtoutput=optset(17)
      gmtxyzfile=optarg(17)
      read(optarg(18), *, err=99, end=98) charheight
      readpickfile=optset(19)
      pickfile=optarg(19)
c option -N
      norm_on=optset(21)
      read(optarg(21), *, err=99, end=98) norm_center, norm_width
      deftitle=optset(22)
      titlestring=optarg(22)
      whitepaper=optset(23)
      defxlab=optset(24)
      xlabelstring=optarg(24)
      defylab=optset(25)
      ylabelstring=optarg(25)
c option -n
      normalize=optset(26)
      read(optarg(26), *, err=99, end=98) normvalue
      wedgestring=optarg(27)
      read(optarg(28), *, err=99, end=98) normlimit
      phasecolor=optset(29)
      phasestring=optarg(30)
      phaseswitch=optset(31)
      read(optarg(31), *, err=99, end=98) phaseshift,grayify
      grayify=max(0.,min(1.,grayify))
      conlab=optset(32)
      read(optarg(32), *, err=99, end=98) cl_intval,cl_minint,cl_signif
c option -f
      moveavg=optset(33)
      read(optarg(33), *, err=99, end=98) avglength
      avglength=max(2,avglength)
      polytrend=optset(34)
      read(optarg(34), *, err=99, end=98) npolytrend,npolymode
      avgtrend=optset(35)
      inccontr=optset(36)
      read(optarg(36), *, err=99, end=98) contrord, contrth
      plflag_bgcurve=optset(37)
      read(optarg(37), *, err=99, end=98) plpar_bgcurvewidth
      setforeground=optset(38)
      read(optarg(38), *, err=99, end=98) rgb_red,rgb_green,rgb_blue
      setbackground=optset(39)
      read(optarg(39), *, err=99, end=98) rgbb_red,rgbb_green,rgbb_blue
      hlsinterpol=optset(40)
      if (optset(41)) read(optarg(41), *, err=99, end=98) maxilim
      omsqrscale=optset(42)
      plflag_colcur=optset(43)
      read(optarg(43), *, err=99, end=98) 
     &  (plpar_colcurc(i),i=1,3),plpar_colcurw
      read(optarg(44), *, err=99, end=98) par_wedgeheight
      subtitlestring=optarg(45)
      plflag_color=optset(46)
      read(optarg(46), *, err=99, end=98) plpar_colcyc
      plflag_linestyle=optset(47)
      read(optarg(47), *, err=99, end=98) plpar_lscyc
      phasereverse=optset(48)
      read(optarg(49), *, err=99, end=98) wedgewidth
      fourierinput=optset(50)
      fourierfile=optarg(50)
c option -st
      adaptivetimescaling=optset(51)

c      print *,subtitlestring
c      print *,optarg(45)
c 
c covariances
      if (replot) implot=.false.
      if (phasecolor) phaseswitch=.false.
      if (norm_on) scale=.true.
      if (adaptivetimescaling) scale=.true.
c
      if (verbose) print *,'verbose mode'
c 
c  get datafile
      call greenread(filename, debug,
     &     maxslo, maxfreq, slo, om,
     &     nslo, nfreq, data, weight, isgrereso, isgabor, iskspec)
c  check dimensions for quick-mode
      print *,' '
      print *,'dimensions of dataset read:'
      if (iskspec) then
        print *,' wavenumber: ',nslo
      elseif (isgabor) then
        print *,'       time: ',nslo
      else
        print *,'   slowness: ',nslo
      endif
      if (isgrereso) then
        print *,' t-slowness: ',nfreq
      else
        print *,'  frequency: ',nfreq
      endif
c 
      pi2=8.*atan(1.)
c report
      print *,' '
      if (isgrereso) then
        df=(om(2)-om(1))*1.e3
        minf=om(1)*1.e3
        maxf=minf+(nfreq-1)*df
        print *,'test slowness range: ',minf,'s/km - ',maxf,
     &          's/km du=',df,'s/km'
      else
        df=(om(2)-om(1))/pi2
        minf=om(1)/pi2
        maxf=minf+(nfreq-1)*df
        print *,'frequency range: ',minf,'Hz - ',maxf,'Hz   df=',df,'Hz'
      endif
      du=(slo(2)-slo(1))*1.e3
      mins=slo(1)*1.e3
      maxs=mins+du*(nslo-1)
      if (isgabor) then
        print *,' time range: ',mins,'s - ',maxs,'s   dt=',du,'s'
      elseif (iskspec) then
        print *,' wavenumber range: ',mins,'1/km - ',maxs,
     &    '1/km   dk=',du,'1/km'
      else
        print *,' slowness range: ',mins,'s/km - ',maxs,
     &    's/km   du=',du,'s/km'
      endif
      print *,' '
c----------------------------------------------------------------------
c initialize
      plstring_version=version
c 
      call grepg_pickinit
      if (readpickfile) call grepg_readpicks(pickfile)
c 
      hints=
     &  'active keys: <p,P,x,X,r,d,D,c,w,1,2,3,4,5,6,7,8,9,0,h,l,s,b>'
      write (pickhint, 51) active_pick
c
c----------------------------------------------------------------------
c read Fourier data
      if (fourierinput) then
        call grepg_readfourier(fourierfile, verbose, om, nfreq, maxfreq)
      else
        call grepg_nofourier
      endif

c----------------------------------------------------------------------
c  create plot-data
c  ----------------
c 
c suppress imaginary/real part
c
      if (implot) then
        print *,'suppress real part...'
        do ifreq=1,nfreq
          do islo=1,nslo
            data(islo,ifreq)=cmplx(aimag(data(islo,ifreq)))
          enddo
        enddo
      endif
      if (replot) then
        print *,'suppress imaginary part...'
        do ifreq=1,nfreq
          do islo=1,nslo
            data(islo,ifreq)=cmplx(real(data(islo,ifreq)))
          enddo
        enddo
      endif
      if (suppress) then
        print *,'suppress zero frequency values to lower limit ',
     &    lowlimit,'...'
        do islo=1,nslo
          data(islo,1)=cmplx(sngl(lowlimit))
        enddo
        print *,'suppress zero slowness values to lower limit ',
     &    lowlimit,'...'
        do ifreq=1,nfreq
          data(1,ifreq)=cmplx(sngl(lowlimit))
        enddo
      endif
      print *,
     & '     limit rescaling factor evaluation to data with slowness>=',
     &           scalingslim
      print *,
     & '    limit maximum amplitude plot scale to data with slowness>='
     &           ,maxilim
      print *,
     & 'limit evaluation of normalizing factor to data with slowness>=',
     &            normlimit
c
c scale each frequency to the same maximum amplitude
c --------------------------------------------------
c
      if (omsqrscale) then
        print *,'scale with 1/omega**2'
        do ifreq=1,nfreq
          if (om(ifreq).lt.1.e-6) then
            value=1./(max(1.e-10,om(ifreq+1))**2)
          else
            value=1./(max(1.e-10,om(ifreq))**2)
          endif
          do islo=1,nslo
            data(islo,ifreq)=data(islo,ifreq)*value
          enddo
        enddo
      endif
c
c prepare scaling function for adaptive scaling
c ---------------------------------------------
c (option -s)
      if (scale) then
c distinguish between scaling along frequency and time axis
        if (adaptivetimescaling) then
c 
          if (verbose) then
            print *,'adaptive scaling will be applied along time axis'
            print *,'  setup scaling function...'
          endif
c check required size of scaling array
          nscaling=nslo
          if (nscaling.gt.maxslo)
     &      stop 'ERROR: too many time samples for adaptive scaling'
c (option -N)
          if (norm_on) then
            print *,'  use normalizing taper...'
c taper function is f(s)=exp(-ln(10)*((c-s)/w)**2)
c with c=norm_center and w=norm_width
c maxvalue here means normvalue
            do islo=1,nslo
              maxvalue=0.
              do ifreq=1,nfreq
                maxvalue=maxvalue+
     &             abs(data(islo,ifreq))*du*exp(-log(10.)*
     &             ((norm_center-slp)/norm_width)**2)
              enddo
              maxvalue=max(maxvalue,sngl(lowlimit))
              scaling(islo)=1./maxvalue
            enddo
          else
            do islo=1,nslo
              maxvalue=max(abs(data(islo,nfreq)),sngl(lowlimit))
              do ifreq=1,nfreq
                maxvalue=max(maxvalue,abs(data(islo,ifreq)))
              enddo
              scaling(islo)=1./maxvalue
            enddo
          endif
        else
c apply adaptive scaling along frequency axis
          nscaling=nfreq
          print *,'scale each frequency...'
          print *,'  scaling with respect to values greater than ',
     &      scalingslim,'s/km'
          print *,'  setup scaling function...'
c (option -N)
          if (norm_on) then
            print *,'  use normalizing taper...'
c taper function is f(s)=exp(-ln(10)*((c-s)/w)**2)
c with c=norm_center and w=norm_width
c maxvalue here means normvalue
            do ifreq=1,nfreq
              maxvalue=0.
              do islo=1,nslo
                slp=mins+du*(islo-1)
                if (slp.ge.scalingslim) maxvalue=maxvalue+
     &             abs(data(islo,ifreq))*du*exp(-log(10.)*
     &             ((norm_center-slp)/norm_width)**2)
c              if ((verbose).and.(ifreq.eq.50)) then
c                print *,islo,slp,exp(-log(10.)*
c     &            ((norm_center-slp)/norm_width)**2),
c     &            abs(data(islo,ifreq)),maxvalue
c              endif
              enddo
              maxvalue=max(maxvalue,sngl(lowlimit))
              scaling(ifreq)=1./maxvalue
            enddo
          else
            do ifreq=1,nfreq
              maxvalue=max(abs(data(nslo,ifreq)),sngl(lowlimit))
              do islo=1,nslo
                slp=mins+du*(islo-1)
                if (slp.ge.scalingslim) maxvalue=
     &               max(maxvalue,abs(data(islo,ifreq)))
              enddo
              scaling(ifreq)=1./maxvalue
            enddo
          endif
        endif
c
c smooth scaling function for adaptive scaling
c --------------------------------------------
c moving average is applied when scaing frequencies independently
c (option -f)
        if (moveavg) then
          avglength=min(avglength,nscaling)
          print *,'  apply moving average of length ',avglength,
     &      ' to scaling function...'
c we do it the easy way
c we do it with the reciprocal value as small data values will cause
c extremely large scaling values that would swamp their neighbours
          do ifreq=1,nscaling
            avgsrc(ifreq)=1./scaling(ifreq)
          enddo
          avgsrc(0)=0.
          avgsrc(nscaling+1)=0.
          do ifreq=1,nscaling
            ic1=ifreq-(avglength/2)
            ic2=ifreq+(avglength/2)
            ic1=max(1,ic1)
            ic2=min(nscaling,ic2)
            coslen=float(ic2-ic1+1)
            value=0.
            ivalue=0.
            do i=ic1,ic2
              value=value
     &           +avgsrc(i)*(1.+cos(2.*pi*float(i-ifreq)/coslen))
              ivalue=ivalue+(1.+cos(2.*pi*float(i-ifreq)/coslen))
            enddo
            value=value/ivalue
            scaling(ifreq)=1./value
          enddo
        endif
        print *,'  apply scaling function...'
        do ifreq=1,nfreq
          do islo=1,nslo
            if (adaptivetimescaling) then
              value=scaling(islo)
            else
              value=scaling(ifreq)
            endif
            data(islo,ifreq)=data(islo,ifreq)*value
          enddo
        enddo
      endif
c 
c normalize total data (option -n)
c     
      if (normalize) then
        print *,'normalize total data to ',normvalue,'...'
        maxvalue=lowlimit
        do ifreq=1,nfreq
          do islo=1,nslo
            slp=mins+du*(islo-1)
            if (slp.ge.normlimit) maxvalue=
     &            max(maxvalue,abs(data(islo,ifreq)))
          enddo
        enddo
        maxvalue=maxvalue/max(normvalue,sngl(lowlimit))
        do ifreq=1,nfreq
          do islo=1,nslo
            data(islo,ifreq)=data(islo,ifreq)/maxvalue
          enddo
        enddo
      endif
c 
c----------------------------------------------------------------------
c
c here we start data modification and extraction
c
c remove polynomial trend if requested
      if (polytrend) then
        call grepg_poly(npolytrend,nslo,nfreq,data,verbose, 
     &     maxslo, maxfreq, npolymode)
      endif
c
c remove average trend if requested
      if (avgtrend) then
        call grepg_remavg(nslo,nfreq,data,verbose, maxslo, maxfreq)
      endif
c
c increase contrast
      if (inccontr) then
        call grepg_contr(nslo,nfreq,data,maxslo,
     &                   maxfreq,contrth,verbose,contrord)
      endif
c 
c prepare maxvalue
      if (plinear) then
        print *,'use linear amplitude values...'
        maxvalue=max(abs(data(nslo,nfreq)),sngl(lowlimit))
      else
        print *,'use log10 of amplitude values...'
        maxvalue=log10(max(abs(data(nslo,nfreq)),sngl(lowlimit)))
      endif
c 
c extract smooth interpolated data
c --------------------------------
c
      if (smooth) then
        print *,'smoothing data...'
c do linear interpolation
        plotslo=max(dimplotslo,nslo)
        plotfreq=max(dimplotfreq,nfreq)
c give a hint
        print *,'subdivisions of plot:'
        print *,'   slowness: ',plotslo
        print *,'  frequency: ',plotfreq
        print *,'doing linear interpolation...'
c prepare some values
        dfre=(maxf-minf)/float(plotfreq-1)
        dslo=(maxs-mins)/float(plotslo-1)
c prepare surrounding ractangle
c   index
        rslo=1
c   coordinates
        slo1=mins
        slo2=mins+du
        do 12 islo=1,plotslo
          rfre=1
          fr1=minf
          fr2=fr1+df
          newrect=.TRUE.
          do 13 ifreq=1,plotfreq
c coordinates of plotpoint
            frp=minf+dfre*(ifreq-1)
            slp=mins+dslo*(islo-1)
c check rectangle
            if ((slp.gt.slo2).and.(rslo.lt.nslo)) then
              rslo=rslo+1
              slo1=mins+du*(rslo-1)
              slo2=mins+du*rslo
              newrect=.TRUE.
            endif
            if ((frp.gt.fr2).and.(rfre.lt.nfreq)) then
              rfre=rfre+1
              fr1=fr2
              fr2=fr2+df
              newrect=.TRUE.
            endif
c set values
            if (newrect) then
c amplitude values
c we do not use the PGPLOT image transfer function (lin or log) as
c interpolating may give better results this way and the PGPLOT wedge does not
c come with logarithmic scales anyway.
              if (plinear) then
                val1=max(sngl(lowlimit),abs(data(rslo,rfre)))
                val2=max(sngl(lowlimit),abs(data(rslo,rfre+1)))
                val3=max(sngl(lowlimit),abs(data(rslo+1,rfre+1)))
                val4=max(sngl(lowlimit),abs(data(rslo+1,rfre)))
              else
                val1=log10(max(sngl(lowlimit),abs(data(rslo,rfre))))
                val2=log10(max(sngl(lowlimit),abs(data(rslo,rfre+1))))
                val3=log10(max(sngl(lowlimit),abs(data(rslo+1,rfre+1))))
                val4=log10(max(sngl(lowlimit),abs(data(rslo+1,rfre))))
              endif
              if ((debug).and.(rfre.eq.1)) then
                value=tf_rectint(fr1,fr2,slo1,slo2,
     &                val1, val2, val3, val4, frp, slp)
                print *,frp,slp,val1,val2,val3,val4,value
                print *,max(sngl(lowlimit),
     &                      abs(data(rslo,rfre))),rslo,rfre
                print *,max(sngl(lowlimit),
     &                      abs(data(rslo,rfre+1))),rslo,rfre+1
                print *,max(sngl(lowlimit),
     &                      abs(data(rslo+1,rfre+1))),rslo+1,rfre+1
                print *,max(sngl(lowlimit),
     &                      abs(data(rslo+1,rfre))),rslo+1,rfre
              endif
c phase values
c interpolate real and imaginary part of complex number
              rval1=real(data(rslo,rfre))
              rval2=real(data(rslo,rfre+1))
              rval3=real(data(rslo+1,rfre+1))
              rval4=real(data(rslo+1,rfre))
              ival1=aimag(data(rslo,rfre))
              ival2=aimag(data(rslo,rfre+1))
              ival3=aimag(data(rslo+1,rfre+1))
              ival4=aimag(data(rslo+1,rfre))
              newrect=.FALSE.
            endif
c interpolate
c amplitude
            value=tf_rectint(fr1,fr2,slo1,slo2,
     &                val1, val2, val3, val4, frp, slp)
            plotdata(islo, ifreq)=value
c phase
            rvalue=tf_rectint(fr1,fr2,slo1,slo2,
     &                rval1, rval2, rval3, rval4, frp, slp)
            ivalue=tf_rectint(fr1,fr2,slo1,slo2,
     &                ival1, ival2, ival3, ival4, frp, slp)
            plotphase(islo, ifreq)=atan2(ivalue,rvalue)*180./pi
            if (plotphase(islo,ifreq).lt.0.)
     &        plotphase(islo,ifreq)=plotphase(islo,ifreq)+360.
            if (phasereverse) plotphase(islo,ifreq)=
     &        360.-plotphase(islo,ifreq)
c update maximum value
            if (slp.ge.maxilim) maxvalue=max(maxvalue,value)
   13     continue
   12   continue
c 
c take data as is (no smoothing)
c ------------------------------
c
      else
        print *,'taking data as is...'
        plotslo=nslo
        plotfreq=nfreq
        dfre=(maxf-minf)/float(plotfreq-1)
        dslo=(maxs-mins)/float(plotslo-1)
c take array directly
        do 14 islo=1,nslo
          slp=mins+du*(islo-1)
          do 15 ifreq=1,nfreq
            if (plinear) then
              value=max(abs(data(islo,ifreq)),sngl(lowlimit))
            else
              value=log10(max(abs(data(islo,ifreq)),sngl(lowlimit)))
            endif
c amplitude
            plotdata(islo,ifreq)=value
c phase
            plotphase(islo,ifreq)=
     &        atan2(aimag(data(islo,ifreq)),
     &              real(data(islo,ifreq)))*180./pi
            if (plotphase(islo,ifreq).lt.0.)
     &        plotphase(islo,ifreq)=plotphase(islo,ifreq)+360.
            if (phasereverse) plotphase(islo,ifreq)=
     &        360.-plotphase(islo,ifreq)
c update maximum value
            if (slp.ge.maxilim) maxvalue=max(maxvalue,value)
   15     continue
   14   continue
      endif
c 
c define minimum value for plot
c
      if (plinear) then
        minvalue=0.
      else
        minvalue=maxvalue-tenpower
      endif
      if (verbose) print *,'minvalue is ',minvalue
      if (verbose) print *,'maxvalue is ',maxvalue

c----------------------------------------------------------------------
c
c   plot data
c   ---------
c
      maindev=pgp_open(outdevice)
      call pgask(.false.)
c 
c prepare background
c ------------------
c
      if (whitepaper) then
        call pgscr(0, 1.,1.,1.)
        call pgscr(1, 0.,0.,0.)
      endif
c 
c set foreground color
c -------------------
      if (setforeground) then
        call pgscr(1, rgb_red, rgb_green, rgb_blue)
      endif
c 
c prepare color table for phase gray shading plot
c -----------------------------------------------
c
      if (setbackground) call pgscr(0, rgbb_red, rgbb_green, rgbb_blue)
      call grepg_prepcol(phaseswitch,phaseshift,grayify,hlsinterpol)
      if (whitepaper) then
        call pgscr(0, 1.,1.,1.)
      else
        call pgscr(0, 0.,0.,0.)
      endif
c 
c ---------------------
c start plot cycle here
c ---------------------
c 
c come back if replot is desired
c 
 999  continue
      call pgslw(setlinewidth)
      call pgsch(charheight)
c 
c prepare box title and labels
c ----------------------------
c
      if (isgrereso) then 
        titleformat='("phase-slowness/test-slowness - plot of ",a,a)'
        xlabel='wavefield phase-slowness / s km\u-1'
        ylabel='resulting phase-slowness / s km\u-1'
      elseif (isgabor) then
        titleformat='("gabor-matrix (spectrogram) - plot of ",a,a)'
        xlabel='frequency / Hz'
        ylabel='time / s'
      elseif (iskspec) then
        titleformat='("wavenumber/frequency - plot of ",a,a)'
        xlabel='frequency / Hz'
        ylabel='wavenumber / km\u-1'
      else
        titleformat='("phase-slowness/frequency - plot of ",a,a)'
        xlabel='frequency / Hz'
        ylabel='phase-slowness / s km\u-1'
      endif
      write(title, fmt=titleformat)
     &  filename(1:index(filename,' ')-1), ' amplitude'
      if (implot)
     &   write(title, fmt=titleformat)
     &     filename(1:index(filename,' ')-1), ' imaginary part'
      if (replot)
     &   write(title, fmt=titleformat) 
     &     filename(1:index(filename,' ')-1), ' real part'
      if (phaseswitch)
     &   write(title, fmt=titleformat) 
     &     filename(1:index(filename,' ')-1), ' phase'
      if (phasecolor)
     &   write(title, fmt=titleformat) 
     &     filename(1:index(filename,' ')-1), ' complex coefficient'
      if (deftitle) title=titlestring
      if (defxlab) xlabel=xlabelstring
      if (defylab) ylabel=ylabelstring
c 
c plot box and box title and labels
c ---------------------------------
c
      call pgenv(minf,maxf,mins,maxs,0,-2)
      call pgqvp(0,pg_vpl,pg_vpr,pg_vpb,pg_vpt)
      pg_vpr=1.-(1.-pg_vpr)*par_wedgeheight
      call pgsvp(pg_vpl,pg_vpr,pg_vpb,pg_vpt)
      call pgbox('BCNTS',0.0,0,'BCNTS',0.0,0)
      if (phasecolor) then
        call pglab(xlabel, ylabel, ' ')
        call pgmtxt('T',3.,0.5,0.5,title)
      else
        call pglab(xlabel, ylabel, title)
      endif
      if (subtitlestring.ne.'NSP') then
        call pgsave
        call pgsch(0.8*charheight)
        call pgmtxt('T', 1., 0.5, 0.5, subtitlestring)
        call pgunsa
      endif
c 
c go for the full colored or grayscale plot including wedge annotation
c --------------------------------------------------------------------
c
      transform(1)=minf-dfre
      transform(3)=dfre
      transform(2)=0.
      transform(4)=mins-dslo
      transform(5)=dslo
      transform(6)=0.
      call pgupdt
      call pgsitf(0)
c 
c wedge annotations
c 
      if (phaseswitch) then
        print *,'plot grayscale phase data...'
        call pgsave
        call pgsch(par_wedgeheight)
        call grepg_phasewedg('RG', 0.3, wedgewidth, 0., 
     &    360., phasestring, whitepaper)
        call pgunsa
      elseif (plinear) then
        print *,'plot linear grayscale amplitude data...'
        call pgsave
        call pgsch(par_wedgeheight)
        call pgwedg('RI', 0.3, wedgewidth, minvalue, 
     &    maxvalue, wedgestring)
        call pgunsa
      else
c we use our own wedge for logarithmic scales
        print *,'plot log10 grayscale amplitude data...'
        call pgsave
        call pgsch(par_wedgeheight)
        call tf_pglogwedg('RI', 0.3, wedgewidth, minvalue, 
     &    maxvalue, wedgestring)
        call pgunsa
      endif
      call pgupdt
c 
c image or grayscale
c 
      if (phasecolor) then
        print *,'plotting full colored complex numbers...'
        call pgsave
        call pgsch(par_wedgeheight)
        call grepg_phasewedg('TI', 0.3, wedgewidth, 0.,
     &            360., phasestring, whitepaper)
        call grepg_phase(plotdata, plotphase, maxslo, maxfreq, 
     &            1, plotslo, 1, plotfreq,
     &            maxvalue, minvalue, transform, whitepaper)
        call pgunsa
      elseif (phaseswitch) then
        print *,'plotting phase grayscale...'
        call pgimag(plotphase, maxslo, maxfreq, 
     &            1, plotslo, 1, plotfreq,
     &            0., 360., transform)
      else
        print *,'plotting amplitude grayscale...'
        call pgimag(plotdata, maxslo, maxfreq, 
     &            1, plotslo, 1, plotfreq,
     &            minvalue, maxvalue, transform)
      endif
      if (gmtoutput) then
        print *,'write GMT xyz-file ',
     &    gmtxyzfile(1:index(gmtxyzfile,' ')),'(one-column)'
        call tfgmt_makexyz(gmtxyzfile, plotdata, maxslo, maxfreq,
     &    plotslo, plotfreq, minf, mins, dfre, dslo, .false.)
      endif
      call pgupdt
c 
c----------------------------------------------------------------------
c 
c do isolines
c
      if (isolines) then
        call pgsls(1)
        print *,'plotting isolines...'
        do 21 i=1,5
          contour(i)=minvalue+i*(maxvalue-minvalue)/6.
   21   continue
        call pgcont(plotdata, maxslo, maxfreq, 
     &              1, plotslo, 1, plotfreq,
     &              contour, -5, transform, 0)
c label contours
        if (conlab) then
          print *,'labelling contours...'
          do i=1,5
            if (plinear) then
              value=contour(i)
            else
              value=10.**contour(i)
            endif
            cl_labpp=int(log10(abs(value)))-cl_signif
            cl_labmm=nint(value/10.**cl_labpp)
            call pgnumb(cl_labmm,cl_labpp,0,cl_contlabel,cl_charinlab)
            call pgconl(plotdata, maxslo, maxfreq, 
     &                  1, plotslo, 1, plotfreq,
     &                  contour(i), transform, cl_contlabel, 
     &                  cl_intval, cl_minint)
          enddo
        endif
      endif
      call pgupdt
c----------------------------------------------------------------------
c
c do resolution borders
c
      if (resolution) then
        print *,'plotting resolution...'
        if (color) call pgsci(2)
c resolution
        startfreq=1/((maxs-mins)*linelength)
        x(1)=maxf
        y(1)=mins
        x(2)=0
        y(2)=mins
        x(3)=0.
        y(3)=maxs
        x(4)=startfreq
        y(4)=maxs
        do 30 ifreq=1,npoints-4
          x(ifreq+4)=(ifreq*(maxf-startfreq)/(npoints-4))+startfreq
          y(ifreq+4)=mins+1/(x(ifreq+4)*linelength)
  30    continue
        call pgsfs(3)
        call pgpoly(npoints,x,y)
        call pgupdt
c plane border
        startfreq=1/((maxs-mins)*tracedx)
        do 31 ifreq=1,npoints
          x(ifreq)=(ifreq*(maxf-startfreq)/npoints)+startfreq
          y(ifreq)=1/(x(ifreq)*tracedx)
  31    continue
        call pgline(npoints,x,y)
        call pgsci(1)
        call pgupdt
      endif
c 
c----------------------------------------------------------------------
c
c do grid
c
c      if (grid) then
c        if (color) then
c          call pgp_grid(2)
c        else
c          call pgp_grid(1)
c        endif
c      endif
      if (grid) then
        if (color) call pgsci(2)
        call pgsls(4)
        call pgslw(1)
        call pgbox('STG',0.0,0,'STG',0.0,0)
        call pgsls(1)
        call pgslw(setlinewidth)
        call pgsci(1)
      endif
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
      call grepg_plotallpicks
c----------------------------------------------------------------------
c 
c now to picking if desired
c 
      if (aliaspick) then
        call grepg_message(.true.,hints,'h')
        call grepg_message(.true.,pickhint,'b')
      endif
c 
  998 continue
      if (aliaspick) then
        call pgband(7,0,0.,0.,cux, cuy, cub)
        if (verbose) then
          print '(a,f6.2,a,f6.2,a)',' ** pick at ',cux,'Hz, ',cuy,'s/km'
        endif
        if ((cub.eq.'X').or.(cub.eq.'x')) then
          aliaspick=.false.
        elseif (index('pPaA',cub).gt.0) then
          call addpick(cux,cuy)
        elseif (index('dD',cub).gt.0) then
          call delpick(cux,cuy)
        elseif (index('1234567890',cub).gt.0) then
          call grepg_eraseallpicks
          call grepg_message(.false.,pickhint,'b')
          active_pick=index('1234567890',cub)
          write(pickhint, 51) active_pick
          call grepg_message(.true.,pickhint,'b')
          call grepg_plotallpicks
        elseif ((cub.eq.' ').or.(cub.eq.'r')) then
          goto 999
        elseif (cub.eq.'s') then
          call grepg_writepicks
        elseif (cub.eq.'l') then
          call grepg_pickinit
          call grepg_readpickfile
          goto 999
        elseif (index('hH',cub).gt.0) then
          pickdx=1./(cux*cuy)
          print 997,' picked value: dx=',1000.*pickdx,'m   ',
     &      '1/lambda=',1.e-3/pickdx,'1/m   ',
     &      'line means u*f*dx=1.'
  997     format(a,f7.2,a,a,f7.4,a,a)
          startfreq=1/((maxs-mins)*pickdx)
          do ifreq=1,npoints
            x(ifreq)=(ifreq*(maxf-startfreq)/npoints)+startfreq
            y(ifreq)=1/(x(ifreq)*pickdx)
          enddo   
          call pgsci(2)
          call pgslw(6)
          call pgline(npoints,x,y)
          call pgsci(1)
          call pgslw(1)
          call pgupdt
c pick flexural wave dispersion curve
        elseif (index('bB',cub).gt.0) then
          pickdx=sqrt(cux)*cuy
c          print *,'pickdx is ',pickdx
          startfreq=(pickdx/maxs)**2
c          print *,'startfreq is ',startfreq
          do ifreq=1,npoints
            x(ifreq)=(ifreq*(maxf-startfreq)/npoints)+startfreq
            y(ifreq)=pickdx/sqrt(x(ifreq))
          enddo   
          call pgsci(3)
          call pgslw(6)
          call pgline(npoints,x,y)
          call pgsci(1)
          call pgslw(1)
          call pgupdt
c plot phasor walkout
        elseif (cub.eq.'w') then
          extdev=pgp_open(outdevice)
          if (extdev.gt.0) then
            call grepg_phasor(cux,cuy)
            call pgclos
          endif
          call pgslct(maindev)
c plot cross section
        elseif (cub.eq.'c') then
          ifreq=max(1,min(nfreq,int((nfreq-1)*(cux-minf)/(maxf-minf))))
          call pgsci(3)
          call pgslw(6)
          call pgmove(cux, mins)
          call pgdraw(cux, maxs)
          call pgsci(1)
          call pgslw(1)
          call pgupdt
          print *,' cross-section at frequency ',ifreq,' (=',cux,'Hz)'
          extdev=pgp_open(outdevice)
          call pgsch(2.0)
          call pgslw(setlinewidth)
          call pgask(.false.)
          call pgsubp(1,2)
          do islo=1,nslo
            xcross(islo)=(islo-1)*(maxs-mins)/(nslo-1)+mins
            ycross(islo)=real(data(islo, ifreq))
          enddo
          mincross=ycross(1)
          maxcross=ycross(1)
          do islo=1,nslo
            mincross=min(mincross,ycross(islo))
            maxcross=max(maxcross,ycross(islo))
          enddo
          if (abs(maxcross-mincross).lt.1.e-20) then
            print *,'real part seems to be rather small'
            maxcross=1.e-6
            mincross=-1.e-6
          endif
          call pgenv(xcross(1),xcross(nslo),mincross,maxcross,0,2)
          write(title, 50) cux
          call pglab('p / s km\u-1','Re[A(p)]',title)
          call pgline(nslo, xcross, ycross)
          do islo=1,nslo
            xcross(islo)=(islo-1)*(maxs-mins)/(nslo-1)+mins
            ycross(islo)=imag(data(islo, ifreq))
          enddo
          mincross=ycross(1)
          maxcross=ycross(1)
          do islo=1,nslo
            mincross=min(mincross,ycross(islo))
            maxcross=max(maxcross,ycross(islo))
          enddo
          if (abs(maxcross-mincross).lt.1.e-20) then
            print *,'imaginary part seems to be rather small'
            maxcross=1.e-6
            mincross=-1.e-6
          endif
          call pgenv(xcross(1),xcross(nslo),mincross,maxcross,0,2)
          call pglab('p / s km\u-1','Im[A(p)]',title)
          call pgline(nslo, xcross, ycross)
          call pgclos
          call pgslct(maindev)
        endif
        goto 998
      endif
      call pgend
      stop
   99 stop 'ERROR: commandline argument'
   98 stop 'ERROR: missing commandline argument'
   50 format('cross-section for ',f10.6,' Hz')
   51 format('pick curve no. ',i2)
      end
c


c----------------------------------------------------------------------
c 
c this routine reads in the data
c 
c 01/07/99   allow taper files
c
      subroutine greenread(filename, debug,
     &     maxslo, maxfreq, slo, om,
     &     nslo, nom, green, weight, isgrereso, isgabor,
     &     iskspec)
c 
      include 'grepg_dim.inc'
c
      character filename*(*)
      logical debug
      integer maxslo, maxfreq
c      complex green(maxfreq, maxslo)
      complex green(maxslo, maxfreq)
      real weight(maxslo, maxfreq)
      real om(maxfreq), slo(maxslo)
      integer nslo, nom
      integer inmagic, cpu, match
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
      logical isgreen,isgrereso,isgabor,iskspec
      character*4 incmagic
      real pi2
      parameter(pi2=2.*3.141592653589)
c 
      print *,'read coefficient file ',filename(1:index(filename, ' '))
      open(lu, file=filename, form='unformatted', status='old', err=99)

c check byte sex
      isgrereso=.false.
      isgabor=.false.
      iskspec=.false.
      read(lu, err=98, end=97) inmagic
      if (debug) then
        print *,'DEBUG: read magic number: ',inmagic,incmagic
      endif
      call tf_bytesex(cmagic, inmagic, cpu, match)
      if (cpu.eq.1) then
        print *,'running on Intel...'
      elseif (cpu.eq.2) then
        print *,'running on Motorola...'
      else
        stop 'ERROR: unknown processor type...'
      endif
      if (match.eq.1) then
        print *,'matching bytesex - good...'
        isgreen=.true.
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        print *,'bytesex read (',incmagic,') is not coefficient file'
        isgreen=.false.
        call tf_bytesex(pcmagic, inmagic, cpu, match)
        if (match.eq.1) then
          print *,'matching bytesex (grereso output) - good...'
          isgreen=.true.
          isgrereso=.true.
          iskspec=.false.
        elseif (match.eq.2) then 
          print *,'bytesex not matching (grereso output)',
     &            ' - we will have to swap!'
          stop 'ERROR: do not know how to do that...'
        else
          isgreen=.false.
          isgrereso=.false.
          iskspec=.false.
          print *,'bytesex read (',incmagic,') ist not grereso file'
          call tf_bytesex(gcmagic, inmagic, cpu, match)
          if (match.eq.1) then
            print *,'matching bytesex (gabor output) - good...'
            isgreen=.true.
            isgabor=.true.
          elseif (match.eq.2) then 
            print *,'bytesex not matching (gabor output)',
     &              ' - we will have to swap!'
            stop 'ERROR: do not know how to do that...'
          else
            isgreen=.false.
            isgrereso=.false.
            iskspec=.false.
            print *,'bytesex read (',incmagic,') ist not grereso file'
            call tf_bytesex(kcmagic, inmagic, cpu, match)
            if (match.eq.1) then
              print *,'matching bytesex (wavenumber spectrum) - good...'
              isgreen=.true.
              isgabor=.false.
              iskspec=.true.
            elseif (match.eq.2) then 
              print *,'bytesex not matching (wavenumber spectrum)',
     &                ' - we will have to swap!'
              stop 'ERROR: do not know how to do that...'
            else
              print *,'bytesex read (',incmagic,
     &          ') ist not wavenumber coefficient file'
              isgreen=.false.
              isgabor=.false.
              iskspec=.false.
            endif
          endif
        endif
      endif
c 
      if (isgreen) then
        read(lu, err=98, end=97) nom, nslo
        if (debug) then
          print *,'DEBUG: nom=',nom,' maxfreq=',maxfreq
          print *,'DEBUG: nslo=',nslo,' maxslo=',maxslo
        endif
        if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
          close(lu, err=96)
          stop 'ERROR: data exceeds array bounds'
        endif
        read(lu, err=98, end=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
c      read(lu, err=98, end=97) ((green(i,j), i=1,nom), j=1,nslo)
        read(lu, err=98, end=97) ((green(j,i), i=1,nom), j=1,nslo)
        close(lu, err=96)
        print *,'coefficient file read and closed'
c convert gabor time values to 1.e3s unit (will be scaled back to 1s in the
c main program)
        if (isgabor) then
          do j=1,nslo
            slo(j)=slo(j)*1.e-3
          enddo
        endif
      else
        print *,'file might be weight file'
        call tf_bytesex(wcmagic, inmagic, cpu, match)
        if (match.eq.1) then
          print *,'matching bytesex of weight file - good...'
        elseif (match.eq.2) then 
          print *,'bytesex not matching - we will have to swap!'
          stop 'ERROR: do not know how to do that...'
        else
          close(lu, err=96)
          print *,'bytesex read (',incmagic,') ist not weight'
          stop 'ERROR: your adventure terminates in an early state...'
        endif
        read(lu, err=88, end=87) nom, nslo
        if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
          close(lu, err=86)
          stop 'ERROR: data exceeds array bounds'
        endif
c      read(lu, err=98, end=97) ((green(i,j), i=1,nom), j=1,nslo)
        read(lu, err=88, end=87) ((weight(j,i), i=1,nom), j=1,nslo)
        close(lu, err=86)
        print *,'weight file read and closed'
        do i=1,nom
          om(i)=float(i)*pi2
          do j=1,nslo
            green(j,i)=cmplx(weight(j,i),0.)
          enddo
        enddo
        do j=1,nslo
          slo(j)=float(j)*1.e-3
        enddo
      endif

      return
   99 stop 'ERROR: opening coefficient file'
   98 stop 'ERROR: reading coefficient file'
   97 stop 'ERROR: reading coefficient file - unexpected end'
   96 stop 'ERROR: closing coefficient file'
   88 stop 'ERROR: reading weight file'
   87 stop 'ERROR: reading weight file - unexpected end'
   86 stop 'ERROR: closing weight file'
      end

c----------------------------------------------------------------------
c 
c comments on the file format
c
      subroutine grepg_filecomment
c
      include 'grepg_dim.inc'
c
      print *,' '
      print *,'File Formats used by grepg:'
      print *,' '
      print *,'omega,p-spectrum (magic number: ',cmagic,')'
      print *,'  typically created by ''greda'' or ''syg''.'
      print *,' '
      print *,'  All frequencies are given as angular frequencies.'
      print *,'  Their unit is 1/s. All phase slowness values are'
      print *,'  given in 1s/m=1.e3s/km.'
      print *,' '
      print *,'  Frequencies as well as slowness values are given'
      print *,'  explicitely in the data file. However they may not'
      print *,'  be set to arbitrary values. They may start and end'
      print *,'  with any value, but must be sampled in equal'
      print *,'  intervals that are calculated from the first'
      print *,'  interval of frequencies and slownesses respectively.'
      print *,' '
      print *,'omega,p-taper/weights (magic number: ',wcmagic,')'
      print *,'  as being used in ''gremlin''.'
      print *,' '
      print *,'p,p-resolution (magic number: ',pcmagic,')'
      print *,'  as created by ''grereso''.'
      print *,' '
      print *,'  In the case of grereso files verbose messages'
      print *,'  regarding frequency concern the test-slowness value.'
      print *,' '
      print *,'Gabor matrix (magic number: ',gcmagic,')'
      print *,'  as created by ''gabor''.'
      print *,' '
      print *,'  In the case of gabor files verbose messages regarding'
      print *,'  phase slowness concern time.'
      print *,'  Time values (gabor matrix) are given in 1s.'
      print *,' '
      print *,'Wavenumber spectrum (magic number: ',kcmagic,')'
      print *,'  as used by ''flgevas''.'
      print *,' '
      print *,'  In the case of wavenumber coefficient files verbose'
      print *,'  messages regarding slowness concern wavenumber.'
      call grepg_fourierinfo
      return
      end
       
