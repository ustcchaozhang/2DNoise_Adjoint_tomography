c this is <greda.f>
c------------------------------------------------------------------------------
c
c Copright 1997,2010 by Thomas Forbriger (IfG Stuttgart)
c
c wavefield transform for single-shot profiles
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
c    24/06/97   V1.0   Thomas Forbriger
c    03/07/97   V1.1   included hankel functions
c    08/07/97   V1.2   correct 1/2 factors for hankel functions
c    25/07/97   V1.3   calculate Fourier Bessel transform
c                      first slowness to be calculated is larger than zero
c                      included distance taper
c                      included special handling for u=zero
c    04/08/97   V1.4   now optional matrix inversion method
c                      and correct linear inversion
c    07/08/97   V1.5   build a correct algorithm using wielandt gauss
c    08/08/97   V2.0   reorganized the whole thing
c                      added edge setting
c    14/08/97   V2.1   added direct matrix method
c                      added K_0 damping method
c                      added discrete gram method
c    22/10/97   Vx.x   special version using imsl
c                      change matrix routine and L-rho
c    23/10/97   V2.2   - use double precision (real) matrix methods
c                      - introduced a workspace common block
c    29/10/97   V2.3   - introduced definition for damping factor
c                        relativ to omega, r_max and s_max
c    30/10/97   V2.4   - all methods should be correct now
c    31/10/97   V2.5   - now with gaussian distance taper
c                      - and gaussian time domain taper
c    06/11/97   V2.6   - starting work on Henry, Orcutt Parker method
c                      - fix problem with frequency=zero
c    12/01/98   V2.7   - fix use of minr (to be able to more than
c                        on receiver at the same offset)
c                      - rearrange help and add some more help
c                      - new option -Q
c                      - stacking of seismograms with same offset
c                      - allow different numbers of samples
c    14/01/98   V2.8   - be more verbose when stacking
c    08/06/98   V3.0   - added the plane wave stacking algorithm
c                        new major version as option naming changed
c    12/08/98   V3.1   - ok there was false information in the help text...
c                        it was a misuse of -P option
c    18/08/98   V3.2   - correct stacking algorithm 
c                        did refer to r(i) and not to r(i)/n
c    28/02/99   V3.3   - allow negative slowness values when stacking plane
c                        waves
c                      - changed order of if,elseif,.. section setting
c                        planewaves to the front
c    02/03/99   V3.4   - there was a servere bug in the spectra calculation
c                        routine - the transform array was not initialized
c                        correctly
c    13/05/99   V3.5   - allow scaling of individual spectral coefficients
c    20/04/00   V3.6   - there was an error in trace sorting in subroutine
c                        parker which should have lead to wrong results with
c                        non-sorted offset data
c                      - and gram(ntr,ntr) was zero
c    27/04/00   V3.7   - report HOP expansion numbers
c                      - scale Fourier Bessel transform with HOP factors
c    29/04/00   V3.8   - hooo - there was still an error in the HOP gram
c                        inverse expansion to alpha
c    24/05/00   V3.9   - changed cosine distance taper to apply factor greater
c                        than zero to last trace
c    21/06/00   V3.10  the trapezoid rule in subroutine backcoeff was awfully
c                      misbehaved
c    23/06/00   V3.11  trapezoid rule was still wrong by a constant factor 2
c    12/06/02   V3.12  two years later :-)
c                      Introduce a "number of wavelength" taper (option -W).
c                      This functionality is implemented into the subroutines
c                      planestack for the Slant Stack algorithm and into
c                      subroutine expmodel. In expmodel it makes only sense
c                      together with the Bessel transform, since only in that
c                      case the inverse of the Gram matrix is diagonal.
c    13/09/02   V3.13  - add extra offset taper
c                      - output seismogram spectra
c    16/09/02   V3.14  - write phasor walkout to file
c    28/03/06   V3.15  - apply special (offset dependent taper) after
c                        rescaling waveforms
c                      - corrected subroutine specialtap
c    27/11/09   V3.16  - some corrections to satify gfortran
c               V3.17  - pwo_init takes an argument!
c    10/03/10   V3.19  - correction in taper function: second index of array
c                        spectra is sample index
c    13/11/10   V3.20  - do not expose misleading term "green" to the
c                        user
c                      - use GSL instead of numerical recipes
c    30.12.2010 V3.21  - implemented libfapidxx
c    12/01/2011 V3.21b - added definition of Fourier transformation 
c                        online help text
c    10/01/2012 V3.22  - implemented radial component
c                        tested with Fourier-Bessel transformation and
c                        HOP inversion both with Bessel kernel and
c                        Hankel(1) kernel; both work well or at least as
c                        well as reconstruction tests for vertical
c                        components (HOP apparently looses some signal
c                        energy due to damping); K-damping, exp-damping, and
c                        boxcar damping all producde nuemrical errors in
c                        the test; slant stack analysis does not
c                        distinguish between vertical and radial
c                        component
c
c==============================================================================
c
      program greda
c
c first we declare some general variables
c
      character*79 version
      parameter(version=
     &'GREDA   V3.22   Calculate Fourier-Bessel expansion coefficients')
c
c calculations common block
      include 'greda_dim.inc'
      include 'greda.inc'
c any
      integer i,j
      real pi2
      parameter(pi2=2.*3.141592653589793)
c 
c----------------------------------------------------------------------
c here follows everything we need for the input data hold
c
c datafile
      character*80 filename, Fourierfile, informat
      real fdata(maxsamp)
      integer idata(maxsamp)
      equivalence(fdata, idata)
      real r(maxtr), dt, tfirst, maxr, minr
c receiver chain
      integer chain(maxtr), firstrec
c 
c----------------------------------------------------------------------
c here is everything we need to perform the calculations
c
c inner product damping exponents
      real rho, rhoq, sigma, expon
c 
c----------------------------------------------------------------------
c here follows what we need to hold and write the output data
c
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='1234')
c magic number for trace spectra binary file identification
      integer spmagic
      character*4 cspmagic
      parameter(cspmagic='SP34')
c Fourier-Bessel coefficients
      character*80 greensfile
      complex green(maxslo, maxom)
      real slo(maxslo), om(maxom), omq
      real smax, fmax
c file
      integer lu
      parameter(lu=20)
c 
c selected method
      character*70 method
      character*4 kernel
c 
c----------------------------------------------------------------------
c here we go with command line parameters
c
c parameters
      real tapfrac, offtapfrac, edgefrac, minoff, stackdelta, rescaleexpo
      real hopnumberfrequency, wltaplen, wltapfrac
      real tapoffsets(4)
      real pwofreq, pwoslo
      character*(80) pwofilename, pwffilename, pwafilename
      logical hopnumberthis, backtranscale
      logical debug, overwrite, hankel1, hankel2, verbose, uzerospecial
      logical matrixmethod, lininv, offtaper, edgeset, linkinv
      logical disgram, softcosine, gausstaper, gausstime, parkermethod
      logical stackso, planewave, rescale, specrescale, hopnumbers
      logical applywltaper,specialtaper,writeFourier
      logical pwofile, pwoautofile, pwosel
c command line
      integer maxopt, lastarg, iargc
      parameter(maxopt=37)
      character*4 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-d,2h-t,2h-s,2h-n,2h-f,2h-o,2h-1,2h-2,2h-v,2h-T,2h-S,
     &  2h-M,2h-L,2h-q,2h-E,2h-K,2h-D,2h-a,2h-b,2h-g,2h-H,2h-O,2h-Q,2h-B,
     &  2h-P,2h-r,2h-F,2h-N,2h-X,2h-W,4h-tap,4h-spo,3h-pw,4h-pwf,4h-pwa,
     &  '-ty','-R'/
      data opthasarg/.FALSE.,4*.TRUE.,4*.FALSE.,.TRUE.,3*.FALSE.,2*.TRUE.,
     &  6*.FALSE.,2*.TRUE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,
     &  7*.TRUE.,.false./
      data optarg/1h-,3h10.,2h8.,1h5,4h100.,4*1h-,2h0.,3*1h-,5h1.,1.,
     &  2h1.,6*1h-,4h0.01,5h1.,1.,4*1h-,3h10.,1h-,6h1.,10.,
     &  11h0.,0.,0.,0.,4*3hnil,'sff','-'/
c 
c======================================================================
c 
c go on with executable code
c
      print *,version
      print *,'Usage: greda datafile coeffile [-ty format] [-R]'
      print *,'             [-L] [-K] [-P] [-D] [-M] [-H]'
      print *,'             [-n nslo] [-s smax] [-f fmax]'
      print *,'             [-t frac] [-T frac] [-E edge]'
      print *,'             [-W n,f] [-a] [-b] [-g]'
      print *,'             [-O minoff] [-B delta] [-r expo] [-F]'
      print *,'             [-1] [-2] [-q f,e] [-Q f,e] [-S]'
      print *,'             [-v] [-o] [-N f] [-X]'
      print *,'             [-tap o1,o2,o3,o4]'
      print *,'             [-spo filename]'
      print *,'             [-pw f,p,file]'
      print *,'             [-pwf filename]'
      print *,'             [-pwa filename]'
      print *,'or     greda -help'
      print *,'or     greda -xhelp'
c 
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, filename)
      if (filename(1:6).eq.'-xhelp') then
        call sff_help_details
        stop
      else if (filename(1:5).eq.'-help') then
        print *,' '
        print *,'Calculate Fourier-Bessel expansion coefficients'
        print *,'for a single-shot seismic profile'
        print *,'by using linear inverse theory. Actually does'
        print *,'a Fourier-Bessel transform by default.'
        print *,' '
        print *,'Copyright 1997,2010 by Thomas Forbriger'
        print *,' '
        print *,'datafile     Name of file containing seismograms.'
        print *,'             The program will expect a homogeneous dataset.'
        print *,'             This means a dataset where all traces have'
        print *,'             the same time of first sample and the same'
        print *,'             sampling interval.'
        print *,'coeffile     Name of file to contain results.'
        print *,'-ty format   input file format (see list below)'
        print *,' '
        print *,'-R           radial component seismograms of a vertical'
        print *,'             single force or an explosion are to be'
        print *,'             transformed.'
        print *,'             (tested for HOP-inversion and'
        print *,'             Fourier-Bessel transformation)'
        print *,' '
        print *,'Available alternatives to the Fourier-Bessel transform:'
        print *,'-L           Calculate by linear inversion (exp-damping).'
        print *,'-K           Calculate by linear inversion (K_0-damping).'
        print *,'-H           Use the method following Henry, Orcutt and Parker:'
        print *,'             Calculate by linear inversion (Lorentz-damping).'
        print *,'             This uses a special integral formula that'
        print *,'             leads to a direct matrix inversion.'
        print *,'             The theory is given in:'
        print *,'               Henry, M., Orcutt, J.A., Parker, R.L., 1980,'
        print *,'               A new method for slant stacking refraction data'
        print *,'               Geoph. Res. Lett., vol. 7, 1073-1076'
        print *,'-D           Calculate by linear inversion (gram matrix'
        print *,'             calculated by discrete numerical integration).'
        print *,'-M           Calculate coefficient matrix by simple matrix'
        print *,'             inversion. (This is a DIRTY way - please don''t'
        print *,'             take it!)'
        print *,'-P           Assume that data represents plane waves. Solve'
        print *,'             by ordinary stacking method (well known as'
        print *,'             ''slant stack'').'
        print *,' '
        print *,'How to define the frequency-slowness range:'
        print *,'-n nslo      Number of slowness for which coefficients'
        print *,'             are calculated. (default is number of'
        print *,'             seismic traces read)'
        print *,'-s smax      Maximum slowness value iin km/s for which'
        print *,'             coefficients are calculated. (default is ',
     &          optarg(3)(1:4),')'
        print *,'-f fmax      Maximum frequency value in Hz. (default'
        print *,'             is to take all theoretical frequencies)'
        print *,' '
        print *,'How to apply tapers to the input data:'
        print *,'-t tfrac     Tapering fraction for each end of the time'
        print *,'             series in percent. (default is ',
     &          optarg(2)(1:4),')'
        print *,'             In the case of a gaussian taper this value gives'
        print *,'             the amplitude fraction of the edge samples'
        print *,'             compared to it''s original values.'
        print *,'-T ofrac     Tapering (cosine taper) fraction for the far end'
        print *,'             of the profile in percent.'
        print *,'             In the case of a gaussian taper this value gives'
        print *,'             the amplitude fraction of the last trace'
        print *,'             compared to it''s original value.'
        print *,'             The default is not to apply any offset domain'
        print *,'             taper.'
        print *,'-tap o1,o2,o3,o4'
        print *,'             special offset taper'
        print *,'             the taper is 0. for offsets smaller than'
        print *,'               o1 and offsets larger than o4'
        print *,'             the taper i 1. for offsets between o2'
        print *,'               and o3'
        print *,'             it has a sine edge between o1 and'
        print *,'               and o2 and a cosine edge between o3'
        print *,'               and o4'
        print *,'-E edge      Specify the sample from which on all'
        print *,'             samples should be tapered to zero as a'
        print *,'             fraction of the time series length.'
        print *,'             (2.*tfrac <= edge <= 1.)'
        print *,'-a           Apply smooth cosine distance taper ',
     &            '(default is hard).'
        print *,'-b           Apply gaussian distance taper ',
     &            '(default is hard cosine).'
        print *,'-g           Apply gaussian time domain taper ',
     &            '(default is cosine).'
        print *,'-W n,f       Apply an offset domain cosine taper of'
        print *,'             exactly ''n'' wavelengths length, where'
        print *,'             ''n'' may be a floating point value.'
        print *,'             The taper fraction (falling edge) is'
        print *,'             given by ''f'' in percent of the taper'
        print *,'             length. The taper length depends then on'
        print *,'             frequency and slowness.'
        print *,'             This option applies to only to the Slant'
        print *,'             Stack and the Bessel transformation.'
        print *,'             In terms of slowness resolution ''n'' '
        print *,'             defines it to be'
        print *,'             delta p = p / n.'
        print *,'-O minoff    Set minimum offset difference that will be'
        print *,'             used to find the minimum receiver distance.'
        print *,'             That is usefull in cases where multiple'
        print *,'             receivers occur at the same offset position.'
        print *,'             (default is ',
     &          optarg(22)(1:4),')'
        print *,'-B delta     Stack all traces lying within the same offset'
        print *,'             interval delta. We will take the mean offset'
        print *,'             value for the stacked result.'
        print *,'-r expo      Rescale seismograms to energy damping of'
        print *,'             r^(-expo) with r being the offset. For'
        print *,'             surface waves without dissipation expo should'
        print *,'             be one.'
        print *,'-F           Do rescaling in the frequency domain to energy'
        print *,'             damping of (omega*r)^(-expo). In this'
        print *,'             case every single Fourier coefficient will'
        print *,'             be rescaled for itself. This procedure will'
        print *,'             just leave the phase information. No amplitude'
        print *,'             information will be conserved.'
        print *,' '
        print *,'Parameters defining the expansion:'
        print *,'-1           Use the Hankel function H^(1)_0 instead of J_0'
        print *,'             (see remark on Fourier transformation below)'
        print *,'-2           Use the Hankel function H^(2)_0 instead of J_0'
        print *,'             (see remark on Fourier transformation below)'
        print *,'-q f,e       Damping factor for inner product.'
        print *,'             The inner product damping will be calculated'
        print *,'             as rho=delta_r_min*f*omega**e.'
        print *,'             default is: ',optarg(14)(1:8)
        print *,'-Q f,e       Damping factor for inner product.'
        print *,'             The inner product damping will be calculated'
        print *,'             as rho=f*omega**e. Overrides settings of'
        print *,'             -q option.'
        print *,'             proposed: ',optarg(23)(1:8)
        print *,'-S           Special handling of zero slowness. This option'
        print *,'             is useful together with the hankel functions'
        print *,'             which become singular at argument zero.'
        print *,'             To calculate the coefficients at'
        print *,'             slowness zero we use the Bessel function.'
        print *,'-X           scale Fourier Bessel transform with HOP factors'
        print *,'             (crazy option!)'
        print *,' '
        print *,'Phasor walkout:'
        print *,'-pw f,p,file write phasor walkout for frequency f and'
        print *,'             slowness p to file'
        print *,'-pwf file    read phasor walkout selection from file'
        print *,'             each line has three entries:'
        print *,'             frequency, slowness, filename'
        print *,'-pwa file    read phasor walkout selection from file'
        print *,'             but generate output filenames automatic'
        print *,'             each line has two entries:'
        print *,'             frequency, slowness'
        print *,' '
        print *,'Other parameters:'
        print *,'-o           Overwrite existing output file.'
        print *,'-v           Be somehow verbose.'
        print *,'-N f         print HOP numbers for frequency f'
        print *,'-spo filename'
        print *,'             write seismic traces'' Fourier'
        print *,'             coefficients to file ''filename'' '
        print *,' '
        print *,'How it works:'
        print *,'In cases -L, -K, -H and -D (linear inversion) we use'
        print *,'a scalar product of the type'
        print *,' '
        print *,'  (f,g) = int_0^P f(p) g(p) D(p) p dp.'
        print *,' '
        print *,'J_0(p*omega*r_j) is called the representer of the data'
        print *,'value d_j, which here is the Fourier expansion coefficient'
        print *,'for the vartical displacement at frequency omega and'
        print *,'offset r_j.'
        print *,' '
        print *,'In case -D P is equal to the maximum slowness and D(p)=1.'
        print *,'In the other cases P is infinity and D(p) is given by'
        print *,' '
        print *,'  D(p) = exp(-rho^2*p^2),         (case -L)'
        print *,'  D(p) = K_0(rho*p), and          (case -K)'
        print *,'  D(p) = 1/(p^2+1/rho^2).         (case -H)'
        print *,' '
        print *,'rho should be chosen somewhere around delta_r_min*omega,'
        print *,'where delta_r_min is the minimal offset difference.'
        print *,' '
        print *,'Definition of the Fourier transformation:'
        print *,'The Fourier transformation used in this program and in'
        print *,'related programs (like gremlin, syg, and gresy) is'
        print *,'defined as'
        print *,' '
        print *,'  U(omega) = int_-infnity^+infnity u(t) exp(-i*omega*t) dt'
        print *,' '
        print *,'Theoretical descriptions of wave propagation often use'
        print *,'exp(i*omega*t) as transform kernel instead of exp(-i*omega*t)'
        print *,'in order to make positive wavenumbers equivalent to wave'
        print *,'propagation in positive coordinate direction. The Fourier'
        print *,'coefficients calculated by this program consequently are'
        print *,'the complex conjugates of those used in theory. Where'
        print *,'H^(2)_0 is used in theory, you have to use H^(1)_0'
        print *,'in greda.'
        print *,' '
        call pwo_cvsid
        print *,' '
        print *,'compiled array dimensions are:'
        print *,'      samples: ',maxsamp
        print *,'       traces: ',maxtr
        print *,'   slownesses: ',maxslo
        print *,'  frequencies: ',maxom
        print *,' '
        print *,'magic numbers:'
        print *,'  Fourier-Bessel coefficient files: ',cmagic
        print *,'  Trace coefficient files: ',cspmagic
        print *,' '
        call sff_help_formats
        stop
      endif
c 
c
c----------------------------------------------------------------------
c 
c configure from command line
c 
c      print *,'DEBUG: call iargc'
      if (iargc().lt.2) stop 'ERROR: missing parameters'
c      print *,'DEBUG: call tf_cmdline'
      call tf_cmdline(3, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
c      print *,'DEBUG: returned from tf_cmdline'
      call getarg(1, filename)
      call getarg(2, greensfile)
      debug=optset(1)
      if (debug) print *,'DEBUG: in options - place 1'
      read(optarg(2), *, err=99) tapfrac
      if (tapfrac.gt.50.) stop 'ERROR: silly taper'
      if (tapfrac.lt.0.) stop 'ERROR: negative taper'
      read(optarg(3), *, err=99) smax
      smax=smax*0.001
      read(optarg(4), '(i10)', err=99) nslo
      if (nslo.lt.1) stop 'ERROR: need positive number of slownesses'
      if (nslo.gt.maxslo) stop 'ERROR: too many slownesses'
      read(optarg(5), *, err=99) fmax
      if (fmax.lt.0.) stop 'ERROR: negative maximum frequency'
      overwrite=optset(6)
      if (debug) print *,'DEBUG: in options - place 2'
      hankel1=optset(7)
      hankel2=optset(8)
      verbose=optset(9)
      offtaper=optset(10)
      if (offtaper) then
        read(optarg(10), *, err=99) offtapfrac
        if (offtapfrac.gt.50.) stop 'ERROR: silly taper'
        if (offtapfrac.lt.0.) stop 'ERROR: negative taper'
      endif
      if (debug) print *,'DEBUG: in options - place 3'
      uzerospecial=optset(11)
      matrixmethod=optset(12)
      lininv=optset(13)
      read (optarg(14), *) sigma,expon
      edgeset=optset(15)
      if (edgeset) then
        read(optarg(15), *, err=99) edgefrac
        if (edgefrac.gt.1.) stop 'ERROR: end of taper behind last sample'
        if (edgefrac.lt.(0.02*tapfrac)) 
     &    stop 'ERROR: edge does not leave enough space for taper'
      endif
      if (debug) print *,'DEBUG: in options - place 4'
      linkinv=optset(16)
      disgram=optset(17)
      softcosine=optset(18)
      gausstaper=optset(19)
      gausstime=optset(20)
      parkermethod=optset(21)
      read (optarg(22), *) minoff
      if ((.not.optset(14)).and.(optset(23)))
     &  read (optarg(23), *) sigma,expon
      stackso=optset(24)
      if (stackso) read (optarg(24), *) stackdelta
      planewave=optset(25)
      rescale=optset(26)
      if (debug) print *,'DEBUG: in options - place 5'
      specrescale=optset(27)
      if (rescale) then
        read(optarg(26), *) rescaleexpo
        if (specrescale) rescale=.false.
      else
        specrescale=.false.
      endif
      hopnumbers=optset(28)
      read(optarg(28), *) hopnumberfrequency
      backtranscale=optset(29)
      applywltaper=optset(30)
      read(optarg(30), *) wltaplen, wltapfrac
      if (debug) print *,'DEBUG: in options - place 6'
      specialtaper=optset(31)
      if (specialtaper) read(optarg(31), *) (tapoffsets(i), i=1,4)
      writeFourier=optset(32)
      Fourierfile=optarg(32)
      pwosel=optset(33)
      if (pwosel) read(optarg(33), *) pwofreq, pwoslo, pwofilename
      pwofile=optset(34)
      pwffilename=optarg(34)
      pwoautofile=optset(35)
      pwafilename=optarg(35)
      informat=optarg(36)
      radial=optset(37)
      if (debug) print *,'DEBUG: read options'
c 
      if ((.not.(planewave)).and.(smax.lt.0.))
     &  stop 'ERROR: negative maximum slowness'

c----------------------------------------------------------------------
c initialize common block for phasor walkout
c      print *,'DEBUG: call pwo_init'
      call pwo_init(verbose)
c      print *,'DEBUG: returned from pwo_init'
c----------------------------------------------------------------------
c report
      print *,' '
      if (radial) then
        if (hankel1) then
          print *,'I will use the Hankel function H^(1)_1 of order one'
          kernel='H1_1'
        elseif (hankel2) then
          print *,'I will use the Hankel function H^(2)_1 of order one'
          kernel='H2_1'
        else
          print *,'I will use the Bessel function of the first kind',
     &      ' J_1 of order one'
          kernel='J_1'
        endif
      else
        if (hankel1) then
          print *,'I will use the Hankel function H^(1)_0 of order zero'
          kernel='H1_0'
        elseif (hankel2) then
          print *,'I will use the Hankel function H^(2)_0 of order zero'
          kernel='H2_0'
        else
          print *,'I will use the Bessel function of the first kind J_0',
     &      ' of order zero'
          kernel='J_0'
        endif
      endif
      print *,' '
c 
c initialize wavelength taper factors
      if (debug) print *,'DEBUG: call initwltaper(wltaplen,wltapfrac)'
      call initwltaper(wltaplen,wltapfrac)
c
c----------------------------------------------------------------------
c phasor walkout selection
c
      if (pwosel) 
     &  call pwo_selpair(pwofreq*pi2,pwoslo,pwofilename,verbose)
      if (pwofile) call pwo_readsel(pwffilename, .false., verbose)
      if (pwoautofile) call pwo_readsel(pwafilename, .true., verbose)
c 
c----------------------------------------------------------------------
c 
c go for the real calculations
c 
c read seismic data
      if (debug) print *,'DEBUG: call readdata'
      call sff_select_input_format(informat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting input file format'
      call readdata(filename, fdata, idata, spectra, r, maxr,
     &     maxtr, maxsamp, ntr, nsamp, dt, tfirst)
      if (debug) print *,'DEBUG: returned from readdata'
c 
      if ((.not.optset(4)).or.(matrixmethod)) nslo=ntr
      if (nslo.gt.maxslo) stop 'ERROR: too many slownesses - check code'
c 
c do stacking
      if (stackso) call stackthem(r, stackdelta, maxr, verbose)
c
c get distances in increasing order
      call tf_rchain(r, chain, ntr, firstrec, 1)
c
c find minimum distance step
      j=firstrec
      i=chain(j)
      minr=maxr
      do while (i.gt.0)
        if (abs(r(i)-r(j)).gt.minoff) minr=min(minr,abs(r(i)-r(j)))
        j=i
        i=chain(i)
      enddo
      print *,' '
      print *,' minimum distance between receivers: ',minr
      print *,'maximum source to receiver distance: ',maxr
c 
      if ((.not.optset(14)).and.(optset(23)))
     &  sigma=sigma/minr
c 
      print *,'rho=',minr*sigma,'*omega**',expon
c 
c apply tapers
      call taper(maxsamp, maxtr, ntr, nsamp, tapfrac, offtapfrac,
     &  maxr, spectra, r, offtaper, verbose, edgeset, edgefrac,
     &  softcosine, gausstaper, gausstime)
c 
c rescale traces if we want them to be rescaled
      if (rescale) call dorescale(rescaleexpo, r)
c
c apply special tapers
      if (specialtaper) 
     &  call specialtap(maxsamp, maxtr, ntr, nsamp, tapoffsets, maxr,
     &    spectra, r, verbose)
c 
c calculate complex spectra
      call calcspec(dt, tfirst, om,
     &  fmax, optset(5))
c
c write Fourier coefficients
      if (writeFourier) 
     &  call Fourierwrite(Fourierfile, spmagic, cspmagic, overwrite,
     &    r, maxr, om)
c 
c rescale traces if we want them to be rescaled
      if (specrescale) call dospecrescale(rescaleexpo, r, om)
c 
c set slowness range
      call setslo(maxslo, nslo, smax, slo, 
     &  hankel1, hankel2, uzerospecial)
c 
c----------------------------------------------------------------------
c 
c now calculate the Fourier-Bessel matrix
c handle each frequency individually
c
      hopnumberthis=.false.
      do i=1,nom
c 
c init phasor walkout
        call pwo_initable(om,slo,nom,nslo,i,verbose)
c 
c damping
c
c something usefull
        omq=om(i)*om(i)
        rho=minr*sigma*om(i)**expon
        rhoq=rho*rho
c 
c do we hopnumber? ;-)
        if (hopnumbers) then
          hopnumberthis=.false.
c          print *,abs(hopnumberfrequency-om(i)/pi2)
c          print *,(0.55*(om(2)-om(1))/pi2)
          if (abs(hopnumberfrequency-om(i)/pi2).lt.(0.501*(om(2)-om(1))/pi2)) 
     &    then
            hopnumberthis=.true.
            print *,' '
            print *,'HOP numbers request for ',
     &              hopnumberfrequency,' Hz comes at ',om(i)/pi2,' Hz'
            print *,'rho is ',rho
          endif
        endif
c
c algorithm section
c =================
c
c Provided algorithms are:
c
c   1. Slant Stack (assuming plane waves)
c   2. HOP-method (linear inversion with Lorentz-damping)
c   3. linear inversion with exp-damping
c   4. linear inversion with K_0-damping
c   5. linear inversion with boxcar damping
c   6. the dirty direct matrix inversion
c   7. discrete Fourier-Bessel transformation
c
c The following conditionals select one of the provided algorithms.
c The standard sequence of calculations is:
c
c   1. calculate a Gram matrix
c      (subroutines gramex, gramket, gramdis)
c   2. calculate expansion coefficients alpha from Gram matrix
c      by numerical solution of system of linear equations
c      (subroutine modexp) 
c   3. calculate model vector of linear inversion from
c      coefficients alpha and selected representer
c      (subroutine expmodel)
c   4. scale model to Fourier-Bessel matrix coefficients
c      (subroutines scalpark, scalex, scalkmet)
c
c Deviations from this scheme:
c
c   - the boxcar damping does not need scaling (step 4)
c   - the Bessel transformation combines steps 1 and two in subroutine
c     backcoeff and does not need scaling (step 4)
c   - the HOP-method knows the inverse of the Gram matrix and omits step 2
c   - the slant stack is too easy to use more than one subroutine ;-)
c     (subroutine planestack)
c   - the dirty direct inversion sets up its one system of linear equations
c     (subroutine forwardmat) and uses subroutine modexp to solve it
c     (steps 3 and 4 are not necessary)
c 
        if (planewave) then
c 
c 1. Slant Stack (assuming plane waves)
c calculate stacking algorithm with plane wave assumption
c -------------------------------------------------------
          if (i.eq.1) then
            print *,' '
            print *,'assume plane waves and use stacking algorithm...'
            print *,' '
            method='Slant Stack'
          endif
          call planestack(om(i), slo, r, green, i, applywltaper)
c 
        elseif (parkermethod) then
c 
c 2. HOP-method (linear inversion with Lorentz-damping)
c calculate by Henry Orcutt Parker integral
c -----------------------------------------
          if (i.eq.1) then
            print *,' '
            print *,'use gram matrix given by Henry, Orcutt and Parker'
            print *,' '
            method='HOP-inversion with '//kernel//' kernel'
            call nullmodel(green)
          else
            if (verbose) then
              print *,'f=',om(i)/pi2,'   rho=',rho
            endif
            rho=om(i)/rho
            rhoq=omq/rhoq
            call parker(rho, r, spectra(1, i), chain, firstrec, hopnumberthis)
            call expmodel(om(i), r, slo, green(1, i),
     &        hankel1, hankel2, uzerospecial, .false.)
            call scalpark(nslo, rhoq, omq, green(1, i), slo,
     &        hopnumberthis,ntr)
          endif
c 
        elseif (lininv) then
c 
c 3. linear inversion with exp-damping
c calculate by linear inversion exponential damping
c -------------------------------------------------
          if (i.eq.1) then
            print *,' '
            print *,'do it by linear inversion (exponential damping)'
            print *,' '
            method='inversion with exp-damping and '//kernel//' kernel'
            call nullmodel(green)
          else
            if (verbose) then
              print *,'f=',om(i)/pi2,'   rho=',rho
            endif
            call gramex(rhoq, omq, r)
            call modexp(i, spectra(1, i))
            call expmodel(om(i), r, slo, green(1, i),
     &        hankel1, hankel2, uzerospecial, .false.)
            call scalex(nslo, rhoq, green(1, i), slo, ntr)
          endif
c 
        elseif (linkinv) then
c 
c 4. linear inversion with K_0-damping
c calculate by linear inversion K_0 damping
c -----------------------------------------
          if (i.eq.1) then
            print *,' '
            print *,'do it by linear inversion (K_0-damping)'
            print *,' '
            method='inversion with K0-damping and '//kernel//' kernel'
            call nullmodel(green)
          else
            if (verbose) then
              print *,'f=',om(i)/pi2,'   rho=',rho
            endif
            call gramkmet(rhoq, omq, r)
            call modexp(i, spectra(1, i))
            call expmodel(om(i), r, slo, green(1, i),
     &        hankel1, hankel2, uzerospecial, .false.)
            call scalkmet(nslo, rho, green(1, i), slo, ntr)
          endif
c 
        elseif (disgram) then
c 
c 5. linear inversion with boxcar damping
c calculate by linear inversion with discrete gram
c ------------------------------------------------
          if (i.eq.1) then
            print *,' '
            print *,'do it by linear inversion (discrete gram)'
            print *,' '
          endif
          if (verbose) then
            print *,'going for frequency ',i,' of ',nom
          endif
          call gramdis(slo, om(i), r)
          call modexp(i, spectra(1, i))
          call expmodel(om(i), r, slo, green(1, i), 
     &      hankel1, hankel2, uzerospecial, .false.)
c 
        elseif (matrixmethod) then
c 
c 6. the dirty direct matrix inversion
c calculate Fourier-Bessel matrix by matrix inversion method
c -------------------------------------------------
          if (i.eq.1) then
            print *,' '
            print *,'do it by direct matrix inversion'
            print *,' '
            print *,'THIS IS A DIRTY WAY! You will run into trouble...'
            method='dirty direct matrix inversion'
          endif
          call forwardmat(om(i), slo, r, hankel1, hankel2, uzerospecial)
          call modexp(i, spectra(1, i))
          do j=1,nslo
            green(j, i)=alpha(j)
          enddo
c 
        else
c 
c 7. discrete Fourier-Bessel transformation
c perform a Fourier Bessel transform
c ----------------------------------
          if (i.eq.1) then
            print *,' '
            print *,'do it by inverse Fourier Bessel transform'
            method='Fourier Bessel transform with '//kernel//' kernel'
          endif
          call backcoeff((verbose.and.(i.eq.1)), r, om(i), 
     &      spectra(1, i), chain, firstrec, hopnumberthis)
          call expmodel(om(i), r, slo, green(1, i), 
     &      hankel1, hankel2, uzerospecial, applywltaper)
          if (backtranscale) then
            if (i.eq.1) then
              call nullmodel(green)
            else
              rho=om(i)/rho
              rhoq=omq/rhoq
              call scalpark(nslo, rhoq, omq, green(1, i), slo,
     &          hopnumberthis,ntr)
            endif
          endif
c 
        endif
c 
c write phasor walkout to file
        call pwo_write(overwrite, verbose, method, r, ntr)
c 
c in any case make shure that verbose is switched on only for
c first loop cycle
c        verbose=.false.
      enddo
c 
c----------------------------------------------------------------------
c 
c write Fourier-Bessel coefficients (easy to use)
c 
      print *,' '
      if (overwrite) then
        print *,'opening coefficient file ',
     &    greensfile(1:index(greensfile,' ')),
     &    ' - overwrite mode'
        open(lu, file=greensfile, form='unformatted', err=98)
      else
        print *,'opening coefficient file ',
     &    greensfile(1:index(greensfile,' '))
        open(lu, file=greensfile, status='new', form='unformatted', err=98)
      endif
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom, nslo
      write(lu, err=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
      write(lu, err=97) ((green(j,i), i=1,nom), j=1,nslo)
      close(lu, err=96)
c 
      stop
   99 stop 'ERROR: reading command line argument'
   98 stop 'ERROR: opening coefficient file'
   97 stop 'ERROR: writing coefficient file'
   96 stop 'ERROR: closing coefficient file'
      end
c
c======================================================================
c
c summary of general subroutines
c ==============================
c
c read sff seismogram traces
c --------------------------
c
c     subroutine readdata(filename, fdata, idata, spectra, r, maxr,
c    &     maxtr, maxsamp, ntr, nsamp, dt, tfirst)
c       
c stack all seismograms within stackdelta (option -B stackdelta)
c ---------------------------------------
c reads common blocks from greda.inc
c
c     subroutine stackthem(r, stackdelta, maxr, verbose)
c
c rescale Fourier coefficients (option -F)
c ----------------------------
c reads common blocks from greda.inc
c
c     subroutine dospecrescale(expon, r, om)
c
c rescale seismogram traces (option -r expon)
c -------------------------
c reads common blocks from greda.inc
c
c     subroutine dorescale(expon, r)
c
c apply time and offset domain tapering (many options)
c -------------------------------------
c
c     subroutine taper(maxsamp, maxtr, ntr, nsamp, tapfrac, offtapfrac,
c    &  maxr, spectra, r, offtaper, verbose, edgeset, edgefrac,
c    &  softcosine, gausstaper, gausstime)
c
c apply special offset domain taper (option -tap tapoffsets)
c ---------------------------------
c
c     subroutine specialtap(maxsamp, maxtr, ntr, nsamp, tapoffsets, maxr
c    &    spectra, r, verbose)
c
c calculate complex Fourier coefficients from waveform data
c ---------------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine calcspec(dt, tfirst, om, fmax, fmaxset)
c 
c write trace Fourier spectra (easy to use) (option -spo filename)
c ---------------------------------------­-
c reads common blocks from greda.inc
c 
c     subroutine Fourierwrite(filename, magic, cmagic, overwrite,
c    &  r, maxr, om)
c
c set slowness values for different expansion cases
c -------------------------------------------------
c fills the array slo with appropriate values
c
c     subroutine setslo(maxslo, nslo, smax, slo, 
c    &  hankel1, hankel2, uzerospecial)
c
c set coefficients for all slowness values to zero
c ------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine nullmodel(ogreen)
c
c expands the model, using alpha as expnsion coefficients and
c J_0, H^1_0, H^2_0 as a representer
c -----------------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine expmodel(omega, r, slo, ogreen, 
c    &  hankel1, hankel2, uzerospecial, applywltaper)
c
c numerically solve the system of linear equations for the gram matrix
c --------------------------------------------------------------------
c claculates the expansion coefficients alpha
c reads common blocks from greda.inc
c
c     subroutine modexp(iom, s)
c
c initialize wavelength specific taper parameters
c -----------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine initwltaper(length,fraction)
c
c set wavelength specific taper to given wavelength
c -------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine setwltaper(omega,slo,r)
c
c----------------------------------------------------------------------
c 
c summary of subroutines specific to different approaches
c =======================================================
c
c fills expansion coefficients alpha with values appropriate to
c modified Fourier-Bessel transform
c -------------------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine backcoeff(verbose, r, omega, spect, chain, firstrec,
c    &                     numbers)
c
c calculate Gram matrix for linear inversion with exp-damping 
c -----------------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine gramex(rhoq, omq, r)
c
c scale coefficient matrix appropriate to exp-damping
c ---------------------------------------------------
c
c     subroutine scalex(nslo, rhoq, ogreen, slo, ntr)
c
c calculate coefficient matrix directly by Slant Stack
c ----------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine planestack(omega, slo, r, green, iomega, applywltaper)
c
c fill Gram matrix for a direct inversion (that's a dirty method)
c ---------------------------------------
c reads common blocks from greda.inc
c
c     subroutine forwardmat(omega, slo, r, hankel1, hankel2, uzerospecial)
c
c calculate Gram matrix for linear inversion with K_0-damping
c -----------------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine gramkmet(rhoq, omq, r)
c
c scale coefficient matrix appropriate to K_0-damping
c ---------------------------------------------
c
c     subroutine scalkmet(nslo, rho, ogreen, slo, ntr)
c
c calculate Gram matrix for linear inversion with boxcar-damping
c --------------------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine gramdis(slo, om, r)
c
c function used by gramdis for numerical integration
c --------------------------------------------------
c
c     double precision function gramdisint(maxslo, nslo,
c    &  slo, rj, rk, om)
c
c calculate expansion coefficients alpha for linear inversion
c with Lorentz-damping
c -----------------------------------------------------------
c reads common blocks from greda.inc
c
c     subroutine parker(rho, r, s, chain, firstrec, numbers)
c
c scale coefficient matrix appropriate to Lorentz-damping
c -------------------------------------------------------
c
c     subroutine scalpark(nslo, rhoq, omq, ogreen, slo, numbers, ntr)
c
c======================================================================
c
c some general subroutines
c
c----------------------------------------------------------------------
c
      subroutine readdata(filename, fdata, idata, spectra, r, maxr,
     &     maxtr, maxsamp, ntr, nsamp, dt, tfirst)
c 
c read sff seismic traces
c
      character filename*(*)
      integer maxtr, maxsamp, ntr, nsamp
      real fdata(maxsamp)
      integer idata(maxsamp)
      complex*16 spectra(maxtr, maxsamp)
      real r(maxtr), dt, tfirst, maxr
c 
      integer lu, ierr, i
      real version
      parameter(lu=20)
      character timestamp*20, code*10, type*20, cs*1, date*6, time*10
      character wid2line*132
      integer srctime(7), datatime(7), tdif(7), itdif(7)
      real sc1, sc2, sc3, c1, c2, c3, tanf, idt, sffu_seconds
      integer nstack, insamp, time_compare
      logical last
c 
c open sff-file
      print *,'open sff data ',filename(1:index(filename,' '))
      call sff_ROpenS(lu, filename, version, timestamp, code,
     &  type, cs, sc1, sc2, sc3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening seismogram file'
      if (index(code, 'S').eq.0) stop 'ERROR: no SRCE line'
      if (cs.ne.'C') stop 'ERROR: source coordinates are not cartesian'
      call sffu_timesrce(date, time, srctime) 
      ntr=0
      maxr=0.
   1  continue
        ntr=ntr+1
        insamp=maxsamp
        call sff_RTraceI(lu, tanf, idt, wid2line, insamp, fdata, idata,
     &    code, last, cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading trace'
        if (index(code, 'I').eq.0) stop 'ERROR: no INFO line'
        if (cs.ne.'C') stop 'ERROR: trace coordinates are not cartesian'
        call sffu_timewid2(wid2line, datatime)
        if (ntr.eq.1) then
          dt=idt
          nsamp=insamp
          call time_sub(datatime, srctime, tdif)
          tfirst=sffu_seconds(tdif)
        else
          call time_sub(datatime, srctime, itdif)
          if (nsamp.ne.insamp)
     &      print *,'NOTICE: your time series have inconsitent numbers ',
     &        'of samples'
          nsamp=max(nsamp,insamp)
          if ((dt.ne.idt).or.
     &        (time_compare(tdif, itdif).ne.0)) then
            print *,'ERROR: sampling interval or'
            stop 'ERROR: time of first sample differs from previous trace'
          endif
        endif
        r(ntr)=sqrt((sc1-c1)**2+(sc2-c2)**2+(sc3-c3)**2)
        maxr=max(maxr,r(ntr))
        do i=1,maxsamp
          spectra(ntr, i)=(0.,0.)
        enddo
        do i=1,insamp
          spectra(ntr, i)=cmplx(fdata(i))
        enddo
        if ((ntr.lt.maxtr).and.(.not.last)) goto 1
      if (.not.(last)) then
        print *,'NOTICE: will ignore traces after ',ntr
        call sff_close(lu)
      endif
      print *,'file read and closed'
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine stackthem(r, stackdelta, maxr, verbose)
c 
c stack all seismograms within stackdelta
c 
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c 
      real r(maxtr), stackdelta, maxr
      logical verbose
c 
c something to work here
      logical obsolete(maxtr)
      integer i,j,k,n
      real refoff
c 
      print *,' '
      print *,'  stack traces within ',stackdelta,'m offset range'
      print *,'  (entered with ',ntr,' traces)'
c 
      do i=1,ntr
        obsolete(i)=.false.
      enddo
c 
      do i=1,ntr-1
        if (.not.(obsolete(i))) then
          n=1
          do j=i+1,ntr
            if (.not.(obsolete(j))) then
              refoff=r(i)/float(n)
              if (abs(r(j)-refoff).le.stackdelta) then
                n=n+1
                if (verbose) print *,'    stack ',j,' at ',r(j),' --> ',i,
     &            ' at ',r(i)
                obsolete(j)=.true.
                do k=1,nsamp
                  spectra(i,k)=spectra(i,k)+spectra(j,k)
                enddo
                r(i)=r(i)+r(j)
              endif
            endif
          enddo
          if (n.gt.1) then
            do k=1,nsamp
              spectra(i,k)=spectra(i,k)/float(n)
            enddo
            r(i)=r(i)/float(n)
            if (verbose) print *,'    ',n,' traces stacked to trace ',
     &        i,' at ',r(i)
          endif
        endif
      enddo
c 
      n=0
      do i=1,ntr
        if (.not.(obsolete(i))) then
          n=n+1
          if (n.ne.i) then
            do k=1,nsamp
              spectra(n,k)=spectra(i,k)
            enddo
            r(n)=r(i)
          endif
        endif
      enddo
c 
      ntr=n
      print *,'  ',ntr,' traces left after stacking'
c 
      maxr=r(1)
      do i=1,ntr
        maxr=max(maxr, r(i))
      enddo
c 
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine dospecrescale(expon, r, om)
c
c rescale traces Fourier coefficients
c
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real expon, r(maxtr)
      real om(maxom)
c
      double precision fact
      integer i,j
c
      print *,' '
      print *,'rescale spectral coefficients to energy content of',
     &        ' (omega*r)^(-',expon,')'
c 
      do i=1,ntr
        do j=1,nsamp
          fact=sqrt((om(j)*r(i))**(-expon))
          spectra(i,j)=spectra(i,j)*fact/abs(spectra(i,j))
        enddo
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine dorescale(expon, r)
c
c rescale traces
c
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real expon, r(maxtr)
c
      double precision sum, fact
      integer i,j
c
      print *,' '
      print *,'rescale seismic traces to energy content of r^(-',expon,')'
c 
      do i=1,ntr
        sum=0.d0
        do j=1,nsamp
          sum=sum+real(spectra(i,j))*real(spectra(i,j))
        enddo
        fact=sqrt(r(i)**(-expon)/sum)
        do j=1,nsamp
          spectra(i,j)=spectra(i,j)*fact
        enddo
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine taper(maxsamp, maxtr, ntr, nsamp, tapfrac, offtapfrac,
     &  maxr, spectra, r, offtaper, verbose, edgeset, edgefrac,
     &  softcosine, gausstaper, gausstime)
c
c apply time and offset domain tapering
c
      integer maxsamp, maxtr, ntr, nsamp
      real tapfrac, offtapfrac, maxr, edgefrac
      double complex spectra(maxtr, maxsamp)
      real r(maxtr)
      logical offtaper, verbose, edgeset, softcosine, gausstaper
      logical gausstime
c 
      integer i, l, wil, wirb, wire
      real fac, tf_sincostap, rtap, pi, ltap
      double precision sigmaq
      parameter(pi=3.141592653589793)
c 
      print *,' '
      print *,'apply taper to input data'
c 
c first apply time domain taper
c 
      wil=int(nsamp*tapfrac/100.)+1
      wire=nsamp
      if (edgeset) wire=int(nsamp*edgefrac)
      wirb=wire-wil 
      if (gausstime) then
        tapfrac=max(tapfrac,1.e-10)
        sigmaq=-1.*(wire/2)**2/log(tapfrac*0.01)
        print *,'  using ',tapfrac,'% time domain gaussian taper'
        print *,'     (zero after sample ',wire,' with ',
     &    nsamp,' samples total)'
      else
        print *,'  using ',tapfrac,'% time domain cosine taper'
        print *,'    (full scale from sample ',
     &    wil,' to ',wirb
        print *,'     zero after sample ',wire,' with ',
     &    nsamp,' samples total)'
      endif
      do i=1,nsamp
        if (gausstime) then
          fac=exp(-1.*(i-wire/2.)**2/sigmaq)
          if (i.gt.wire) fac=0.
        else
          fac=tf_sincostap(i, 1, wil, wirb, wire)
        endif
        if (verbose) print *,'  sample ',i,'   fac: ',fac
        do l=1,ntr
          spectra(l,i)=fac*spectra(l,i)
        enddo
      enddo
c 
c apply offset domain taper
      if (offtaper) then
        if (gausstaper) then
          offtapfrac=max(offtapfrac,1.e-10)
          sigmaq=-1.*maxr*maxr/log(offtapfrac*0.01)
          print *,'  apply ',offtapfrac,
     &      '% offset domain gaussian taper'
          print *,'    from 0m to ',maxr,'m'
        else
          ltap=maxr*(1.-0.01*offtapfrac)
          rtap=maxr*(1.+(0.01*offtapfrac)**2)
          if (softcosine) then
            print *,'  apply ',offtapfrac,
     &        '% offset domain soft cosine taper'
          else
            print *,'  apply ',offtapfrac,
     &        '% offset domain hard cosine taper'
          endif
          print *,'    from ', ltap,'m to ',rtap,'m'
        endif
c 
        do i=1,ntr
          fac=1.
          if (gausstaper) then
            fac=exp(-1.*r(i)*r(i)/sigmaq)
          elseif (r(i).gt.ltap) then
            if (softcosine) then
c smooth taper
              fac=0.5*(1.+cos(pi*(r(i)-ltap)/(rtap-ltap)))
            else
c hard taper
              fac=cos(0.5*pi*(r(i)-ltap)/(rtap-ltap))
            endif
          endif
          if (verbose) then
            print *,'    at ',r(i),'m   fac: ',fac
          endif
          do l=1,nsamp
            spectra(i,l)=fac*spectra(i,l)
          enddo
        enddo
      else
        print *,'  no offset domain taper will be applied'
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine specialtap(maxsamp, maxtr, ntr, nsamp, tapoffsets, 
     &    maxr, spectra, r, verbose)
c
c apply special offset domain taper
c
c 28/03/2006: order of array indices in spectra was wrong
c
      integer maxsamp, maxtr, ntr, nsamp
      real tapoffsets(4), maxr
      double complex spectra(maxtr, maxsamp)
      real r(maxtr)
      logical verbose
c 
      integer i, l
      real fac, pi
      parameter(pi=3.141592653589793)
c 
      print *,' '
      print *,'apply special offset taper to input data'
      print *,'  min=',tapoffsets(1),', left=',tapoffsets(2),
     & ' right=',tapoffsets(3),', max=',tapoffsets(4)
c 
c apply offset domain taper
c 
      do i=1,ntr
        fac=1.
        if (r(i).lt.tapoffsets(1)) then
          fac=0.
        elseif (r(i).gt.tapoffsets(4)) then
          fac=0.
        elseif (r(i).lt.tapoffsets(2)) then
          fac=sin(0.5*pi*(r(i)-tapoffsets(1))/
     &      (tapoffsets(2)-tapoffsets(1)))
        elseif (r(i).gt.tapoffsets(3)) then
          fac=cos(0.5*pi*(r(i)-tapoffsets(3))/
     &      (tapoffsets(4)-tapoffsets(3)))
        else
          fac=1.
        endif
        if (verbose) then
          print *,'    at ',r(i),'m   fac: ',fac
        endif
        do l=1,nsamp
          spectra(i,l)=fac*spectra(i,l)
        enddo
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine calcspec(dt, tfirst, om, fmax, fmaxset)
c 
c calculate complex spectra of time series data
c
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real dt, tfirst, om(maxom), fmax
      logical fmaxset
c 
      real scal, df
      integer i, j, pow, newnsamp
      real pi2
      parameter(pi2=2.*3.141592653589793)
c 
      print *,' '
      print *,'transform dataset to frequency domain'
c now get next integer power of two
      pow=0
    1 continue
        pow=pow+1
        newnsamp=2**pow
        if (newnsamp.lt.nsamp) goto 1
      if (newnsamp.gt.maxsamp)
     &  stop 'ERROR: can not fit integer power of two into samples dimension'
c 
c get correct scaling factor to manipulate FORK
      scal=dt*sqrt(float(newnsamp))
c calculate other values
      nom=newnsamp/2+1
      df=1/(newnsamp*dt)
      if (nom.gt.maxom) stop 'ERROR: too many frequencies - check code'
      print *,'  transforming ',newnsamp,' samples to ',nom,' frequencies'
      print *,'  Nyquist frequency is ',(nom-1)*df,' Hz'
c 
c set frequencies (in cycles per second)
      do i=1,nom
        om(i)=pi2*float(i-1)*df
      enddo
c 
c check fmax limit
      if (fmaxset) then
        if (((nom-1)*df).le.fmax) then
          print *,'  frequency limit is ignored as it is lower than nyquist:'
          print *,'  maximum frequency now is ',df*(nom-1),' Hz'
          print *,'  minimum frequency is 0 Hz'
        else
          nom=int(fmax/df)+1
          print *,'  frequency range is limited to ',nom,' frequencies'
          print *,'  maximum frequency now is ',df*(nom-1),' Hz'
          print *,'  minimum frequency is 0 Hz'
        endif
      endif
c
c go and transform
      do i=1,ntr
        do j=1,nsamp
          alpha(j)=scal*spectra(i,j)
        enddo
        if (nsamp.lt.newnsamp) then
          do j=nsamp+1,newnsamp
            alpha(j)=(0.d0,0.d0)
          enddo
        endif
        call tf_dfork(newnsamp, alpha, -1.d0)
        do j=1,nom
          spectra(i,j)=alpha(j)
        enddo
        if (nom.lt.maxom) then
          do j=nom+1,maxom
            spectra(i,j)=(0.d0,0.d0)
          enddo
        endif
      enddo
c      
c up to now we ignore the static time shift tfirst of the first sample
c should used for a shifting theorems application
      if (tfirst.gt.(dt*0.01)) then
        print *,'NOTICE: ignoring static time shift of ',tfirst,
     &          'seconds'
      endif
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
c write trace Fourier spectra (easy to use)
c 
      subroutine Fourierwrite(filename, magic, cmagic, overwrite,
     &  r, maxr, om)
c
      include 'greda_dim.inc'
      include 'greda.inc'
c
      character*4 cmagic
      integer magic
      character*(*) filename
      logical overwrite
      real r(maxtr), maxr, om(maxom)
c 
      integer chain(maxtr), firstinchain,i,j
      integer k(maxtr)
      integer lu
      parameter(lu=20)
c 
      print *,' '
      if (overwrite) then
        print *,'opening Fourier file ',filename(1:index(filename,' ')),
     &    ' - overwrite mode'
        open(lu, file=filename, form='unformatted', err=98)
      else
        print *,'opening Fourier file ',filename(1:index(filename,' '))
        open(lu, file=filename, status='new', 
     &    form='unformatted', err=98)
      endif
c 
c sort offsets
      call tf_rchain(r, chain, ntr, firstinchain, 1)
      j=firstinchain
      do i=1,ntr
        k(i)=j
        j=chain(j)
      enddo
c 
      call tf_magic(cmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) nom, ntr
      write(lu, err=97) (om(i), i=1,nom), (r(k(i)), i=1,ntr)
      write(lu, err=97) ((spectra(k(j),i), i=1,nom), j=1,ntr)
      close(lu, err=96)
c 
      return
   98 stop 'ERROR: opening Fourier file'
   97 stop 'ERROR: writing Fourier file'
   96 stop 'ERROR: closing Fourier file'
      end
c 
c----------------------------------------------------------------------
c 
      subroutine setslo(maxslo, nslo, smax, slo, 
     &  hankel1, hankel2, uzerospecial)
c
c set slowness values for different expansion cases
c
      integer maxslo, nslo
      real smax, slo(maxslo)
      logical hankel1, hankel2, uzerospecial
c
      integer iu
c
      print *,' '
      print *,'prepare slowness range'
c 
      do iu=1,nslo
        if ((uzerospecial).or.((.not.(hankel1)).and.(.not.(hankel2)))) then
          slo(iu)=(iu-1)*smax/(nslo-1)
        else
          slo(iu)=(iu)*smax/(nslo)
        endif
      enddo
c 
      print *,'  scanning ',nslo,' slowness values from ',slo(1)*1.e3,
     &  ' to ',slo(nslo)*1.e3,' s/km'
c 
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine nullmodel(ogreen)
c 
c set model to zero (for critical cases like omega=zero)
c
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      complex ogreen(maxslo)
c 
      integer iu
c 
c evaluate expansion for every slowness
      do iu=1,nslo
        ogreen(iu)=(0.,0.)
      enddo
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine expmodel(omega, r, slo, ogreen, 
     &  hankel1, hankel2, uzerospecial, applywltaper)
c 
c expand the model from coefficients alpha using
c J_0 or H^1_0 or H^2_0 as representers
c or
c J_1 or H^1_1 or H^2_1 as representers, respectively
c
c This subroutine is used by almost all methods, including the
c Fourier-Bessel transformation
c
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
      include 'greda_pwo.inc'
c
      real omega, r(maxtr), slo(maxslo)
      complex ogreen(maxslo)
      logical hankel1, hankel2, uzerospecial, applywltaper
c 
      integer it, iu
      double precision tf_dj0, arg, tf_dy0
      double precision tf_dj1, tf_dy1
      real thisom, slowness
      double complex sum, add
c 
c evaluate expansion for every slowness
      do iu=1,nslo
        slowness=slo(iu)
        sum=(0.d0,0.d0)
        if (applywltaper) then
          call setwltaper(omega,slowness,r)
        endif
        if (radial) then
          do it=1,ntr
            arg=slowness*omega*r(it)
            add=(0.d0,0.d0)
            if ((uzerospecial).and.(iu.eq.1)) then
c use J_1
                arg=alpha(it)*tf_dj1(arg)*wltaperfac(it)
            elseif (hankel1) then
c use H^(1)_1
              arg=max(arg,1.d-100)
              add=alpha(it)*(tf_dj1(arg)+(0.d0,1.d0)*tf_dy1(arg))*
     &                      wltaperfac(it)*0.5
            elseif (hankel2) then
c use H^(2)_1
              arg=max(arg,1.d-100)
              add=alpha(it)*(tf_dj1(arg)-(0.d0,1.d0)*tf_dy1(arg))*
     &                      wltaperfac(it)*0.5
            else
c use J_1
              add=alpha(it)*tf_dj1(arg)*wltaperfac(it)
            endif
            sum=sum+add
          enddo
        else
          do it=1,ntr
            arg=slowness*omega*r(it)
            add=(0.d0,0.d0)
            if ((uzerospecial).and.(iu.eq.1)) then
c use J_0
                arg=alpha(it)*tf_dj0(arg)*wltaperfac(it)
            elseif (hankel1) then
c use H^(1)_0
              arg=max(arg,1.d-100)
              add=alpha(it)*(tf_dj0(arg)+(0.d0,1.d0)*tf_dy0(arg))*
     &                      wltaperfac(it)*0.5
            elseif (hankel2) then
c use H^(2)_0
              arg=max(arg,1.d-100)
              add=alpha(it)*(tf_dj0(arg)-(0.d0,1.d0)*tf_dy0(arg))*
     &                      wltaperfac(it)*0.5
            else
c use J_0
              add=alpha(it)*tf_dj0(arg)*wltaperfac(it)
            endif
            sum=sum+add
          enddo
        endif
        ogreen(iu)=sum
      enddo
c 
c calculate phasor walkout
      if (pwo_nlu.gt.0) then
        do iu=1,pwo_nlu
          slowness=pwo_p(pwo_lu(iu))
          thisom=pwo_om(pwo_lu(iu))
          sum=(0.d0,0.d0)
          if (applywltaper) then
            call setwltaper(thisom,slowness,r)
          endif
          do it=1,ntr
            arg=slowness*thisom*r(it)
            add=(0.d0,0.d0)
            if ((uzerospecial).and.(iu.eq.1)) then
c use J_0
                arg=alpha(it)*tf_dj0(arg)*wltaperfac(it)
            elseif (hankel1) then
c use H^(1)_0
              arg=max(arg,1.d-100)
              add=alpha(it)*(tf_dj0(arg)+(0.d0,1.d0)*tf_dy0(arg))*
     &                        wltaperfac(it)*0.5
            elseif (hankel2) then
c use H^(2)_0
              arg=max(arg,1.d-100)
              add=alpha(it)*(tf_dj0(arg)-(0.d0,1.d0)*tf_dy0(arg))*
     &                        wltaperfac(it)*0.5
            else
c use J_0
              add=alpha(it)*tf_dj0(arg)*wltaperfac(it)
            endif
            sum=sum+add
            pwo_phasor(iu, it)=sum
          enddo
        enddo
      endif
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
c calculate model expansion coefficients alpha from gram matrix
c 
c This subroutine solves the system of linear equations set up with the
c Gram matrix. All methods of linear inversion using the Gram matrix
c make use of this subroutine, except the HOP-method, which uses an
c analytical expression for the Gram matrix.
c
      subroutine modexp(iom, s)
c 
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      integer iom
      double complex s(maxtr)
c
      integer info, i, j
      double precision b(maxtr, 2)
      double precision gramwork(maxtr, maxtr)
c 
      do i=1,ntr
        b(i,1)=real(s(i))
        b(i,2)=imag(s(i))
        do j=1,ntr
          gramwork(i,j)=gram(i,j)
        enddo
      enddo
c      stop 'SORRY build first'
      call dposv('U', ntr, 2, gramwork, maxtr, b, maxtr, info)
c      call zgesv(ntr, 1, gram, maxtr, ipiv, alpha, maxtr, info)
c      print *,' entering dl2acg for freq ',iom
c      call dl2acg(ntr, mat, maxtr, s, 1, alpha, ws, ipiv, wk)
c      call dl2lcg(ntr, mat, maxtr, s, 1, alpha, ws, ipiv, wk)
c      call dl2ahf(ntr, mat, maxtr, s, 1, alpha, ws, ipiv, wk)
c      call dl2ncg(ntr, mat, maxtr, ws, 80, alpha, ws, ipiv, wk)
      if (info.lt.0) then
c        print *,'ERROR (zgesv): argument ',-info,' had illegal value'
        print *,'ERROR (dposv): argument ',-info,' had illegal value'
        stop
      endif
      if (info.gt.0) then
        print *,'ERROR (dposv): leading minor order ',info,
     &    ' is not positive definit at frequency ',iom
c        print *,'ERROR (zgesv): your matrix is singular at frequency ',iom
        print *,'ERROR: setting result to zero'
        do i=1,ntr
          alpha(i)=(0.d0, 0.d0)
        enddo
      endif
      do i=1,ntr
        alpha(i)=dcmplx(b(i,1), b(i,2))
      enddo
c 
      return
      end
c 
c======================================================================
c 
c here we go with the various strategies
c
c======================================================================
c 
c Method: straight foward Fourier-Bessel transformation
c -----------------------------------------------------
c
c This subsroutine prepares values to be used in subroutine expmodel to
c calculate Fourier-Bessel expansion coefficients by a straight forward
c Fourier-Bessel transformation.
c
c     
      subroutine backcoeff(verbose, r, omega, spect, chain, firstrec,
     &                     numbers)
c
c calculate coefficients for transform
c
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real r(maxtr), omega
      double complex spect(maxtr)
      integer chain(maxtr), firstrec
      logical verbose, numbers
c 
      integer i, j, k, l, tidx(maxtr)
      real dr, omegaq
      double precision as,aa,ps,pi,maxval
      parameter(pi=3.1415926535897931159)
      double complex cs,ca
c 
c it's a kind of
c discrete expansion coefficients using diagonal gram matrix
      omegaq=omega**2
      do i=1,ntr
        alpha(i)=r(i)*omegaq*spect(i)
      enddo
c 
c multiply with delta r to set up inverse transform
      if (verbose) then 
        print *,' '
        print *,'building offset steps'
      endif
      j=firstrec
      k=j
      do i=1,ntr
        tidx(i)=j
        l=k
        k=j
        j=chain(k)
        if (l.eq.k) then
          dr=(r(j)-r(k))
        elseif (j.lt.1) then
          dr=(r(k)-r(l))
        else
          dr=(r(j)-r(l))
        endif
        dr=dr*0.5
        alpha(k)=alpha(k)*dr
        if (verbose) print *,'  offset ',i,r(k),'m   trapezoid step ',dr,'m',
     &    '   index ',k
      enddo
c
c print numbers
      if (numbers) then
        maxval=0.
        do i=2,ntr-1
          cs=spect(tidx(i))
          aa=abs(cs)
          maxval=max(maxval,aa)
        enddo
        print *,' '
        print *,'Here are the numbers you requested:'
        print 50,'#','r','As','PHIs','Aalpha','AlpRed','AlpRed2'
        do i=2,ntr-1
          cs=spect(tidx(i))
          ca=alpha(tidx(i))
          as=abs(cs)
          aa=abs(ca)/omegaq/maxval
          as=as/maxval
          ps=datan2(imag(cs),real(cs))/pi
          print 51,i,r(tidx(i)),as,ps,aa,aa/sqrt(r(tidx(i))),aa/r(tidx(i))
        enddo
      endif
c 
      return
   50 format(1x,a3,1x,a5,1x,5(a10,1x))
   51 format(1x,i3,1x,f5.1,1x,2(f10.5,1x),3(f10.4,1x))
      end
c 
c======================================================================
c
c Method: Linear inversion using a Gram matrix formulation and 
c         exponential damping with exp(-rohq*uq) damping factor
c
c----------------------------------------------------------------------
c 
c calculate gram matrix for exponent damping factor
c
      subroutine gramex(rhoq, omq, r)
c 
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real rhoq, omq
      real r(maxtr)
c 
      integer i,j
      double precision tf_gsl_sf_bessel_I0
      double precision tf_gsl_sf_bessel_I1
      double precision mwr, wr2, er2, earg, ibarg
c
      er2=0.5d0/rhoq
      mwr=-0.25d0*omq/rhoq
      wr2=omq*er2
c
c calculate upper half and diagonal
      if (radial) then
        do i=1,ntr
          do j=i,ntr
            earg=mwr*(r(i)**2+r(j)**2)
            ibarg=wr2*r(i)*r(j)
            gram(i,j)=er2*exp(earg)*tf_gsl_sf_bessel_I1(ibarg)
          enddo
        enddo
      else
        do i=1,ntr
          do j=i,ntr
            earg=mwr*(r(i)**2+r(j)**2)
            ibarg=wr2*r(i)*r(j)
            gram(i,j)=er2*exp(earg)*tf_gsl_sf_bessel_I0(ibarg)
          enddo
        enddo
      endif
c 
c matrix is symmetric
      do j=1,ntr-1
        do i=j+1,ntr
          gram(j,i)=gram(i,j)
        enddo
      enddo
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine scalex(nslo, rhoq, ogreen, slo, ntr)
c 
c scale coefficient matrix correct for exponential damping
c 
      include 'greda_dim.inc'
      include 'greda_pwo.inc'
c
      integer nslo, ntr
      real rhoq
      complex ogreen(maxslo)
      real slo(maxslo)
c 
      integer iu,it
      double precision factor
c 
c scale to reflectivity inner product
      do iu=1,nslo
        ogreen(iu)=ogreen(iu)*exp(-rhoq*slo(iu)*slo(iu))
      enddo
c
c scale phasor walkout
      if (pwo_nlu.gt.0) then
        do iu=1,pwo_nlu
          factor=exp(-rhoq*pwo_p(pwo_lu(iu))*pwo_p(pwo_lu(iu)))
          do it=1,ntr
            pwo_phasor(iu,it)=pwo_phasor(iu,it)*factor
          enddo
        enddo
      endif
c 
      return
      end
c 
c======================================================================
c 
c Method: slant stack
c -------------------
c
c assume plane waves and do it by stacking seismograms
c
c This method cannot distinguish between vertical and radial component.
c
      subroutine planestack(omega, slo, r, green, iomega, applywltaper)
c 
      include 'greda_dim.inc'
      include 'greda.inc'
      include 'greda_pwo.inc'
c
      real omega, slo(maxslo), r(maxtr)
      complex green(maxslo, maxom)
      integer iomega
      logical applywltaper
c 
      double complex sum, imu
      parameter(imu=(0.d0,1.d0))
      integer i,j
c
      do i=1,nslo
c apply wavelength specific taper
        if (applywltaper) then
          call setwltaper(omega,slo(i),r)
        endif
        sum=(0.d0,0.d0)
        do j=1,ntr
          sum=sum+(spectra(j,iomega)*exp(imu*omega*slo(i)*r(j)))*
     &            wltaperfac(j)
        enddo
        green(i, iomega)=sum
      enddo
c 
c calculate phasor walkout
      if (pwo_nlu.gt.0) then
        do i=1,pwo_nlu
c apply wavelength specific taper
          if (applywltaper) then
            call setwltaper(omega,pwo_p(pwo_lu(i)),r)
          endif
          sum=(0.d0,0.d0)
          do j=1,ntr
            sum=sum+(spectra(j,iomega)*
     &              exp(imu*pwo_om(pwo_lu(i))*pwo_p(pwo_lu(i))*r(j)))*
     &              wltaperfac(j)
            pwo_phasor(i, j)=sum
          enddo
        enddo
      endif
c 
      return
      end
c
c======================================================================
c 
c Method: direct discrete matrix inversion
c ----------------------------------------
c
c This is a DIRTY APPROACH and will most probably fail
c
c calculate forward matrix
c
      subroutine forwardmat(omega, slo, r, hankel1, hankel2, uzerospecial)
c 
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real omega
      real slo(maxslo)
      real r(maxtr)
      logical hankel1, hankel2, uzerospecial
c 
      integer iu, it
      double precision arg, parg, du, pdu
      double precision tf_dj0, tf_dy0
c
      if (nslo.ne.ntr) stop 'ERROR (forwardmat): need an NxN system'
c 
      pdu=slo(2)-slo(1)
      do iu=1,nslo
        du=pdu
        parg=omega*slo(iu)
        if ((iu.eq.1).or.(iu.eq.nslo)) du=pdu*0.5d0
        if ((uzerospecial).and.(iu.eq.1)) then
          do it=1,ntr
            arg=parg*r(it)
            gram(it,iu)=tf_dj0(arg)*slo(iu)*du
          enddo
        elseif (hankel1) then
          do it=1,ntr
            arg=max(parg*r(it),1.d-100)
            gram(it,iu)=(tf_dj0(arg)+(0.d0,1.d0)*tf_dy0(arg))*slo(iu)*du
          enddo
        elseif (hankel2) then
          do it=1,ntr
            arg=max(parg*r(it),1.d-100)
            gram(it,iu)=(tf_dj0(arg)-(0.d0,1.d0)*tf_dy0(arg))*slo(iu)*du
          enddo
        else
          do it=1,ntr
            arg=parg*r(it)
            gram(it,iu)=tf_dj0(arg)*slo(iu)*du
          enddo
        endif
      enddo
c
      return
      end
c
c======================================================================
c
c Method: Linear inversion using a Gram matrix formulation and 
c         damping with K_0 damping factor
c
c----------------------------------------------------------------------
c 
c calculate gram matrix for K_0 damping factor
c
      subroutine gramkmet(rhoq, omq, r)
c 
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real rhoq, omq
      real r(maxtr)
c 
      integer i,j
      double precision riq, rjq, riqq, rjqq, omqq, rhoqq
      double precision arg
c 
c
      omqq=omq*omq
      rhoqq=rhoq*rhoq
c calculate upper half and diagonal
      do i=1,ntr
        riq=r(i)*r(i)
        riqq=riq*riq
        do j=i,ntr
          rjq=r(j)*r(j)
          rjqq=rjq*rjq
          arg=(omq*omq*(riqq+rjqq)+rhoqq+2.d0*omq*
     &        (rhoq*(riq+rjq)-omq*riq*rjq)) 
          arg=max(arg,1.d-100)
          gram(i,j)=1.d0/sqrt(arg)
        enddo
      enddo
c 
c matrix is symmetric
      do j=1,ntr-1
        do i=j+1,ntr
          gram(j,i)=gram(i,j)
        enddo
      enddo
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine scalkmet(nslowness, rho, ogreen, slo, ntraces)
c 
c scale coefficient matrix correct for K_0 damping
c 
      include 'greda_dim.inc'
      include 'greda.inc'
      include 'greda_pwo.inc'
c
      integer nslowness, ntraces
      real rho
      complex ogreen(maxslo)
      real slo(maxslo)
c 
      integer iu, it
      double precision tf_gsl_sf_bessel_k1
      double precision tf_gsl_sf_bessel_k0, factor
c 
c scale to reflectivity inner product
      if (radial) then
        do iu=1,nslowness
          ogreen(iu)=ogreen(iu)*tf_gsl_sf_bessel_k1(dble(slo(iu)*rho))
        enddo
      else
        do iu=1,nslowness
          ogreen(iu)=ogreen(iu)*tf_gsl_sf_bessel_k0(dble(slo(iu)*rho))
        enddo
      endif
c
c scale phasor walkout
      if (radial) then
        if (pwo_nlu.gt.0) then
          do iu=1,pwo_nlu
            factor=tf_gsl_sf_bessel_k1(dble(pwo_p(pwo_lu(iu))*rho))
            do it=1,ntraces
              pwo_phasor(iu,it)=pwo_phasor(iu,it)*factor
            enddo
          enddo
        endif
      else
        if (pwo_nlu.gt.0) then
          do iu=1,pwo_nlu
            factor=tf_gsl_sf_bessel_k0(dble(pwo_p(pwo_lu(iu))*rho))
            do it=1,ntraces
              pwo_phasor(iu,it)=pwo_phasor(iu,it)*factor
            enddo
          enddo
        endif
      endif
c 
      return
      end
c
c======================================================================
c 
c method independent subroutines to handle wavelength specific tapers
c
c----------------------------------------------------------------------
c
      subroutine initwltaper(length,fraction)
c
c initialize wavelength taper factors to unity
c and set taper parameters
c
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real length, fraction
c
      integer i
c
      do i=1,maxtr
        wltaperfac(i)=1.
      enddo
      wltaperlen=length
      wltaperfrac=1.-fraction/100.
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine setwltaper(omega,slo,r)
c
c set wavelength taper factors for given wavelength
c
      include 'greda_dim.inc'
      include 'greda.inc'
c 
      real omega,slo,r(maxtr)
c
      integer i
      real pi2, wlength, tapr1, tapr2, argfac
      parameter(pi2=2.*3.141592653589793)
c 
      wlength=pi2/(omega*slo)
      tapr2=wltaperlen*wlength
      tapr1=tapr2*wltaperfrac
      argfac=0.25*pi2/(tapr2-tapr1)
c
      do i=1,ntr
        if (r(i).lt.tapr1) then
          wltaperfac(i)=1.
        elseif (r(i).gt.tapr2) then
          wltaperfac(i)=0.
        else
          wltaperfac(i)=cos(argfac*(r(i)-tapr1))
        endif
      enddo
c      print *,(wltaperfac(i),i=1,ntr)
c 
      return
      end
c 
c======================================================================
c 
c
c Method: Linear inversion using a Gram matrix formulation and 
c         boxcar damping
c
c This requires a discretized integration to calculate the Gram matrix
c
c----------------------------------------------------------------------
c 
c calculate gram matrix with discrete integration
c
      subroutine gramdis(slo, om, r)
c 
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real om
      real r(maxtr), slo(maxslo)
c 
      integer i,j
c 
      double precision gramdisint
c
c calculate upper half and diagonal
      do i=1,ntr
        do j=i,ntr
          gram(i,j)=gramdisint(maxslo, nslo, slo, r(i), r(j), om, radial)
        enddo
      enddo
c 
c matrix is symmetric
      do j=1,ntr-1
        do i=j+1,ntr
          gram(j,i)=gram(i,j)
        enddo
      enddo
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
c calculate discrete gram integral
c
      double precision function gramdisint(maxslo, nslo,
     &  slo, rj, rk, om, radialcomp)
c
      logical radialcomp
c 
      integer maxslo, nslo
      real slo(maxslo), om, rj, rk
c 
      integer iu
      double precision du, result, pdu, argj, argk, pargj, pargk
      double precision tf_dj0
      double precision tf_dj1
c
      pdu=slo(2)-slo(1)
      pargj=om*rj
      pargk=om*rk
      result=0.d0
      if (radialcomp) then
        do iu=1,nslo
          du=pdu
          if ((iu.eq.1).or.(iu.eq.nslo)) du=pdu*0.5d0
          argj=pargj*slo(iu)
          argk=pargk*slo(iu)
          result=result+tf_dj1(argj)*tf_dj1(argk)*slo(iu)*du
        enddo
      else
        do iu=1,nslo
          du=pdu
          if ((iu.eq.1).or.(iu.eq.nslo)) du=pdu*0.5d0
          argj=pargj*slo(iu)
          argk=pargk*slo(iu)
          result=result+tf_dj0(argj)*tf_dj0(argk)*slo(iu)*du
        enddo
      endif
      gramdisint=result
c 
      return
      end
c 
c======================================================================
c 
c Method: Linear inversion using a Gram matrix formulation and 
c         Lorentz damping
c
c This is the HOP-method published by Henry, Orcutt and Parker.
c
c----------------------------------------------------------------------
c 
c calculate the model expansion coefficients from a direct matrix
c inversion
c
      subroutine parker(rho, r, s, chain, firstrec, numbers)
c 
c get common block
      include 'greda_dim.inc'
      include 'greda.inc'
c
      real rho
      real r(maxtr)
      double complex s(maxtr)
      integer firstrec, chain(maxtr)
      logical numbers
c 
      double precision p(maxtr), q(maxtr)
      integer tidx(maxtr)
      integer i,j
      double precision tf_gsl_sf_bessel_k0
      double precision tf_gsl_sf_bessel_i0
      double precision tf_gsl_sf_bessel_k1
      double precision tf_gsl_sf_bessel_i1
      double precision arg
c
      double precision q0,q1,pi,maxval,alpa,alpaphi
      double precision a,am1,ap1,phim1,phip1,phi
      double complex c,cm1,cp1,alp
      parameter(pi=3.1415926535897931159)
c     
      i=firstrec
      j=1
      if (radial) then
        do while (i.gt.0) 
          arg=r(i)*rho
          p(j)=tf_gsl_sf_bessel_i1(arg)
          q(j)=tf_gsl_sf_bessel_k1(arg)
          tidx(j)=i
          i=chain(i)
          j=j+1
        enddo
      else
        do while (i.gt.0) 
          arg=r(i)*rho
          p(j)=tf_gsl_sf_bessel_i0(arg)
          q(j)=tf_gsl_sf_bessel_k0(arg)
          tidx(j)=i
          i=chain(i)
          j=j+1
        enddo
      endif
c 
c the array gram is used here to hold the inverse of the gram matrix!
      if (ntr.lt.3) stop 'ERROR less than 3 traces is senseless'
      do i=1,ntr-1
        gram(i,i+1)=1.d0/(p(i)*q(i+1)-q(i)*p(i+1))
      enddo
      gram(1,1)=-gram(1,2)*p(2)/p(1)
      gram(ntr,ntr)=-gram(ntr-1,ntr)*q(ntr-1)/q(ntr)
      do i=2,ntr-1
        gram(i,i)=gram(i-1,i)*gram(i,i+1)*(p(i+1)*q(i-1)-q(i+1)*p(i-1))
      enddo
c 
c model expansion coefficients
      alpha(tidx(1))=gram(1,1)*s(tidx(1))+s(tidx(2))*gram(1,2)
      alpha(tidx(ntr))=gram(ntr,ntr)*s(tidx(ntr))+s(tidx(ntr-1))*gram(ntr-1,ntr)
      do i=2,ntr-1
        alpha(tidx(i))=gram(i,i)*s(tidx(i))+
     &    s(tidx(i-1))*gram(i-1,i)+s(tidx(i+1))*gram(i,i+1)
      enddo
c 
c HOP numbers
      if (numbers) then
        maxval=0.
        do i=2,ntr-1
          c=s(tidx(i))
          a=abs(c)
          maxval=max(maxval,a)
        enddo
        print *,' '
        print *,'Here are the numbers you requested:'
        print 50,'#','r','Ared','phi','aa','aap',
     &           'q0','q1','qr','A-1','phi-1','A+1','phi+1'
        do i=2,ntr-1
          q0=gram(i,i)
          q1=gram(i,i+1)
          c=s(tidx(i))
          cm1=s(tidx(i-1))/c
          cp1=s(tidx(i+1))/c
          alp=alpha(tidx(i))
          alpa=abs(alp)
          alpaphi=datan2(imag(alp),real(alp))
          a=abs(c)
          phi=datan2(imag(c),real(c))
          am1=abs(cm1)
          phim1=datan2(imag(cm1),real(cm1))
          ap1=abs(cp1)
          phip1=datan2(imag(cp1),real(cp1))
          phi=phi/pi
          phim1=phim1/pi
          phip1=phip1/pi
          alpa=alpa/(q0*a)
          a=a*sqrt(r(tidx(i)))/maxval
c          alpa=alpa/maxval/r(tidx(i))/a
          alpaphi=alpaphi/pi
          print 51,i,r(tidx(i)),a,phi,alpa,alpaphi,
     &             q0,q1,-q1/q0,am1,phim1,ap1,phip1
        enddo
      endif
c 
      return
   50 format(a3,1x,a5,1x,4(a5,1x),2(a6,1x),a4,1x,4(a6,1x))
   51 format(i3,1x,f5.1,1x,4(f5.2,1x),2(f6.1,1x),f4.2,1x,4(f6.3,1x))
      end
c 
c----------------------------------------------------------------------
c 
      subroutine scalpark(nslo, rhoq, omq, ogreen, slo, 
     &  numbers, ntr)
c 
c scale coefficient matrix for parkers method
c
      include 'greda_dim.inc'
      include 'greda_pwo.inc'
c 
      integer nslo, ntr
      real rhoq, omq
      complex ogreen(maxslo)
      real slo(maxslo)
      logical numbers
c 
      integer iu,it
      double precision factor
c 
c scale to reflectivity inner product
      do iu=1,nslo
        ogreen(iu)=ogreen(iu)/(slo(iu)*slo(iu)+rhoq/omq)
      enddo
c
c scale phasor walkout
      if (pwo_nlu.gt.0) then
        do iu=1,pwo_nlu
          factor=1.d0/(pwo_p(pwo_lu(iu))*pwo_p(pwo_lu(iu))+rhoq/omq)
          do it=1,ntr
            pwo_phasor(iu,it)=pwo_phasor(iu,it)*factor
          enddo
        enddo
      endif
c 
c print numbers
      if (numbers) then
        print *,' '
        print *,'HOP scaling factors'
        print 50,'#','slo','fac'
        do iu=1,nslo
          print 51,iu,slo(iu),1./(slo(iu)*slo(iu)+rhoq/omq)
        enddo
      endif
cc 
      return
   50 format(1x,a3,1x,a10,1x,a10)
   51 format(1x,i3,1x,f10.7,1x,g10.2)
      end
c 
c ----- END OF greda.f -----
