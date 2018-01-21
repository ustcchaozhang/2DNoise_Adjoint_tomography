c this is <gresy.f>
c------------------------------------------------------------------------------
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c calculate seismograms from a greens function matrix file
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
c    26/06/97   V1.0   Thomas Forbriger
c    25/07/97   V1.1   now calculating the fourier bessel transform
c    06/04/98   V1.2   applied response function
c    18/04/00   V1.3   set a definite date (year zero is 
c                      interpreted as relative date)
c    20/04/00   V1.4   suppress zero frequency and zero slowness option
c                      introduced Hankel mode
c    21/04/00   V1.5   grand maleur: did never select hankel2
c                      set factor 0.5 for H1 and H2
c    27/08/01   V1.6   allow full length filename parameters
c    02/11/01   V1.7   allow too many response coefficients
c    03/04/02   V1.8   support radial component
c    09/01/03   V1.9   be not too strict when checking frequencies fread
c                      from response file. Check against frequency and not
c                      against frequency interval.
c    05/07/07   V1.10  check slowness sampling interval
c    18/01/10   V1.11  there is a problem: 
c                      gresy receives coordinates in km where they should be
c                      in m (rcv-file specifies 0.003, gresy receives 30km)
c                      I introduced verbose output and checked the program
c                      the problem disappeared for unkown reason...
c    26/02/10   V1.12  introduced line-source mode
c    01/03/10   V1.13  correction (linesource summation was simply wrong)
c    02/03/10   V1.14  corrected (hopefully) line source integration
c    12/01/2011 V1.14b - added definition of Fourier transformation 
c                        online help text
c    18/01/2011 V1.15  implement libfapidxx interface
c    10/04/2011 V1.16  program supports selection of sampling parameters
c    26/04/2011        master file mode is tested 
c    29/11/2011 V1.17  provide complex exponential function for line
c                      source (travelling wave expansion)
c    29/11/2011 V1.18  implemented radial component line source
c                      both new cases successfully tested against refmet
c                      notice: refmet is not yet tested against a
c                      reference
c
c==============================================================================
c
      program gresy
      character*79 version
      parameter(version='GRESY   V1.18   GREens function SYnthetics')
c dimensions
      integer maxtr, maxsamp, maxom, maxu
      parameter(maxu=10000, maxtr=maxu, maxom=4100, maxsamp=maxom*2)
      integer ntr, nsamp, nom, nu, nsampwrite
c greens function
      character*78 greenname
      complex green(maxom, maxu)
      real om(maxom)
      real slo(maxu)
c receiver specifications
      double precision rcvvred, rcvtli, rcvtre 
      character*80 rcvtext
      double precision phi(maxtr)
c seismogram parameters
      character*80 seisname,rcvname
      double precision r(maxtr), rmax
      complex*16 sdata(maxsamp)
      real fdata(maxsamp)
      integer idata(maxsamp)
      equivalence(fdata, idata)
c functions
      real sffu_offset, sffu_tfirst
c free block
      integer maxfree, nfree
      parameter(maxfree=5)
      character*80 free(maxfree)
c SFF parameters
      real sffversion, tanf
      character timestamp*13, code*10
c srce line
      character srctype*40, date*20, time*20, scs
      real sc1, sc2, sc3
c info line
      character cs
      real c1, c2, c3
      integer nstack
c seismogram trace to sff file
      character*132 wid2line
      logical last
      real dt
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=15)
      character*3 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c options
      logical debug, optlambda, optnew, optresponse, hankel1, hankel2
      logical suppress, radial, verbose, linesource, usemaster
      real lambdalim, tapfrac
      character*80 respfile, fileformat
c taper
      real tf_costap
      integer ltap, rtap
c response file
      real rom, rre, rim
      complex response(maxom)
      integer nresp
      real omega(maxom)
      real domega
c any
      integer mlu,lu, i, ierr, io, iu
      double precision pi2, arg, du, dom, scal, pi
      parameter(lu=20,mlu=21,pi=3.141592653589793115997)
      parameter(pi2=2.d0*pi)
      double complex ime
      parameter(ime=(0.,1.))
c functions
      double precision tf_dj0, tf_dy0
      double precision tf_dj1, tf_dy1
c here are the keys to our commandline options
      data optid/'-D','-l','-o','-t','-r','-S','-1','-2','-R','-v',
     &           '-L','-ty','-dt', '-N', '-m'/
      data opthasarg/.FALSE.,.TRUE.,.FALSE.,2*.TRUE.,6*.FALSE.,
     &               3*.true.,.false./
      data optarg/'-','1.','-','10.','junk',6*'-','sff',
     &            '1.e-18','1000000','-'/
c----------------------------------------------------------------------
c 
c read commandline
c 
      print *,version
      print *,'Usage: gresy greenfile seisfile rcvfile|masterfile'
      print *,'             [-m] [-l lambda] [-o]'
      print *,'             [-t frac] [-r respfile] [-S] [-1] [-2] [-R]'
      print *,'             [-v] [-d] [-L] [-ty f] [-dt dt] [-N n]'
      print *,'or     gresy -help'
      print *,'or     gresy -xhelp'
      if (iargc().lt.1) stop 'ERROR: no arguments'
      call getarg(1, greenname)
      if (greenname(1:6).eq.'-xhelp') then
        call sff_help_details
        stop
      else if (greenname(1:5).eq.'-help') then
        print *,' '
        print *,'Calculates synthetic seismograms from greens function.' 
        print *,' '
        print *,'greenfile    file containing Fourier-Bessel expansion'
        print *,'             coefficients of an impulse response'
        print *,'             (in a format produced by ''syg'' or'
        print *,'             ''greda'')'
        print *,'seisfile     output file to contain seismograms'
        print *,' '
        print *,'rcvfile      receiver definition (refmet format)'
        print *,'masterfile   waveform data file; the traces in the output file'
        print *,'             ''seisfile'' to be created will have the same'
        print *,'             spatial and temporal coordinates as the'
        print *,'             ''masterfile'' '
        print *,' '
        print *,'rcvfile and masterfile are alternatives to each other'
        print *,'see option -m and comment below'
        print *,' '
        print *,'-v           produce verbose output'
        print *,'-D           produce debug output'
        print *,'-m           the third parameter is understood as the name'
        print *,'             of a ''masterfile'' which must be a valid'
        print *,'             time series data file.'
        print *,'-ty f        select seismogram file format f (see below)'
        print *,'-dt dt       desired sampling interval for output'
        print *,'-N n         desired number of samples for output'
        print *,'-l lambda    limits the used slowness range by a minimum'
        print *,'             wavelength in meters'
        print *,'             (default: ',optarg(2)(1:4),')'
        print *,'-o           overwrite output'
        print *,'-t frac      tapering fraction for slowness domain taper'
        print *,'             given in percent of full slowness range'
        print *,'             (default: ',optarg(4)(1:4),')'
        print *,'-r respfile  file to contain system response in a format'
        print *,'             produced by ''gremlin'' '
        print *,'-S           suppress zero frequency and zero slowness'
        print *,'-1           use Hankel 1 instead of Bessel'
        print *,'             exp(i*omega*p*r) instead for cos in case'
        print *,'             of a line source'
        print *,'             this is a travelling wave expansion'
        print *,'             (see remark on Fourier transformation below)'
        print *,'-2           use Hankel 2 instead of Bessel'
        print *,'             exp(-i*omega*p*r) instead for cos in case'
        print *,'             of a line source'
        print *,'             this is a travelling wave expansion'
        print *,'             (see remark on Fourier transformation below)'
        print *,'-L           simulate seismograms from line source (2D)'
        print *,'-R           calculate radial component'
        print *,' '
        print *,'Definition of the Fourier transformation:'
        print *,'The Fourier transformation used in this program and in'
        print *,'related programs (like gremlin, syg, and greda) is'
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
        print *,'in gresy.'
        print *,' '
        print *,'Input file units:'
        print *,' '
        print *,'  Slowness unit: s/m'
        print *,'  Frequency unit: angular frequency 1/s'
        print *,'  amplitude unit: m**3/s if spectrum represents displacement'
        print *,'                  waveform in m'
        print *,' '
        print *,'Output trace coordinates and time series sampling can'
        print *,'be specified alternatively by'
        print *,'a) rcvfile and options -N and -dt'
        print *,'or'
        print *,'b) masterfile'
        print *,'Option -m selection alternative b), such that command  '
        print *,'line options -N and -dt become ineffective. In any case'
        print *,'the sampling of respfile must be appropriate to match'
        print *,'the desired properties of output sampling and of the'
        print *,'input greenfile.'
        print *,' '
        print *,'array dimensions:'
        print *,'   maximum number of slowness values: ',maxu
        print *,'  maximum number of frequency values: ',maxf
        print *,'            maximum number of traces: ',maxtr
        print *,' '
        call refmet_rcvinf
        print *,' '
        call sff_help_formats
        stop
      endif
      if (iargc().lt.3) stop 'ERROR: need more arguments'
c 
      call getarg(2, seisname)
      call getarg(3, rcvname)
c
      call tf_cmdline(4, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      debug=optset(1)
      optlambda=optset(2)
      if (optlambda) then
        read(optarg(2), *, err=99) lambdalim
      endif
      optnew=optset(3)
      read(optarg(4), *, err=99)tapfrac
      tapfrac=tapfrac/100.
      optresponse=optset(5)
      respfile=optarg(5)
      suppress=optset(6)
      hankel1=optset(7)
      hankel2=optset(8)
      radial=optset(9)
      verbose=optset(10)
      linesource=optset(11)
      fileformat=optarg(12)
      read(optarg(13), *, err=99) dt
      if (dt.lt.1.e-20) stop 'ERROR: sampling interval too small'
      read(optarg(14), *, err=99) nsampwrite
      usemaster=optset(15)

c 
      call sff_select_output_format(fileformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting output file format'
c----------------------------------------------------------------------
      if (radial) print *,'calculate radial component'
c 
c read configuration and greens function
c 

      if (.not.usemaster) then
c 
c read coordinates from receiver file if no master file is used
c -------------------------------------------------------------
        call refmet_rrcv(rcvname, rcvtext,
     &    rcvvred, rcvtli, rcvtre, ntr, maxtr, r, phi, 0.d0,
     &    0, 1, debug)
c
        print *,'receiver configuration was taken from:'
        print *,' ',rcvtext
        if (abs(rcvvred).gt.0.)
     &    print *,'WARNING: traveltime reduction will be ignored'
        if (abs(rcvtli).gt.0.)
     &    print *,'WARNING: left time shift will be ignored'
        if (abs(rcvtre).gt.0.)
     &    print *,'WARNING: right time shift will be ignored'
        if (verbose) then
          print *,'calculate seismograms for ',ntr,' receivers at'
          do i=1,ntr
            print *,r(i),' km   ',phi(i),' deg'
          enddo
        endif
      else
c open master file if selected
c ----------------------------
        print *,'take spatial and temporal sampling parameters from'
        print *,rcvname
        call sff_select_input_format(fileformat, ierr)
        if (ierr.ne.0) stop 'ERROR: selecting input file format'
        call sff_ROpenS(mlu, rcvname, sffversion, timestamp,
     &                  code, srctype, scs, sc1, sc2, sc3,
     &                  date, time, ierr)
        if (ierr.ne.0) stop 'ERROR: opening master file'
        if (index(code,'S').eq.0)
     &    stop 'ERROR: master file does not contain source parameters'
      endif
c 
      call greenread(greenname, debug,
     &     maxu, maxom, slo, om,
     &     nu, nom, green)
c 
      print *,'some information on green file read:'
      print *,' ',nu,' slownesses from ',slo(1),'s/m to ',slo(nu),'s/m'
      print *,' ',nom,' frequencies from ',om(1)/pi2,
     &  'Hz to ',om(nom)/pi2,'Hz'
      print *,' green frequency interval: ',(om(2)-om(1))/pi2,' Hz'
c 
      if (suppress) then
        print *,'suppress zero frequency and zero slowness coefficients'
        do io=1,maxom
          green(io,1)=(0.,0.)
        enddo
        do iu=1,maxu
          green(1,iu)=(0.,0.)
        enddo
      endif
c 
c----------------------------------------------------------------------
c
c read response file if desired
c
      if (optresponse) then
        do io=1,maxom
          response(io)=(0.,0.)
        enddo
c 
        print *,'read response from file ',
     &    respfile(1:index(respfile, ' '))
        open(lu, file=respfile, err=97)
        read(lu, 50, err=96, end=95)
        nresp=0
    2   read(lu, *, err=96, end=3) rom,rre,rim
        nresp=nresp+1
        if (nresp.gt.maxom) stop 'ERROR: too many samples'
        omega(nresp)=rom
        response(nresp)=cmplx(rre,rim)
        goto 2
    3   continue
        close(lu, err=94)
        print *,' response: file read and closed'
c 
        if (nresp.eq.0) stop 'ERROR: no coefficient read'
        print *,' response: read ',nresp,' coefficients'
        print *,' response: minimum angular frequency: ',omega(1),' 1/s'
        print *,' response: maximum angular frequency: ',
     &    omega(nresp),' 1/s'
        domega=(omega(nresp)-omega(1))/float(nresp-1)
        if ((omega(1)/domega).gt.1.e-10) 
     &    stop 'ERROR: first angular frequency schould be zero'
        print *,' response: angular frequency stepsize: ',domega, ' 1/s'
c 
        if (nresp.gt.nom) then
          nresp=nom
          print *,'NOTICE: using only ',nresp,' coefficients'
        endif
c 
        print *,' response: maximum frequency: ',omega(nresp)/pi2,' Hz'
        print *,' response: frequency interval: ',domega/(2.*pi),' Hz'
c 
        do io=1,nresp
          if (abs((omega(io)-om(io))/om(io)).gt.1.e-4) then
            print *,'ifre=',io,' om green=',
     &              om(io),' om resp=',omega(io)
         stop 'ERROR: inconsitency between greens function and response'
          endif
        enddo
      else
        do io=1,maxom
          response(io)=(1.,0.)
        enddo
      endif
c 
c----------------------------------------------------------------------
c 
c open seismogram file
c
      free(1)=version
      free(2)='green file '//greenname(1:index(greenname, ' '))
      if (usemaster) then
        free(3)='spatial and temporal sampling are taken from '
     &    //rcvname(1:index(rcvname, ' '))
        nfree=3
      else
        free(3)='receiver configuration from '
     &    //rcvname(1:index(rcvname, ' '))
        free(4)=rcvtext
        nfree=4
        srctype='implicit in green file'
        scs='C'
        sc1=0.
        sc2=0.
        sc3=0.
        date='990418'
        time='000000.000'
      endif
      if (optnew) call sff_New(lu, seisname, ierr)
      if (ierr.ne.0) stop 'ERROR: deleting seismogram file'
      call sff_WOpenFS(lu, seisname, free, nfree, srctype, 
     &                 scs, sc1 ,sc2 ,sc3,
     &                 date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening seismogram file'
c----------------------------------------------------------------------
c 
c preparatory calculations
c ------------------------
c 
c scale down epicentral distances to meters if offsets are specified
c by receiver file
      if (.not.usemaster) then
        do i=1,ntr
          r(i)=r(i)*1000.
        enddo
        if (debug) then
          print *,'DEBUG: receiver offsets after scaling to meters:'
          print *,(r(i),i=1,ntr)
        endif
      endif

c check consistency of sampling of Fourier-Bessel coefficients
c ------------------------------------------------------------
c check stepwidth
      dom=om(2)-om(1)
      do io=2,nom
        if (abs(om(io)-om(io-1)-dom).gt.(0.01*dom)) 
     &    print *,'WARNING: dom varies more than 1%'
      enddo
c 
      du=slo(2)-slo(1)
      do iu=2,nu
        if (abs(slo(iu)-slo(iu-1)-du).gt.(0.01*du)) 
     &    print *,'WARNING: du varies more than 1%'
      enddo
c
      if (om(1).gt.(0.01*dom))
     &  stop 'ERROR: program requires the first frequency to be 0.Hz!'

c the following two checks can be done here in advance if the spatial
c sampling is defined by a reciever configuration file
c it will be done on per-trace basis later if the sampling is defined by
c a master file
c
c check trapezoid rule stepsize
      if (.not.(usemaster)) then
        rmax=r(1)
        do i=1,ntr
          rmax=max(rmax,r(i))
          call checkslownesssampling(sngl(r(i)),sngl(du),om(nom))
        enddo

c obtain number of samples and sampling interval
        call sampling(dom, nom, dt, maxsamp, nsampwrite, nsamp, 
     &    verbose, debug)
c 
        print *,'traces will have ',nsampwrite,' samples at ',dt,
     &    's sampling interval'
      endif
c 
c report mode of calculatiomode of calculation
      if (linesource) then
        if (hankel1) then
          print *,'I will use the exp(i*omega*p*r) expansion (line-source, 2D)'
        elseif (hankel2) then
          print *,'I will use the exp(-i*omega*p*r) expansion (line-source, 2D)'
        else
          print *,'I will use the cosine expansion (line-source, 2D)'
        endif
      elseif (hankel1) then
        print *,'I will use the first Hankel function'
      elseif (hankel2) then
        print *,'I will use the second Hankel function'
      else
        print *,'I will use the Bessel function'
      endif
c----------------------------------------------------------------------
c big loop over traces
c --------------------
      last=.false.
      i=0
      do while (.not.last)
        i=i+1

c obtain sampling parameters for this trace from master file
        if (usemaster) then
          nsampwrite=maxsamp
          call sff_RTraceI(mlu, tanf, dt,
     &              wid2line, nsampwrite, fdata,
     &              idata, code, last,
     &              cs, c1, c2, c3, nstack, ierr)
          if (index(code,'I').eq.0)
     &    stop 'ERROR: master file does not contain receiver parameters'
          r(i)=sffu_offset(cs,c1,c2,c3,scs,sc1,sc2,sc3)
          tanf=sffu_tfirst(wid2line, time, date)
          call checkslownesssampling(sngl(r(i)),sngl(du),om(nom))

c obtain number of samples and sampling interval
          call sampling(dom, nom, dt, maxsamp, nsampwrite, nsamp, 
     &      verbose, debug)
c 
          print *,'trace will have ',nsampwrite,' samples at ',dt,
     &      's sampling interval'
        else
          if (i.eq.ntr) last=.true.
          tanf=0.
        endif
        print *,'working on trace ',i,' at ',r(i),'m'
c calculate bessel integral
        do io=1,nom
c set slowness taper
c          if (debug) print *,'enter frequency loop'
          if (optlambda) then
            if (om(io).lt.dom) then
              rtap=nu
              ltap=nu
            else
              rtap=min(int((pi2/om(io)/lambdalim/du+1.)*(1.+tapfrac*0.5)),nu)
              ltap=min(int((pi2/om(io)/lambdalim/du+1.)*(1.-tapfrac*0.5)),nu)
            endif
            ltap=min(ltap,int(rtap*(1.-tapfrac)))
          else
            rtap=nu
            ltap=int(rtap*(1.-tapfrac))
          endif
          if ((verbose).and.(i.eq.1)) then
            print *,'u-taper for ',om(io)/pi2,'Hz: ',ltap,'-',rtap,
     &        '   ',slo(ltap),'s/m-',slo(rtap),'s/m'
          endif
          sdata(io)=(0.d0,0.d0)
c          if (debug) print *,'enter slowness loop'
c----------------------------------------------------------------------
c distinguish different options for expansion kernels
          if (linesource) then
            if (hankel1) then
              do iu=1,rtap
                arg=slo(iu)*om(io)*r(i)
                if (radial) then
                  sdata(io)=sdata(io)+green(io,iu)*
     &              0.5*ime*sign(1.d0,r(i))*exp(ime*arg)*
     &              du*tf_costap(iu,0,0,ltap,rtap)
                else
                  sdata(io)=sdata(io)+green(io,iu)*
     &              0.5*exp(ime*arg)*
     &              du*tf_costap(iu,0,0,ltap,rtap)
                endif
              enddo
            elseif (hankel2) then
              do iu=1,rtap
                arg=slo(iu)*om(io)*r(i)
                if (radial) then
                  sdata(io)=sdata(io)+green(io,iu)*
     &              0.5*ime*sign(1.d0,r(i))*exp(-ime*arg)*
     &              du*tf_costap(iu,0,0,ltap,rtap)
                else
                  sdata(io)=sdata(io)+green(io,iu)*
     &              0.5*exp(-ime*arg)*
     &              du*tf_costap(iu,0,0,ltap,rtap)
                endif
              enddo
            else
              do iu=1,rtap
                arg=slo(iu)*om(io)*r(i)
                if (radial) then
                  sdata(io)=sdata(io)+green(io,iu)*
     &              sign(1.d0,r(i))*sin(arg)*
     &              du*tf_costap(iu,0,0,ltap,rtap)
                else
                  sdata(io)=sdata(io)+green(io,iu)*
     &              cos(arg)*
     &              du*tf_costap(iu,0,0,ltap,rtap)
                endif
              enddo
            endif
            sdata(io)=2.d0*sdata(io)/max(1.d-50,om(io))
          elseif (hankel1) then
            do iu=1,rtap
              arg=max(slo(iu)*om(io)*r(i),1.d-100)
              if (radial) then
                sdata(io)=sdata(io)+green(io,iu)*
     &            (tf_dj1(arg)+(0.d0,1.d0)*tf_dy1(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*slo(iu)
              else
                sdata(io)=sdata(io)+green(io,iu)*
     &            (tf_dj0(arg)+(0.d0,1.d0)*tf_dy0(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*slo(iu)
              endif
            enddo
            sdata(io)=0.5d0*sdata(io)
          elseif (hankel2) then
            do iu=1,rtap
              arg=max(slo(iu)*om(io)*r(i),1.d-100)
              if (radial) then
                sdata(io)=sdata(io)+green(io,iu)*
     &            (tf_dj1(arg)-(0.d0,1.d0)*tf_dy1(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*slo(iu)
              else
                sdata(io)=sdata(io)+green(io,iu)*
     &            (tf_dj0(arg)-(0.d0,1.d0)*tf_dy0(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*slo(iu)
              endif
            enddo
            sdata(io)=0.5d0*sdata(io)
          else
            do iu=1,rtap
              arg=slo(iu)*om(io)*r(i)
              if (radial) then
                sdata(io)=sdata(io)+green(io,iu)*
     &            tf_dj1(arg)*
     &            du*tf_costap(iu,0,0,ltap,rtap)*slo(iu)
              else
                sdata(io)=sdata(io)+green(io,iu)*
     &            tf_dj0(arg)*
     &            du*tf_costap(iu,0,0,ltap,rtap)*slo(iu)
              endif
            enddo
          endif
        enddo
c end of selection of expansion kernels
c----------------------------------------------------------------------
        if (debug) print *,'left loops'
        do io=nom+1,nsamp-nom+1
          sdata(io)=(0.d0,0.d0)
        enddo
c apply response (Fourier coefficients of source wavelet)
c -------------------------------------------------------
        if (optresponse) then
          do io=1,nom
            sdata(io)=sdata(io)*response(io)
          enddo
        endif
c 
c apply time shift
        do io=1,nom
          sdata(io)=sdata(io)*exp(ime*tanf*om(io))
        enddo
c
c make Fourier coefficients complete
        do io=1,nom-1
          sdata(nsamp-io+1)=conjg(sdata(io+1))
        enddo
c 
c transform to time domain
        if (debug) print *,'go fork'
        call tf_dfork(nsamp, sdata, 1.d0)
        if (debug) print *,'done fork'
c 
c copy trace to writing array and scale to impulse response
        scal=1.d0/(sqrt(float(nsamp))*dt)
        if (debug) print *,'move array'
        do io=1,nsamp
          fdata(io)=sngl(real(sdata(io)*scal))
          if ((debug).and.
     &       (abs(imag(sdata(io))).gt.(0.01*abs(real(sdata(io))))))
     &       print *,'WHERE does this imaginary part come from?'
        enddo
c consistency check:
        if (nsampwrite.gt.nsamp) stop 
     &    'ERROR: too many samples requested for output file'
        if (.not.(usemaster)) then
c prepare wid2line
          call sff_prepwid2(nsampwrite, 1./dt, 'NSP', 
     &      1999, 4, 18, 0, 0, 'NSP   ',
     &      'NSP   ', 'NSP   ', 0., -1., -1., -1., -1., wid2line, ierr)
          if (ierr.ne.0) stop 'ERROR: preparing WID2 line'
          c1=sngl(r(i)*cos(pi2*phi(i)/360.))
          c2=sngl(r(i)*sin(pi2*phi(i)/360.))
          c3=0.
          cs='C'
          nstack=0
        endif
        if (debug) print *,'go and write ',nsampwrite,' samples'
        call sff_WTraceI(lu, wid2line, nsampwrite, fdata, idata, last,
     &    cs, c1, c2, c3, nstack, ierr)
        if (debug) print *,'did writing'
        if (ierr.ne.0) stop 'ERROR: writing trace'
      enddo
c -----------------------
c end of loop over traces
c----------------------------------------------------------------------


c final check
      call checkslownesssampling(sngl(rmax),sngl(du),om(nom))
c 
      stop
   99 stop 'ERROR: reading command line argument'
   97 stop 'ERROR opening response file'
   96 stop 'ERROR reading response file'
   95 stop 'ERROR reading response file - unexpected end'
   94 stop 'ERROR closing response file'
   50 format(//////)
      end
c
c======================================================================
c 
c some subroutines
c
c----------------------------------------------------------------------
c 
c this routine reads in the data
c
      subroutine greenread(filename, debug,
     &     maxslo, maxfreq, slo, om,
     &     nslo, nom, green)
c
      character filename*(*)
      logical debug
      integer maxslo, maxfreq
      complex green(maxfreq, maxslo)
      real om(maxfreq), slo(maxslo)
      integer nslo, nom
      integer inmagic, cpu, match
      character*4 cmagic, incmagic
      parameter(cmagic='1234')
      equivalence(incmagic, inmagic)
      integer lu
      parameter(lu=20)
      integer i,j
c 
      if (debug) print *,'in subroutine greenread'
      print *,'read green file ',filename(1:index(filename, ' '))
      open(lu, file=filename, form='unformatted', status='old', err=99)

c check byte sex
      read(lu, err=98, end=97) inmagic
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
      elseif (match.eq.2) then 
        print *,'bytesex not matching - we will have to swap!'
        stop 'ERROR: do not know how to do that...'
      else
        close(lu, err=96)
        print *,'bytesex read is ',incmagic
        stop 'ERROR: bytesex is unkown - oh oh...'
      endif
      read(lu, err=98, end=97) nom, nslo
      if ((nom.gt.maxfreq).or.(nslo.gt.maxslo)) then
        close(lu, err=96)
        stop 'ERROR: data exceeds array bounds'
      endif
c read frequencies and phase slowness values
c frequency is given as angular frequency in 1/s
c phase slowness is given in s/m
      read(lu, err=98, end=97) (om(i), i=1,nom), (slo(i), i=1,nslo)
      read(lu, err=98, end=97) ((green(i,j), i=1,nom), j=1,nslo)
      close(lu, err=96)
      print *,'green file read and closed'

      return
   99 stop 'ERROR: opening green file'
   98 stop 'ERROR: reading green file'
   97 stop 'ERROR: reading green file - unexpected end'
   96 stop 'ERROR: closing green file'
      end
       
c----------------------------------------------------------------------
c
c provide sampling data
c
      subroutine sampling(dom, nom, dt, Ms, Ns, Ns2, verbose, debug)
c
      double precision dom
      real dt
      integer nom, Ms, Ns, Ns2
      logical debug, verbose
c
c input:
c   dom: sampling interval for angular frequency values
c   nom: number of frequencies
c   ms: maximum number of samples, i.e. dimension of time series array 
c   debug: produce debug output if true
c
c inout/output:
c   dt: upon input: desired sampling interval
c       upon output: feasible sampling interval
c   ns: upon input: desired number of samples
c       upon output: feasible number of samples
c
c output:
c   ns2: number of samples to be used for Fourier expansion 
c        (power of two; where ns <= ns2 <= ms)
c
      double precision pi2, duration
      parameter(pi2=2.d0*3.141592653589793115997)
      integer inns 
      real indt
c 
      if (debug) print *,'in subroutine sampling'
c 
c remember
      indt=dt
      inns=ns
c 
c time series duration to which dom corresponds 
      duration=pi2/dom
c 
      if (debug) then
        print *,'dom: ',dom
        print *,'duration: ',duration,'s'
        print *,'desired dt: ',dt,'s'
        print *,'desired ns: ',ns
        print *,'maximum ms: ',ms
      endif
c 
c determine ns2
      ns2=1
    1 continue
        ns2=ns2*2
      if (((2*ns2).le.ms).and.(abs((dt/(duration/ns2))-1.).gt.1.e-5)) goto 1
c 
      dt=sngl(duration/ns2)
      if (ns.gt.ns2) ns=ns2
      if ((nom*2).gt.ns2) stop
     &  'ERROR: too few samples: nom must be adjusted - not yet implemented'
      if (verbose) then
        if ((ns.ne.inns).or.(abs((dt/indt)-1.).gt.1.e-3)) then
          print *,'desired sampling parameters are inappropriate'
          print *,'desired values were: ',dt,'s interval and ',
     &      ns,' samples'
          print *,'temporal sampling set to: ',dt,'s interval and ',
     &      ns,' samples'
        endif
      endif
c 
      if (debug) then
        print *,'selected dt: ',dt,'s'
        print *,'selected ns: ',ns
        print *,'selected ns2: ',ns2
      endif
c 
      return
      end


c ----- END OF gresy.f -----
