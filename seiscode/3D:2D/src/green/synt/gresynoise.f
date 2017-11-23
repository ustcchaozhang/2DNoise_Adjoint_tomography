c this is <gresynoise.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
c
c calculate noise seismograms
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
c this file is obtained and modified from gresy.f (09/05/2007)
c
c REVISIONS and CHANGES
c    09/05/2007   V1.0   Thomas Forbriger
c    10/05/2007   V1.1   introduced randomization
c    11/05/2007   V1.2   allow selection of number of output samples
c    15/05/2007   V1.3   reordered loops; phase is of correct type now
c    21/05/2007   V1.4   provide longer seismograms by keep multiple copies of
c                        noisymized Fourier coefficients
c    24/05/2007   V1.5   write north and east component
c                        corrected buffer indexing
c                 V1.6   return to vertical and radial component
c    05/07/2007   V1.7   check slowness sampling interval
c    10/07/2007   V1.8   - complex conjugate was not applied at the correct
c                          index in the array of Fourier coefficients
c                        - use sqrt of Hanning taper rather than Hanning
c                          taper, since we add noise time series where signal
c                          power should be added, not amplitude
c                          Using a Hanning taper the number of sets used to
c                          construct the resulting time series is apparent
c                          from the envelope of the result. Using the sqrt of
c                          the Hanning taper, the envelope appears to be
c                          clean.
c    11/07/2007   V1.9   - Apply random scaling factor only per offset and not
c                          per set. The latter would result in non-stationary
c                          variance of the resulting time series.
c                        - Correction: Array scalefac was single precision but
c                          library function expects double precision
c                        - introduced verbose option
c    19/07/2007   V1.10  - Correction: R component for receivers was not
c                          stacked; R component was Z componten plus last
c                          receivers R component
c                          R should be correct now
c    23/07/2007   V1.11  - report rms values for all traces
c    07/09/2007   V1.12  - correct label for scaling factors
c                        - apply scaling for horizontal components
c
c ============================================================================
c
c - calculate impulse response (subsurface's Green's function)
c   for each receiver
c   the sampling of Fourier coefficients is already defined by the parameters
c   used together with syg; we can refine the sampling in the time domain by
c   zero-padding the spectra; we can extend the time window of seismograms by
c   stringing multiple compies of noisymize coefficients
c
c   we need space for
c   maxset              sets of Fourier coefficients
c   maxom               samples of each set of Fourier coefficients
c   2*maxom             samples in the Fourier transform array
c   (maxset+1)*maxom    samples for the output time series
c
c   actual numbers:
c   nset                selected through command line
c                       must be: nset <= maxset
c   nom                 number of Fourier coefficients to store in each set 
c                       selected through Green's coefficient file
c   nsetsamp            number of samples to obtain from each set
c                       largest values with nsetsamp <= 2*maxom
c   nsamp               number of output samples
c                       (nset+1)*nsetsamp/2
c
      program gresynoise
c
      character*(*) version
      parameter(version=
     &'GRESYNOISE   V1.12   calculate noise seismograms')
c
c dimensions
      integer maxtr, maxsetsamp, maxom, maxu, maxset, maxsamp
c      parameter(maxu=1000, maxtr=10000, maxom=4100, maxsamp=maxom*2)
      parameter(maxu=10000, maxtr=10000, maxom=4100)
      parameter(maxset=20, maxsamp=(maxset+1)*maxom, maxsetsamp=2*maxom)
      integer ntr, nsamp, Znom, Znu, Rnom, Rnu, nset, nsetsamp
c greens function
      character*78 greenname, Rgreenname
      complex Zgreen(maxom, maxu)
      complex Rgreen(maxom, maxu)
      real Zom(maxom), Rom(maxom)
      real Zslo(maxu), Rslo(maxu)
c receiver specifications
      real*8 rcvvred, rcvtli, rcvtre, rmax
      character*80 rcvtext
      real*8 phi(maxtr)
      double precision scalefac(maxtr)
c seismogram parameters
      integer pown
      character*80 Zseisname,rcvname,Rseisname
      real*8 r(maxtr)
      real zrms(maxtr), rrms(maxtr)
      integer imaxzrms,imaxrrms
      complex*16 Ztrans(maxsetsamp)
      complex*16 Zsets(maxom,maxset)
      complex*16 Zfourier(maxom)
      real Zfdata(maxsamp)
      integer Zidata(maxsamp)
      equivalence(Zfdata, Zidata)
      complex*16 Rtrans(maxsetsamp)
      complex*16 Rsets(maxom,maxset)
      complex*16 Rfourier(maxom)
      real Rfdata(maxsamp)
      integer Ridata(maxsamp)
      equivalence(Rfdata, Ridata)
c free block
      integer maxfree, nfree
      parameter(maxfree=5)
      character*80 free(maxfree)
c srce line
      character srctype*40, date*20, time*20, cs
      real c1, c2, c3
c info line
      integer nstack
c seismogram trace to sff file
      character cstation*5, cauxid*4, cinstype*6, cchannel*3
      character*132 wid2line
      logical last
      real dt
c taper
      real tf_costap
      real tf_sincostap
      integer ltap, rtap
c source response 
      double precision  phase(maxom)
      complex ime
      parameter(ime=(0.,1.))
c any
      integer lu, i, ierr, io, iu, is
      real*8 pi2, arg, du, dom, scal, pi
      parameter(lu=20,pi=3.141592653589793115997)
c functions
      real*8 tf_dj0, tf_dy0
      real*8 tf_dj1, tf_dy1
c options
      logical debug, optlambda, optnew, optresponse, hankel1, hankel2
      logical suppress, optrndscale, verbose, reportrms
      real lambdalim, tapfrac, scalexp
      integer limnsetsamp
      integer argnsets
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=14)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/'-d','-l','-o','-t','-i','-S','-1','-2','-e',
     &  '-r','-n','-m','-v','-R'/
      data opthasarg/.FALSE.,.TRUE.,.FALSE.,.TRUE.,4*.FALSE.,
     &  .TRUE.,.FALSE.,2*.TRUE.,2*.FALSE./
      data optarg/'-','1.','-','10.',4*'-','0.',3*'1',2*'-'/
c------------------------------------------------------------------------------
c basic information
c
      greenname=' '
      if (iargc().eq.1) call getarg(1, greenname)
      if ((greenname(1:5).eq.'-help').or.(iargc().lt.5)) then
        print *,version
        print *,'Usage: gresynoise Zgreen Rgreen Zseis Rseis rcvfile'
        print *,'                  [-l lambda] [-o] [-n n] [-m n]'
        print *,'                  [-t frac] [-e exp] [-r]'
        print *,'                  [-S] [-1] [-2] [-i] [-v]'
        print *,'                  [-d] [-R]'
        print *,'or     gresynoise -help'
        if (greenname(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'Calculates synthetic noise seismograms'
        print *,'from greens function.' 
        print *,' '
        print *,'Zgreen       file containing expension coeffcients'
        print *,'             produced by ''syg'' or ''greda'' '
        print *,'             for the vertical component'
        print *,'Rgreen       file containing expension coeffcients'
        print *,'             for the radial component'
        print *,'Zseis        output file for vertical component'
        print *,'Rseis        output file for radial component'
        print *,'rcvfile      receiver definition (refmet format)'
        print *,'-l lambda    limits the used slowness range by a'
        print *,'             minimum wavelength in meters'
        print *,'             (default: ',optarg(2)(1:4),')'
        print *,'-o           overwrite output'
        print *,'-i           calculate impulse response'
        print *,'             do not randomize the signals''s phase'
        print *,'-e exp       scale seismograms with offset'
        print *,'             scaling factor: offset**exp'
        print *,'-r           additionally apply random scaling'
        print *,'-t frac      tapering fraction for slowness domain'
        print *,'             taper given in percent of full slowness'
        print *,'             range (default: ',optarg(4)(1:4),')'
        print *,'-S           suppress zero frequency and zero slowness'
        print *,'-1           use Hankel 1 instead of Bessel'
        print *,'-2           use Hankel 2 instead of Bessel'
        print *,'-n n         number of sets to use'
        print *,'             time series will become larger with each'
        print *,'             additional set of Fourier coefficients'
        print *,'-m n         limit number of samples in one set to n'
        print *,'             if possible'
        print *,'-v           be more verbose'
        print *,'-d           produce debug output'
        print *,'-R           report rms values'
        print *,' '
        print *,'Input file units:'
        print *,' '
        print *,'  Slowness unit: s/m'
        print *,'  Frequency unit: angular frequency 1/s'
        print *,'  amplitude unit: m**3/s if spectrum represents'
        print *,'                  displacement waveform in m'
        print *,' '
        print *,'Array dimensions:'
        print *,'  maximum number of frequencies:     ',maxom
        print *,'  maximum number of slowness values: ',maxu
        print *,'  maximum number of offsets:         ',maxtr
        print *,'  maximum number of sets:            ',maxset
        print *,'  derived from this:'
        print *,'    maximum number of samples:       ',maxsamp
        print *,' '
        call refmet_rcvinf
        print *,' '
        call gninfo
        stop
      endif
      if (iargc().lt.3) stop 'ERROR: need more arguments'
c 
      call getarg(1, greenname)
      call getarg(2, Rgreenname)
      call getarg(3, Zseisname)
      call getarg(4, Rseisname)
      call getarg(5, rcvname)
c
      call tf_cmdline(6, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      debug=optset(1)
      optlambda=optset(2)
      if (optlambda) then
        read(optarg(2), *, err=99) lambdalim
      endif
      optnew=optset(3)
      read(optarg(4), *, err=99) tapfrac
      tapfrac=tapfrac/100.
      optresponse=.not.optset(5)
      suppress=optset(6)
      hankel1=optset(7)
      hankel2=optset(8)
      read (optarg(9), *, err=99) scalexp
      optrndscale=optset(10)
      read (optarg(11), *, err=99) argnsets
      argnsets=min(maxset, argnsets)
      argnsets=max(1,argnsets)
      print *,'number of sets to be used: ',argnsets
      nset=argnsets
      limnsetsamp=maxsetsamp
      if (optset(12)) read(optarg(12), *, err=99) limnsetsamp
      verbose=optset(13)
      reportrms=optset(14)
c 
      pi2=2.d0*pi
c 
c----------------------------------------------------------------------
c 
      if (debug) then
        print *,'greenname: ',greenname(1:index(greenname, ' ')-1)
        print *,'Rgreenname: ',Rgreenname(1:index(Rgreenname, ' ')-1)
        print *,'Zseisname: ',Zseisname(1:index(Zseisname, ' ')-1)
        print *,'Rseisname: ',Rseisname(1:index(Rseisname, ' ')-1)
        print *,'rcvname: ',rcvname(1:index(rcvname, ' ')-1)
      endif

      if (debug) then
        print *,'array dimensions:'
        print *,'maxu:          ',maxu
        print *,'maxtr:         ',maxtr
        print *,'maxom:         ',maxom
        print *,'maxset:        ',maxset
        print *,'maxsamp:       ',maxsamp
        print *,'maxsetsamp:    ',maxsetsamp
      endif
c
c----------------------------------------------------------------------
c read configuration and greens function
c 
      call refmet_rrcv(rcvname, rcvtext,
     &  rcvvred, rcvtli, rcvtre, ntr, maxtr, r, phi, 0.d0,
     &  0, 1, debug)
c
      print *,'receiver configuration was taken from:'
      print *,' ',rcvtext
      if (abs(rcvvred).gt.0.)
     &  print *,'WARNING: traveltime reduction will be ignored'
      if (abs(rcvtli).gt.0.)
     &  print *,'WARNING: left time shift will be ignored'
      if (abs(rcvtre).gt.0.)
     &  print *,'WARNING: right time shift will be ignored'
c 
      call greenread(greenname, debug,
     &     maxu, maxom, Zslo, Zom,
     &     Znu, Znom, Zgreen)
c 
      print *,'some information on green file ',
     &  'read for vertical component:'
      print *,'  name: ',greenname(1:index(greenname, ' ')-1)
      print *,' ',Znu,' slownesses from ',
     &  Zslo(1),'s/m to ',Zslo(Znu),'s/m'
      print *,' ',Znom,' frequencies from ',
     &  Zom(1)/pi2,'Hz to ',Zom(Znom)/pi2,'Hz'
      print *,' green frequency interval: ',(Zom(2)-Zom(1))/pi2,' Hz'
c 
      call greenread(Rgreenname, debug,
     &     maxu, maxom, Rslo, Rom,
     &     Rnu, Rnom, Rgreen)
c 
      print *,'some information on green file ',
     &  'read for radial component:'
      print *,'  name: ',Rgreenname(1:index(Rgreenname, ' ')-1)
      print *,' ',Rnu,' slownesses from ',
     &  Rslo(1),'s/m to ',Rslo(Rnu),'s/m'
      print *,' ',Rnom,' frequencies from ',
     &  Rom(1)/pi2,'Hz to ',Rom(Rnom)/pi2,'Hz'
      print *,' green frequency interval: ',(Rom(2)-Rom(1))/pi2,' Hz'
c 
c both sets of coefficients must be coherent, since we have to apply the same
c noise signal
      if (Znu.ne.Rnu) stop 'ERROR: numbers of slowness values differ'
      if (Znom.ne.Rnom) stop 'ERROR: numbers of frequencies differ'
      do io=1,maxom
        if (abs(Rom(io)-Zom(io)).gt.(1.e-5*Zom(i)))
     &    stop 'ERROR: inconsistent frequency'
      enddo
      do iu=1,maxu
        if (abs(Rslo(iu)-Zslo(iu)).gt.(1.e-5*Zslo(iu)))
     &    stop 'ERROR: inconsistent slowness'
      enddo
c 
c initialize arrays
      do is=1,maxset
        do io=1,maxom
          Zsets(io,is)=(0.,0.)
          Rsets(io,is)=(0.,0.)
        enddo
      enddo
      do io=1,maxsamp
        Zfdata(io)=0.
        Rfdata(io)=0.
      enddo
c 
c suppress zero frequency and slowness if requested
      if (suppress) then
        print *,'suppress zero frequency and zero slowness coefficients'
        do io=1,maxom
          Zgreen(io,1)=(0.,0.)
          Rgreen(io,1)=(0.,0.)
        enddo
        do iu=1,maxu
          Zgreen(1,iu)=(0.,0.)
          Rgreen(1,iu)=(0.,0.)
        enddo
      endif
c 
c 
c----------------------------------------------------------------------
c 
c build seismic traces
c 
c scale down epicentral distances to meters
      do i=1,ntr
        r(i)=r(i)*1000.
        scalefac(i)=1.
      enddo
      if (optrndscale) call tf_gsl_rng_ugaussian(scalefac, ntr)
c check stepwidth
      dom=Zom(2)-Zom(1)
      do io=2,Znom
        if (abs(Zom(io)-Zom(io-1)-dom).gt.(0.01*dom)) 
     &    print *,'WARNING: dom varies by more than 1%'
      enddo
c 
      du=Zslo(2)-Zslo(1)
      do iu=2,Znu
        if (abs(Zslo(iu)-Zslo(iu-1)-du).gt.(0.01*du)) 
     &    print *,'WARNING: du varies by more than 1%'
      enddo
c check trapezoid rule stepsize
      rmax=r(1)
      do i=1,ntr
        rmax=max(rmax,r(i))
        call checkslownesssampling(sngl(r(i)),sngl(du),Zom(Znom))
      enddo
c
      if (Zom(1).gt.(0.01*dom))
     &  stop 'ERROR: I assume the first frequency to be 0.Hz!'
c we have got all frequencies now and are going to fit them to
c a power of 2 number
      limnsetsamp=limnsetsamp
      limnsetsamp=min(maxsetsamp/2,limnsetsamp)
      limnsetsamp=max(2*Znom,limnsetsamp)
      limnsetsamp=limnsetsamp
      pown=0
    1 continue
        pown=pown+1
        nsetsamp=2**pown
        if (nsetsamp.lt.limnsetsamp) goto 1
      if (nsetsamp.lt.(2*Znom)) then
        print *,' limnsetsamp:',limnsetsamp
        print *,' nsetsamp:   ',nsetsamp
        print *,' maxsetsamp: ',maxsetsamp
        print *,' Znom:       ',Znom
        stop 'ERROR: cannot use enough samples'
      endif
c calculate sampling interval
      dt=sngl(pi2/(nsetsamp*dom))
      nsamp=(nset+1)*nsetsamp/2
      print *,'sets will have ',nsetsamp,' samples at ',dt,
     &  's sampling interval'
      print *,'final output series will have ',nsamp,
     &  ' samples'
c 
      if (hankel1) then
        print *,'I will use the first Hankel function'
      elseif (hankel2) then
        print *,'I will use the second Hankel function'
      else
        print *,'I will use the Bessel function'
      endif
c
c initialize rms index
      imaxzrms=1
      imaxrrms=1
c start receiver loop
c -------------------
      do i=1,ntr
c
c initialize rms values
        zrms(i)=0.
        rrms(i)=0.
c
c calculate bessel integral
c----------------------------------------------------------------------
c frequency loop
        do io=1,Znom
c set slowness taper
cc          if (debug) print *,'enter frequency loop'
          if (optlambda) then
            if (Zom(io).lt.dom) then
              rtap=Znu
              ltap=Znu
            else
              rtap=min(int((pi2/Zom(io)/lambdalim/du+1.)*
     &             (1.+tapfrac*0.5)),Znu)
              ltap=min(int((pi2/Zom(io)/lambdalim/du+1.)*
     &             (1.-tapfrac*0.5)),Znu)
            endif
            ltap=min(ltap,int(rtap*(1.-tapfrac)))
          else
            rtap=Znu
            ltap=int(rtap*(1.-tapfrac))
          endif
          if ((i.eq.1).and.(verbose)) then
            print *,'u-taper for ',Zom(io)/pi2,'Hz: ',ltap,'-',rtap,
     &        '   ',Zslo(ltap),'s/m-',Zslo(rtap),'s/m'
          endif
          Zfourier(io)=(0.d0,0.d0)
          Rfourier(io)=(0.d0,0.d0)
c
c start slowness loop (conditional: select base function)
c -------------------------------------------------------
cc          if (debug) print *,'enter slowness loop'
          if (hankel1) then
            do iu=1,rtap
              arg=max(Zslo(iu)*Zom(io)*r(i),1.d-100)
                Rfourier(io)=Rfourier(io)+Rgreen(io,iu)*
     &            (tf_dj1(arg)+(0.d0,1.d0)*tf_dy1(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*Rslo(iu)
                Zfourier(io)=Zfourier(io)+Zgreen(io,iu)*
     &            (tf_dj0(arg)+(0.d0,1.d0)*tf_dy0(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*Zslo(iu)
            enddo
            Zfourier(io)=0.5d0*Zfourier(io)
            Rfourier(io)=0.5d0*Rfourier(io)
          elseif (hankel2) then
            do iu=1,rtap
              arg=max(Zslo(iu)*Zom(io)*r(i),1.d-100)
                Rfourier(io)=Rfourier(io)+Rgreen(io,iu)*
     &            (tf_dj1(arg)-(0.d0,1.d0)*tf_dy1(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*Rslo(iu)
                Zfourier(io)=Zfourier(io)+Zgreen(io,iu)*
     &            (tf_dj0(arg)-(0.d0,1.d0)*tf_dy0(arg))*
     &            du*tf_costap(iu,0,0,ltap,rtap)*Zslo(iu)
            enddo
            Zfourier(io)=0.5d0*Zfourier(io)
            Rfourier(io)=0.5d0*Rfourier(io)
          else
            do iu=1,rtap
              arg=Zslo(iu)*Zom(io)*r(i)
                Rfourier(io)=Rfourier(io)+Rgreen(io,iu)*
     &            tf_dj1(arg)*
     &            du*tf_costap(iu,0,0,ltap,rtap)*Rslo(iu)
                Zfourier(io)=Zfourier(io)+Zgreen(io,iu)*
     &            tf_dj0(arg)*
     &            du*tf_costap(iu,0,0,ltap,rtap)*Zslo(iu)
            enddo
          endif
c end slowness loop
c -----------------
        enddo
c end of frequency loop
c----------------------------------------------------------------------
        if (debug) print *,'left loops'
c apply response
        scal=1.d0/(sqrt(float(nsamp))*dt)*(r(i)**scalexp)
        scal=scal*scalefac(i)
        print 51,i,r(i),scalefac(i),scal
        do is=1,nset
          if (optresponse) then
            call tf_gsl_rng_uniform(phase, Znom)
            do io=1,Znom
              Zfourier(io)=Zfourier(io)*cdexp(ime*pi2*phase(io))
              Rfourier(io)=Rfourier(io)*cdexp(ime*pi2*phase(io))
cc              print *,i,io,Zfourier(io),Rfourier(io),phase(io)
            enddo
          endif
c scale trace and stack
c R-component is scaled to be used as N- and E-component equally
cc          print *,'rec # ', i, '  scal: ',scal
          do io=1,Znom
cc            print *,i,io,is,Zfourier(io),Rfourier(io),scal
            Zsets(io,is)=Zsets(io,is)+Zfourier(io)*scal
            Rsets(io,is)=Rsets(io,is)+Rfourier(io)*scal*0.707
            zrms(i)=zrms(i)+
     &        sngl(scal**2*real(Zfourier(io)*conjg(Zfourier(io))))
            rrms(i)=rrms(i)+
     &        sngl(scal**2*real(Rfourier(io)*conjg(Rfourier(io))))
          enddo
        enddo
        zrms(i)=sqrt(zrms(i)/(nset*Znom))
        rrms(i)=sqrt(rrms(i)/(nset*Znom))
        if (zrms(i).gt.zrms(imaxzrms)) imaxzrms=i
        if (rrms(i).gt.rrms(imaxrrms)) imaxrrms=i
      enddo
c receiver loop ends here
c -----------------------
c      do io=1,Znom
c        print *,Zsdata(io),Rsdata(io)
c      enddo
c
      print *,'make Fourier coefficients complete...'
      print *,'transform sets and stack...'
      do io=1,nsamp
        Zfdata(io)=0.
        Rfdata(io)=0.
      enddo
c
      do is=1,nset
c make Fourier coefficients complete
c 
c parameters (where X means Z or R):
c Xsets     Fourier coefficients as obtained from Bessel expansion
c Xnom      Number of Fourier coefficients read from input file
c nsetsamp  Number of Fourier coefficients to be passed to FFT
c nsamp     Number of total sample for output file (is of no use here)
c           
        do io=1,nsetsamp
          Ztrans(io)=(0.d0,0.d0)
          Rtrans(io)=(0.d0,0.d0)
        enddo
        do io=1,Znom
          Ztrans(io)=Zsets(io,is)
          Rtrans(io)=Rsets(io,is)
        enddo
        do io=1,Znom-1
          Ztrans(nsetsamp-io+1)=conjg(Ztrans(io+1))
          Rtrans(nsetsamp-io+1)=conjg(Rtrans(io+1))
        enddo
c
        print *,'transform set ',is,' to time domain...'
c transform to time domain
        if (debug) print *,'go fork'
cc        print *,'go fork'
cc        do io=1,nsetsamp
cc          print *, io, Ztrans(io), Rtrans(io)
cc        enddo
        call tf_dfork(nsetsamp, Ztrans, 1.d0)
        call tf_dfork(nsetsamp, Rtrans, 1.d0)
        if (debug) print *,'done fork'
cc        print *,'done fork'
cc        do io=1,nsetsamp
cc          print *, io, Ztrans(io), Rtrans(io)
cc        enddo
c extract time series
        ltap=nsetsamp/2
        rtap=ltap+1
        if (is.eq.1) ltap=1
        if (is.eq.nset) rtap=nsetsamp+1
        do io=1,nsetsamp
c          scal=tf_costap(io,1,ltap,rtap,nsetsamp)
          scal=tf_sincostap(io,1,ltap,rtap,nsetsamp+1)
          i=io+(is-1)*(nsetsamp/2)
          Zfdata(i)=Zfdata(i)+sngl(real(Ztrans(io))*scal)
          if (debug) then
            write(10+is, *), i,io,scal
          endif
          if ((debug).and.
     &       (abs(imag(Ztrans(io))).gt.(0.01*abs(real(Ztrans(io))))))
     &       then
               print *,'WHERE does this imaginary part come from?'
               print *,' Z(io): ',io,Ztrans(io)
          endif
          Rfdata(i)=Rfdata(i)+sngl(real(Rtrans(io))*scal)
          if ((debug).and.
     &       (abs(imag(Rtrans(io))).gt.(0.01*abs(real(Rtrans(io))))))
     &       then
               print *,'WHERE does this imaginary part come from?'
               print *,' R(io): ',io,Rtrans(io)
          endif
c          print *,Zfdata(io),Rfdata(io)
        enddo
      enddo
c----------------------------------------------------------------------
c 
c open seismogram file
      print *,'write seismograms...'
c
      free(1)=version
      free(2)='green file '//greenname(1:index(greenname, ' '))
      free(3)='receiver configuration from '//
     &  rcvname(1:index(rcvname, ' '))
      free(4)=rcvtext
      nfree=4
      srctype='implicit in green file'
      cs='C'
      c1=0.
      c2=0.
      c3=0.
      date='990418'
      time='000000.000'
      c1=0.
      c2=0.
      c3=0.
      cs='C'
      nstack=0
      last=.false.
      cstation='NSP'
      cauxid='NSP'
      cinstype='NSP'
c only one trace per file
      last=.true.
c vertical component
c ------------------
      if (optnew) call sff_New(lu, Zseisname, ierr)
      if (ierr.ne.0) stop 'ERROR: deleting seismogram file'
      call sff_WOpenFS(lu, Zseisname, 
     &   free, nfree, srctype, cs, c1 ,c2 ,c3,
     &   date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening seismogram file'
c prepare wid2line
      if (debug) print *,'go and write',nsamp
      cchannel='Z'
      call sff_prepwid2(nsamp, 1./dt, cstation, 1999, 4, 18, 0, 0, 
     &  cchannel, cauxid, cinstype,
     &  0., -1., -1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line'
      call sff_WTraceI(lu, wid2line, nsamp, Zfdata, Zidata, last,
     &  cs, c1, c2, c3, nstack, ierr)
      if (debug) print *,'did writing'
      if (ierr.ne.0) stop 'ERROR: writing trace'
c radial component
c ---------------
      if (optnew) call sff_New(lu, Rseisname, ierr)
      if (ierr.ne.0) stop 'ERROR: deleting seismogram file'
      call sff_WOpenFS(lu, Rseisname, 
     &   free, nfree, srctype, cs, c1 ,c2 ,c3,
     &   date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening seismogram file'
c prepare wid2line
      if (debug) print *,'go and write',nsamp
      cchannel='R'
      call sff_prepwid2(nsamp, 1./dt, cstation, 1999, 4, 18, 0, 0, 
     &  cchannel, cauxid, cinstype,
     &  0., -1., -1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line'
      call sff_WTraceI(lu, wid2line, nsamp, Rfdata, Ridata, last,
     &  cs, c1, c2, c3, nstack, ierr)
      if (debug) print *,'written'
      if (ierr.ne.0) stop 'ERROR: writing trace'
c final check
      call checkslownesssampling(sngl(rmax),sngl(du),Zom(Znom))
c----------------------------------------------------------------------
c report rms values if verbose
      if (reportrms) then
        print *,'rms amplitudes:'
        do i=1,ntr
          print 52,i,zrms(i),rrms(i)
        enddo
        print *,'  first,largest,last:'
          print 52,1,zrms(1),rrms(1)
          print 53,imaxzrms,zrms(imaxzrms)
          print 54,imaxrrms,rrms(imaxrrms)
          print 52,ntr,zrms(ntr),rrms(ntr)

      endif
c 
      stop
   99 stop 'ERROR: reading command line argument'
   51 format(3x,'trace: ',i3,1x,'offset: ',f10.2,' m',
     &       2x,'rnd fac: ',f10.5,2x,'scaling: ',e10.3)
   52 format(5x,'trace: ',i3,' Z:',e12.5,' R:',e12.5)
   53 format(5x,'trace: ',i3,' Z:',e12.5)
   54 format(5x,'trace: ',i3,15x,' R:',e12.5)
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
      if (debug) print *,'DEBUG: entered subroutine greenread'
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
      subroutine gninfo
c
      print *,'What this program does:'
      print *,'-----------------------'
      print *,' '
      print *,'This program calculates synthetic noise '
     &       ,'seismograms for H/V analyses. For this'
      print *,'purpose it reads Fourier-Bessel expansion '
     &       ,'coefficients for the response to a'
      print *,'vertical force or explosion from files '
     &       ,'produced by the program syg for'
      print *,'vertical component and radial component '
     &       ,'displacement fields.'
      print *,' '
      print *,'The program can handle an almost arbitrarily '
     &       ,'large number of receivers.'
      print *,'For each receiver it does the following:'
      print *,'  1. The Fourier transform for vertical and '
     &       ,'radial component is calculated by'
      print *,'     evaluating the Bessel expansion for the '
     &       ,'given offset of the receiver.'
      print *,'     These are the Fourier coefficients of '
     &       ,'the impulse response of the'
      print *,'     subsurface to the defined source.'
      print *,'  2. Unless option -i is used, a phase value '
     &       ,'from uniform random distribution'
      print *,'     (between 0 and 2*pi) is added to each '
     &       ,'Fourier phase. At each frequency'
      print *,'     the same phase factor is used for the '
     &       ,'vertical and radial component,'
      print *,'     equivalent to an identical time shift in '
     &       ,'both components. This is the'
      print *,'     step of producing a noise sequence.'
      print *,'  3. If option -r is used, the amplitude of '
     &       ,'all Fourier coefficients together'
      print *,'     is scaled with a factor from a normal '
     &       ,'random distribution.'
      print *,'  4. If option -e is used the amplitude of '
     &       ,'the signal is scaled by'
      print *,'     r to the power of exp, where r is the '
     &       ,'offset and exp is the argument to'
      print *,'     option -e.'
      print *,'  5. The Fourier coefficients are added to '
     &       ,'the already existing Fourier'
      print *,'     coefficients. This is equivalent of '
     &       ,'simulating the signal at a single'
      print *,'     receivers that is exited by many sources '
     &       ,'at different offsets.'
      print *,'  6. If more then one set is used, steps 2 to '
     &       ,'5 are repeated for the other'
      print *,'     sets.'
      print *,'  The number of Fourier coefficients produced '
     &       ,'this way and the frequency range'
      print *,'  and interval is defined by the sampling of '
     &       ,'the input files produced by syg.'
      print *,' '
      print *,'After stcking the singals for all offsets, '
     &       ,'the sets are transformed to the'
      print *,'time domain and combined to a time series. '
     &       ,'The following steps are taken for'
      print *,'the vertical and the radial component signal:'
      print *,'  1. Fourier coefficients are zero-padded to '
     &       ,'the maximum number of samples'
      print *,'     that can be handled. This we the '
     &       ,'sampling interval of the time series'
      print *,'     will become smaller and samples will be '
     &       ,'smoothly interpolated. However'
      print *,'     the frequency range of non-zero signals '
     &       ,'is still defined by the input'
      print *,'     files. The frequency sampling of the '
     &       ,'input file on the other hand defines'
      print *,'     the length of the time series produced '
     &       ,'for each set.'
      print *,'     The number of samples in each set '
     &       ,'(interpolation, not length of time'
      print *,'     series) can be adjusted by option -m '
     &       ,'within reasonable ranges. Since the'
      print *,'     default is to use the maximum, the '
     &       ,'number can only be reduced.'
      print *,'  2. The zero-padded Fourier coefficients are '
     &       ,'transformed to the time domain.'
      print *,'  3. All sets are combined to one long time '
     &       ,'series. The are copied to one'
      print *,'     array, overlapping by the half of their '
     &       ,'length. The first series of the'
      print *,'     overlapping range is tapered by a '
     &       ,'squared cosine, while the second series'
      print *,'     is tapered by a suqared sine. Thus the '
     &       ,'mean amplitude remains stationary'
      print *,'     and each intermediate set effectively is '
     &       ,'hanning-tapered.'
      print *,'     This way (by using more than one set) '
     &       ,'the length of the output signal can'
      print *,'     be extended. If each set has duration T '
     &       ,'(defined by the frequency'
      print *,'     sampling of the input Fourier Bessel '
     &       ,'coefficients), then the total'
      print *,'     duration of the output is (N+1)*T/2, '
     &       ,'where N is the number of sets used.'
      print *,' '
      print *,'The horizontal component written by this program is the'
      print *,'radial component of the PSV-case. The simulation problem'
      print *,'to be solved for H/V studies requires N- and E-component'
      print *,'signals, where we assume that signals from all azimuths'
      print *,'contribute at the same level. For noise signals, the'
      print *,'power of the resulting signal equals the integral over'
      print *,'the power of the contributing signals.'
      print *,' '
      print *,'The power of the vertical component as an average over'
      print *,'contributions from all azimuths is:'
      print *,'  PZ = int_0^(2*pi) pz(phi) d phi / (2*pi),'
      print *,'where pz(phi) is the power of the wave contributing from'
      print *,'azimuth phi. For the horizontal components this is'
      print *,'  PN = int_0^(2*pi) cos(phi)**2 pr(phi) d phi / (2*pi)'
      print *,'for the north component and'
      print *,'  PE = int_0^(2*pi) sin(phi)**2 pr(phi) d phi / (2*pi)'
      print *,'for the east component respectively if pr(phi) is the'
      print *,'radial component in the PSV case.'
      print *,' '
      print *,'We take pz(phi)=pz, pr(phi)=pr constant for all phi.'
      print *,'The power scaling factor for PZ then is 1 and for PN and'
      print *,'PE is 1/2. Amplitudes scale with the square root of',
     &        ' power.'
      print *,'To use the radial component written by this program as'
      print *,'an effective horizontal component for both N and E,'
      print *,'we must scale its amplitude by 1/sqrt(2)=0.707.'
c
      return
      end
c
c ----- END OF gresynoise.f ----- 
