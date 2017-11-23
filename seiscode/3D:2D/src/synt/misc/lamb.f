c     this is <lamb.f>
c======================================================================
c $Id$
c
c Copyright 1996, 2010 by Thomas Forbriger
c Copyright (c) 1999 by Gerhard Mueller (IMG Frankfurt)
c
c This program calculates the response of a homogeneous halfspace to a
c single vertical force acting on the surface of the halfspace. This
c is the solution to the classical Lamb's problem (Lamb 1904, eq. 141).
c The algorithm used in this program is provided by Forbriger (1996).

c Lamb H., 1904. On the propagation of tremors over the surface of an
c elastic solid. Phil. Trans. Roy. Soc. London, 203: 1–42.
c
c Forbriger T., 1996. Interpretation von Oberflaechenwellen in der
c Flachseismik. Diplomarbeit, Institut fuer Geophysik, Universitaet Stuttgart.
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
c V1.0  Thomas Forbriger 25/4/1996
c V1.1  30/4/96     mit akausalem Daempfungsmodell
c V1.2  02/5/96     korrektes Vorzeichen der Kraft
c V1.3  03/5/96     reading parameters
c V1.4  08/5/96     fixed double-minus in qnumerator
c V1.5  25/01/11    implemented interface to libsff and libfapidxx
c V1.5a 08/01/2014  revised copyright and usage information
c
c======================================================================
      program lamb
c----------------------------------------------------------------------
c u:        horizontal slowness
c a:        vertical p-slowness corresponding to u
c b:        vertical s-slowness corresponding to u
c alpha:    p-velocity (in m/s)
c beta:     s-velocity (in m/s)
c rho:      density (in kg/(m**3))
c mu:       rigidity
c omega:    angular frequency
c q:        horizontal displacement pointing away from source (in m)
c w:        vertical displacement pointing into halfspace (in m)
c r:        receiver distances from source (in m)
c Force:    strength of a force pointing into the halfspace (in N)
c----------------------------------------------------------------------
      integer maxtraces, maxsamples, maxprolog
      parameter(maxtraces=200)
      parameter(maxsamples=4096)
      parameter(maxprolog=50)
      character*80 prolog(maxprolog)
      real distance(maxtraces)
      real fdata(maxsamples)
      real stmin,stsec,odt
      integer nsamples, ntraces, nprolog
      character*80 version, srcfctstr, filever, filehor
      character*50 oformat
      integer lu
      parameter(lu=12)

      complex*16 q(maxtraces, maxsamples)
      complex*16 w(maxtraces, maxsamples)
      complex*16 g(maxsamples)
      complex*16 a,b,rayleigh,qnumerator,wnumerator
      complex*16 J0,J1,Iq,Iw,pre
      complex*16 IME,alphaC,betaC,alpsq,betsq
      parameter (IME=(0.D0,1.D0))

      double precision r(maxtraces)
      double precision alpha,beta,rho,mu,qa,qb,HIN,RUECK
      double precision u,f(maxsamples),uq
      double precision tf_dj0,tf_dj1,jarg
      double precision umin,uwil,uwir,umax
      double precision fmin,fwil,fwir,fmax
      double precision dt,TS,pi,du,ddu,Tlen,fny,df,utap,ftap,t
      double precision omegau,costap,srcfct,srcl,Force
      double precision reray,imray,absray,raylim
      parameter(pi=3.1415926535898D0)
      parameter(HIN=-1.D0, RUECK=1.D0)

      integer len,i,j
      integer nu,iu,nf,if,iminf,imaxf,ir
      logical last
      
      srcfct(t,srcl)=dsin(pi*t/srcl)**3
      srcfctstr='srcfkt(t,srcl)=dsin(pi*t/srcl)**3'
      
      version=
     & 'LAMB  V1.5a The solution to Lamb''s problem (1904, eq. 141)'

c buggy fires up the floating exceptiong traps and is made
c available by libwo.a (just usable with Sun-Fortran)
c      call buggy
c======================================================================
c inform user
      print *,version
      print *,' '
      print *,'Calculating the response of an elastic halfspace'
      print *,'when being penetrated by a vertical single force'
      print *,'at r=0 and t=0. The z-axis ist counted positiv into'
      print *,'the halfspace. Positiv vertical displacements are'
      print *,'pointing into the halfspace. Positiv horizontal'
      print *,'displacements are pointing away from the source.'
      print *,'The displacement-results are given in meters.'
      print *,'The Force should be given in Newton. If the source-'
      print *,'length TS is less than the sampling interval a'
      print *,'single delta-pulse will be calculated. A finite'
      print *,'TS results in a sin^3-pulse.'
      print *,' '
      print *,'Copyright 1996, 2010 by Thomas Forbriger'
      print *,'Copyright (c) 1999 by Gerhard Mueller (IMG '
     &       ,'Frankfurt)'
      print *,' '
      print *,'This program calculates the response of a '
     &       ,'homogeneous halfspace to a'
      print *,'single vertical force acting on the surface '
     &       ,'of the halfspace. This'
      print *,'is the solution to the classical Lamb`s '
     &       ,'problem (Lamb 1904, eq. 141).'
      print *,'The algorithm used in this program is '
     &       ,'provided by Forbriger (1996).'
      print *,' '
      print *,'Lamb H., 1904. On the propagation of tremors '
     &       ,'over the surface of an'
      print *,'elastic solid. Phil. Trans. Roy. Soc. London, '
     &       ,'203: 1–42.'
      print *,' '
      print *,'Forbriger T., 1996. Interpretation von '
     &       ,'Oberflaechenwellen in der'
      print *,'Flachseismik. Diplomarbeit, Institut fuer '
     &       ,'Geophysik, Universitaet Stuttgart.'
      print *,' '
      print *,'This version is compiled for'
      print *,maxtraces,' receivers with ',maxsamples,' samples each'
      print *,' '
      print *,'please input model parameters'
      print *,'and parameters for numerical calculation:'
      print *,'-----------------------------------------'
      
c----------------------------------------------------------------------
c set parameters of model
      print *,'p-velocity [m/s], s-velocity [m/s], density [kg/m^3]'
      read(5, *) alpha, beta, rho
      if (alpha.eq.0.D0) stop 'ERROR: alpha is zero'     
      if (beta.eq.0.D0) stop 'ERROR: beta is zero'     
      if (rho.eq.0.D0) stop 'ERROR: rho is zero'     
c      alpha=6000.D0
c      beta=4000.D0
c      rho=2700.D0
      print *,'p-waves-quality, s-wave-quality'
      print *,'   (quality=0 means full elastic case)'
      read(5, *) qa, qb
      if (qa.eq.0.D0) print *,'>> using elastic case for p-waves' 
      if (qb.eq.0.D0) print *,'>> using elastic case for s-waves' 
c      qa=200.D0
c      qb=200.D0
c set seismogram parameters
      print *,'sampling-interval [s], number of samples [two''s power]'
      print *,'   (a number of samples value of 11 will result in'
      print *,'    seismograms with 2^11=2048 samples)'
      read(5, *) dt,len
      if (2**len.gt.maxsamples) stop 'ERROR: too many samples'
      if (dt.eq.0.D0) stop 'ERROR: sampling interval is zero'
      print *,'>> seismograms will have ',2**len,' samples each'
c      dt=.5D0
c two's power of seismogram length in samples
c      len=11
c set source parameters
      print *,'source-length [s], Force [kg*m/s^2]'
      read(5, *) TS,Force
      if (TS.eq.0.D0) print *,'>> using delta-impulse force'
      if (Force.eq.0.D0) stop 'ERROR: force is zero'
c      TS=10.D0
c      Force=1.D10
c set receiver distances
      print *,'number of receivers'
      print *,'   (must be less or equal ',maxtraces,')'
      read(5, *) ntraces
      if (ntraces.gt.maxtraces) stop 'ERROR: too many traces'
      do i=1,ntraces
        print *,'distance from source for receiver ',i,' [m]'
        read(5, *) r(i)
        if (r(i).lt.0.D0) stop 'ERROR: distance must be positiv'
      enddo
c      ntraces=40
c      if (ntraces.gt.maxtraces) stop 'ERROR: too many traces'
c      do i=1,ntraces
c        r(i)=10.D3*dble(i)
c      enddo
c      ntraces=4
c      if (ntraces.gt.maxtraces) stop 'ERROR: too many traces'
c      r(1)=300.D3
c      r(2)=600.D3
c      r(3)=900.D3
c      r(4)=1200.D3
c set numerical parameters for slowness integration
      print *,'umin [s/m], uwil [s/m], uwir [s/m], umax [s/m]'
      print *,'   (slowness range to be calculated:'
      print *,'    umin: minimal slowness being calculated'
      print *,'    uwil: left cosine-taper slowness'
      print *,'    uwir: right cosine-taper slowness'
      print *,'    umax: maximal slowness being calculated)'
      read(5, *) umin, uwil, uwir, umax
      if (umax.lt.umin) stop 'ERROR: umin is greater than umax'
      if (umin.lt.0.D0) stop 'ERROR: umin is negativ'
c      umin=0.D0
c      uwil=0.1D-10
c      uwir=0.9D-3
c      umax=1.2D-3
      print *,'nu'
      print *,'   (number of slowness steps between',umin,
     &      ' s/m and ',umax,' s/m)'
      read(5, *) nu
      if (nu.eq.0) stop 'ERROR: nu is zero'
c      nu=10000
c set numerical parameters for frequency-tapering
      print *,'fmin [Hz], fwil [Hz], fwir [Hz], fmax [Hz]'
      print *,'   (frequency range to be calculated:'
      print *,'    fmin: minimal frequency being calculated'
      print *,'    fwil: left cosine-taper frequency'
      print *,'    fwir: right cosine-taper frequency'
      print *,'    fmax: maximal frequency being calculated)'
      read(5, *) fmin, fwil, fwir, fmax
      if (fmax.lt.fmin) stop 'ERROR: fmin is greater than fmax'
      if (fmin.lt.0.D0) stop 'ERROR: fmin is negativ'
c      fmin=0.D0
c      fwil=0.0D0
c      fwir=0.85D0
c      fmax=0.95D0
      print *,'select file format from list:'
      call sff_help_formats
      read(5, '(a)') oformat
c----------------------------------------------------------------------


c----------------------------------------------------------------------
c set basic values
c   limit to stop rayleigh pole
      raylim=1.D-100
c   filenames for output
      filever='lamb.v.'//oformat
      filehor='lamb.h.'//oformat
c   number of samples
      nsamples=int(2.D0**len)
      Tlen=dble(nsamples)*dt
      if (nsamples.gt.maxsamples) stop 'ERROR: too many samples'
c   elastic constants and complex velocities for damping
      mu=rho*beta*beta
      if (qa.eq.0.D0) then
        alphaC=dcmplx(alpha)
      else
        alphaC=dcmplx(alpha)*dcmplx(1.D0,0.5D0/qa)
      endif
      if (qb.eq.0.D0) then
        betaC=dcmplx(beta)
      else
        betaC=dcmplx(beta)*dcmplx(1.D0,0.5D0/qb)
      endif
c   alpha- and beta-slowness (body waves)
      alpsq=1/(alphaC*alphaC)
      betsq=1/(betaC*betaC)
c   frequency-parameters
      nf=nsamples/2+1
      df=1.D0/Tlen
      fny=dble(nf-1)*df
      du=(umax-umin)/dble(nu)
      if (fmax.gt.fny) stop 'ERROR: stay in frequency range'
c   index-borders of frequency band
      iminf=int(fmin/df)+1
      imaxf=int(fmax/df)+2
      imaxf=max(imaxf,nf)
c   recalculate frequency bandwidth from index-borders
      fmin=df*dble(iminf-1)
      fmax=df*dble(imaxf-1)


c----------------------------------------------------------------------
c initialize arrays
      do j=1,nsamples
c   use impulse-response
        g(j)=dcmplx(1.D0,0.D0)
c   set array width frequency value to each step
        f(j)=df*dble(j-1)
        do i=1,ntraces
c   arrays for sum-results
          q(i,j)=dcmplx(0.D0,0.D0)
          w(i,j)=dcmplx(0.D0,0.D0)
        enddo
      enddo
c----------------------------------------------------------------------


c----------------------------------------------------------------------
c create source
      if (TS.gt.(3.D0*dt)) then
        print *,'creating source'
        print *,srcfctstr
        do i=1,nsamples
          t=dble(i-1)*dt
          g(i)=dcmplx(0.D0,0.D0)
          if (t.le.TS) g(i)=dcmplx(srcfct(t,TS),0.D0)
        enddo
        call dfork(nsamples,g,HIN)
      else
        srcfctstr='impulse'
      endif

c----------------------------------------------------------------------
c create prolog
 2    format(x,a24,f15.6,a24,e15.6)
 3    format(x,3(a14,f12.2))
 4    format(x,2(a24,f15.6))
 5    format(x,a30,4f12.4)
 6    format(x,a30,4f12.8)
 7    format(x,3(a16,i10))
      nprolog=12
      if (nprolog.gt.maxprolog) stop 'ERROR: too many prolog lines'
      write(prolog(1), '(a)') version
      write(prolog(2), '(a)') 'source signal:'
      write(prolog(3), '(a)') srcfctstr
      write(prolog(4), 2) 'source-length [s]:',TS,
     &    'source-strength [N]:',Force
      write(prolog(5), 3) 'Vp [m/s]:', alpha, 'Vs [m/s]:',beta,
     &    'rho [kg/m**3]:',rho
      write(prolog(6), 4) 'Quality alpha:',qa,
     &    'Quality beta:',qb
      write(prolog(7), 4) 'length [s]:',Tlen,
     &    'sampling interval [s]:',dt
      write(prolog(8), 4) 'Nyquist freq. [Hz]:',fny,
     &    'frequency step [Hz]:',df
      write(prolog(9), 5) 'frequency cos-taper [Hz]:',
     &    fmin,fwil,fwir,fmax
      write(prolog(10), 6) 'slowness cos-taper [s/m]:',
     &    umin,uwil,uwir,umax
      write(prolog(11), 7) 'frequencies: ',nf,
     &    'slownesses: ',nu,'samples: ',nsamples

c report
      print *,' '
      print *,'These are the parameters you gave me:'
      print *,'-------------------------------------'
      do i=1,nprolog-1
        write(*, *) prolog(i)
      enddo

c======================================================================
c start big slowness loop
      print *,'starting slowness loop'
      do iu=0,nu

        if (iu.eq.int(iu/50.)*50) print *,'slowness ',iu,u,'s/m'

c calculate frequency independent values
        u=dble(iu)*du+umin
        uq=u*u
c as defined in lamb's paper a and b should be 
c pure real positiv or pure imaginary positiv
c but our a or b is imaginary when lamb's one is real and vice versa !!
        a=cdsqrt(alpsq-dcmplx(uq))
        b=cdsqrt(betsq-dcmplx(uq))

c integrate with trapezoid rule
        ddu=du
        if ((iu.eq.0).or.(iu.eq.nu)) ddu=du*0.5D0

c set slowness taper
        utap=costap(umin,uwil,uwir,umax,u)
        ddu=ddu*utap

c calc rayleigh function
        rayleigh=dcmplx((2.D0*uq-betsq)**2)+dcmplx(4.D0*uq)*a*b
        reray=dble(rayleigh)
        imray=dimag(rayleigh)
        absray=abs(rayleigh)
c be aware of the rayleigh-pole
        if (absray.eq.0.D0) then
          rayleigh=raylim
        elseif (absray.lt.raylim) then
          rayleigh=dcmplx((reray/abs(reray)),(imray/abs(imray)))*
     &              raylim
        endif
c calculate the numerators for the greens function
        qnumerator=dcmplx(uq)*(betsq-dcmplx(2.D0*uq)-
     &               dcmplx(2.D0,0.D0)*a*b)
        wnumerator=dcmplx(u)*betsq*a*IME

c----------------------------------------------------------------------
c start big frequency loop
        do if=iminf,imaxf

          omegau=2.D0*pi*f(if)*u

c----------------------------------------------------------------------
c start receiver loop
          do ir=1,ntraces
            jarg=omegau*r(ir)
c d_j0 and d_j1 are the Besselfunction of the first kind
c of order zero and one and are available as intrinsic functions
c under Sun-Fortran
            J0=dcmplx(tf_dj0(jarg))
            J1=dcmplx(tf_dj1(jarg))

            Iq=J1*qnumerator/rayleigh*ddu
            Iw=J0*wnumerator/rayleigh*ddu
            q(ir,if)=q(ir,if)+Iq
            w(ir,if)=w(ir,if)+Iw

c end of receiver big loop
          enddo
c end of big frequency loop
        enddo
c end of big slowness loop
      enddo
      print *,'finished slowness loop'


c----------------------------------------------------------------------
c now supply all with full source-terms and slowness indipendent factors
      
      do if=iminf,imaxf

        ftap=costap(fmin,fwil,fwir,fmax,f(if))
        pre=dcmplx(ftap*(-Force)*f(if)/mu)*g(if)

        do ir=1,ntraces

          q(ir,if)=q(ir,if)*pre
          w(ir,if)=w(ir,if)*pre

        enddo
      enddo
      print *,'finished to complete spectra'

c======================================================================
c now go to time domain


c----------------------------------------------------------------------
c perform fourier transform on each trace

      do ir=1,ntraces

c make time series real
        do if=2,nf-1
          q(ir,nsamples-if+2)=dconjg(q(ir,if))
          w(ir,nsamples-if+2)=dconjg(w(ir,if))
        enddo

c fourier transform
c   calculate horizontal seismograms
        do if=1,nsamples
          g(if)=q(ir,if)
        enddo
        call dfork(nsamples, g, RUECK)
        do if=1,nsamples
          q(ir,if)=g(if)
        enddo

c   calculate vertical seismograms
        do if=1,nsamples
          g(if)=w(ir,if)
        enddo
        call dfork(nsamples, g, RUECK)
        do if=1,nsamples
          w(ir,if)=g(if)
        enddo

      enddo
      print *,'finished calculation of time series'
c----------------------------------------------------------------------
c write out signals
c   the subroutine seiswrite is made available from libtf.a and
c   just writes the complete set of receivers into one file

c stmin and stsec specify the beginning of the seismogram according to
c source-time and are only of historical interest
      stmin=0.
      stsec=0.

      print *,'select file format ',oformat(1:index(oformat, ' ')-1)
      call sff_select_output_format(oformat, i)
      if (i.ne.0) stop 'ERROR: selecting output format'

      odt=sngl(dt)
     
c write horizontals
      write(prolog(nprolog), '(a)') 'horizontal component'
      print *,'open horizontal component file'
      call fileopen(lu, filehor, maxprolog, nprolog, prolog)
      last=.false.
      do i=1,ntraces
        if (i.eq.ntraces) last=.true.
        distance(i)=sngl(r(i))
        do j=1,nsamples
          fdata(j)=real(q(i,j))
        enddo
        call sffu_simplewrite(lu, last, fdata, nsamples, odt,
     &    distance(i))
      enddo

c write verticals
      write(prolog(nprolog), '(a)') 'vertical component'
      print *,'open vertical component file'
      call fileopen(lu, filever, maxprolog, nprolog, prolog)
      last=.false.
      do i=1,ntraces
        if (i.eq.ntraces) last=.true.
        do j=1,nsamples
          fdata(j)=real(w(i,j))
        enddo
        call sffu_simplewrite(lu, last, fdata, nsamples, odt,
     &    distance(i))
      enddo

      print *,'finished'

      stop
      end


c======================================================================
      subroutine fileopen(lu, filename, maxprolog, nprolog, prolog)
c 
      integer lu
      character filename*(*)
      integer maxprolog, nprolog
      character*(*) prolog(maxprolog)
c
      integer ierr
      character st*20
      character cs*1
      real c1,c2,c3
      character date*6, time*10
c
      st='NSP'
      cs='C'
      c1=0.
      c2=0.
      c3=0.
      date='010209'
      time='000000.000'
c
      call sff_New(lu, filename, ierr)
      if (ierr.ne.0) stop 'ERROR (fileopen): creating file'
      call sff_WOpenFS(lu, filename, 
     &  prolog, nprolog, st, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR (fileopen): opening file'
      return
      end
c----------------------------------------------------------------------
c
c function costap     (it's a simple cosine-taper)
c
      double precision function costap(min,wil,wir,max,val)
      double precision min,wil,wir,max,val
      double precision pi
      parameter(pi=3.1415926535898D0)
      if (val.lt.min) then
        costap=0.D0
      elseif(val.le.wil) then
        if (wil.eq.min) then
          costap=0.D0
        else
          costap=0.5D0-0.5D0*dcos((val-min)*pi/(wil-min))
        endif
      elseif(val.gt.max) then
        costap=0.D0
      elseif(val.ge.wir) then
        if (max.eq.wir) then
          costap=0.D0
        else
          costap=0.5D0+0.5D0+dcos((val-wir)*pi/(max-wir))
        endif
      else
        costap=1.D0
      endif
      return
      end


c----------------------------------------------------------------------
c the following fast fourier transform is just copied from
c the program refseis from J. Ungerer
c
c This code was originally published by Gerhard Müller
c in his lecture notes on digital signal processing:
c Gerhard Mueller, 1999. Digitale Signalverarbeitung. Skriptum zur
c gleichnamigen Vorlesung. Institut für Meteorologie und Geophysik,
c Universität Frankfurt.
c
c The original algorithm appears to be due to Claerbout, J.F., 
c "Fundamentals of Geophysical Data Processing with Applications 
c to Petroleum Prospecting", McGraw-Hill, 1976.
c
c
C23456789012345678901234567890123456789012345678901234567890123456789012
C*** Subroutine fuer Fouriertransformation *****************************
C Die von Gerherd Mueller verwendetet schnelle Fouriertransformation   C
C FORK wurde umgeschrieben fuer DOUBLE COMPLEX                         C
C Es muessen implementiert sein: DOUBLE COMPLEX,DCMPLX,CDEXP           C
C                                                                      C
C Zum Verfahren der schnellen Fouriertransformation(FFT) und zur Ar-   C
C beitsweise von FORK siehe G.Mueller: Digitale Signalverarbeitung I,  C
C Vorlesungsmanuskript.                                                C
C                                                                      C
C Variablen:                                                           C
C    LX       Seismogrammlaenge bzw. Anzahl der Stuetzstellen,Abtast-  C
C             werte des Seismogramms/Spektrums.Muss eine Zeier-Potenz  C
C             sein.                                                    C
C    cx(LX)   Feld auf dessen Realteil die Funktionswerte der Zeit-    C
C             funktion stehen und nach Transformation ihre Fourierko-  C
C             effizienten.                                             C
C    SIGNI    SIGNI=-1.D0 bedeutet Berechnung der Fourierkoeffizienten C
C             SIGNI=+1.D0 bedeutet Ruecktransformation                 C
C**********************************************************************C     

      SUBROUTINE DFORK(LX,CX,SIGNI)
      INTEGER     I,ISTEP,J,L,LX,M
      REAL*8      SC,PI,SIGNI
      COMPLEX*16  CX(LX),CARG,CW,CTEMP

      PI=3.14159265358979D0
      J=1
      SC=1.D0/DBLE(LX)
      SC=DSQRT(SC)
      DO 5  I=1,LX
      IF(I.GT.J) GOTO 2
      CTEMP=CX(J)*SC
      CX(J)=CX(I)*SC
      CX(I)=CTEMP
2     M=LX/2
3     IF(J.LE.M) GOTO 5
      J=J-M
      M=M/2
      IF(M.GE.1) GOTO3
5     J=J+M
      L=1
6     ISTEP=2*L
      DO 8  M=1,L
      CARG=DCMPLX(0.,1.)*(PI*SIGNI*DBLE(M-1))/DBLE(L)
      CW=CDEXP(CARG)
      DO 8 I=M,LX,ISTEP
      CTEMP=CW*CX(I+L)
      CX(I+L)=CX(I)-CTEMP
8     CX(I)=CX(I)+CTEMP
      L=ISTEP
      IF(L.LT.LX) GOTO 6
      RETURN
      END
