c this is <grema.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1990 by Joachim Ungerer
c
c Modifcations:
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c this is a modified Version of resus just to extract a
c vertical component greens matrix from a reflectivity matrix
c
c This file contains code implemented by Joachim Ungerer (see refmet.f)
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
c    30/07/97   V1.0   Thomas Forbriger
c    11/12/99   V1.1   chnaged to tf_cmdline
c
c==============================================================================
      PROGRAM grema

      character*70 version
      parameter(version='GREMA   V1.1   reflectivity matrix to green')

c array dimension declaration
      integer mu, msl, mf
      PARAMETER  (MU=2100)
      PARAMETER  (MSL=4100) 
      PARAMETER  (Mf=MSL/2+1)
c======================================================================
c parameter declaration

      REAL*8      hin,rueck,pi
      PARAMETER  (hin=-1.D0,rueck=1.D0,pi=3.14159265358979d0)
      COMPLEX*16  IME
      PARAMETER  (IME=(0.D0,1.D0))

      CHARACTER text*70, sourcetext*72, receivertext*72, 
     &          outunits*8,line*80

      INTEGER   E,f,fmi,fma,fwl,fwr,h,i,k,l,Nf,Nff,
     &          Nu,Ny,SL,typ,outsig,srcsig

      REAL*8    Du,u,umin,umax,uwil,uwir,utap,uQ,gew,ZQ,
     &          Df,Dt,fmin,fmax,fwil,fwir,FL,Fny,TL,radius,
     &          fr(MSL),ftap(Mf),t(MSL),w(MSL),
     &          FFI,NFI,
     &          ap,hfkt,Thd,The,T1,T2,tvar,
     &          Tli,Tre,Vred,help,
     &          M0,M0dim,Mxx,Myy,Mzz,Mxy,Mxz,Myz,
     &          K0,K1,K2,K3,L1,L2

      COMPLEX*16  ah,bh,alphCh,betaCh,alphCtop,betaCtop,atop,btop

      COMPLEX*16  H11,H22,I11,I22,hilf,
     &            DDu,ea,eb,earg,red,g(MSL),
     &            In1,In2,In3,In4,In5,In6,
     &            AQ(0:2),BQ(0:2),CQ(0:2),DQ(0:2),EQ(2),FQ(2),
     &            B0(0:2),D0(0:2),F0(2)

      COMPLEX*16  C(MSL)

      complex     green(msl, mu)

      COMPLEX     IRM11(Mf),IRM12(Mf),IRM21(Mf),
     &            IRM22(Mf),ITR11(Mf),ITR12(Mf),
     &            ITR21(Mf),ITR22(Mf),irm(Mf),itr(Mf)

      real slo(mu), om(mf)
c magic number for binary file identification
      integer magic, lu, j
      character*4 cmagic
      parameter(cmagic='1234', lu=21)
c matrix reading
      character*80 matrixfile
      logical readmatrix
      integer matrixlu, ifmi, ifma
      character*70 matversion, mattext
      character*72 matsrctext, matmodtext
      parameter(matrixlu=20)
      real qah, qbh, qatop, qbtop
      real*8 alphah, betah, rhoh, zh
      real*8 alphatop, betatop, rhotop
      real*8 ifwil, ifwir, iuwil, iuwir
c velocity dispersion
      double precision nuref
      complex*16 dispfac(Mf)
      logical disperse
c earth flattening approximation
      double precision efa_z, efa_r
c source function
      character*80 hfktstr
c sff FREE
      integer sff_maxfree, sff_nfree, sff_freebase
      parameter(sff_maxfree=100)
      character*80 sff_free(sff_maxfree)
c sff data
      real fdata(MSL)
      integer idata(MSL)
      equivalence(fdata,idata)
c files
      character*80 mainfile, modelfile, sourcefile, receiverfile
      character*80 basename
c verbosity
      integer lev1,lev2,lev3,lev4
      parameter(lev1=0,lev2=1,lev3=2,lev4=3)
c commandline
      integer cl_maxopt, cl_lastarg, iargc
      parameter(cl_maxopt=6)
      character*2 cl_optid(cl_maxopt)
      character*40 cl_optarg(cl_maxopt)
      logical cl_optset(cl_maxopt), cl_opthasarg(cl_maxopt)
c command line options and parameter values
      integer cl_vlevel
      character*80 cl_comsel
      logical cl_debug, cl_rprogress, cl_comeach
c here are the keys to our commandline options
      data cl_optid/2h-d,2h-v,2h-o,2h-c,2h-s,2h-m/
      data cl_opthasarg/.FALSE.,2*.TRUE.,.FALSE.,2*.TRUE./
      data cl_optarg/1h-,1h1,9hgrema.out,1h-,2h  ,1h-/


c======================================================================
c 
c basic setup part
c 
c----------------------------------------------------------------------
c give basic information
      call grema_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  MSL, Mf, sff_maxfree)

c      call buggy

c set options
      call tf_cmdline(1, cl_lastarg,
     &     cl_maxopt, cl_optid, cl_optarg, cl_optset, cl_opthasarg)
      cl_debug=cl_optset(1)
      read(cl_optarg(2), '(i10)') cl_vlevel
      basename=cl_optarg(3)
      cl_comeach=cl_optset(4)
      cl_comsel=cl_optarg(5)
      readmatrix=cl_optset(6)
      if (readmatrix) then
        matrixfile=cl_optarg(6)
      else
        stop 'ERROR: you MUST give me a matrix file!'
      endif
      cl_rprogress=.false.
      if (cl_vlevel.lt.0) cl_rprogress=.true.
      cl_vlevel=abs(cl_vlevel)
      if (cl_debug) print *,'DEBUG: cl_vlevel', cl_vlevel
c get filename
      if (iargc().eq.(cl_lastarg)) stop 'ERROR: filename?'
      if (iargc().gt.(cl_lastarg+1))
     &  print *,'WARNING: additional parameters are ignored'
      call getarg((cl_lastarg+1), mainfile)

c======================================================================
c 
c read configuration from file
c error conditions will be worked out behind end statement
c 
c----------------------------------------------------------------------
c read main control file
      call refmet_rmain(mainfile, modelfile, sourcefile, receiverfile, 
     &  cl_vlevel, cl_debug, lev2, umin, uwil, uwir, umax, Nu, 
     &  fmin, fwil, fwir, fmax, Dt, SL, MSL, text)

c----------------------------------------------------------------------
c read source configuration
      call refmet_rsource(sourcefile, sourcetext, outunits,
     &  typ, outsig, srcsig, The, Thd, ZQ, M0, Mxx, Myy, Mzz, Mxy, Mxz, Myz,
     &  cl_vlevel, lev2, cl_debug)

c----------------------------------------------------------------------
c open matrix file
c
      if (cl_vlevel.gt.lev2) print 52,'opening ',
     &  matrixfile(1:index(matrixfile, ' ')-1)
      open(matrixlu, form='unformatted', status='old',
     &  file=matrixfile, err=99)
      read(matrixlu, err=98) matversion
      read(matrixlu, err=98) ifmi, ifma
      read(matrixlu, err=98) dt, SL, fmin, ifwil, ifwir, fmax
      read(matrixlu, err=98) Nu, umin, iuwil, iuwir, umax
      read(matrixlu, err=98) zq, radius
      read(matrixlu, err=98) qah, qbh, alphah, betah, rhoh, zh
      read(matrixlu, err=98) qatop, qbtop, alphatop, betatop, rhotop
      read(matrixlu, err=98) mattext, matmodtext, matsrctext
      read(matrixlu, err=98) nuref

      if (SL.gt.MSL) stop 'ERROR: too many samples'
      if (nu.gt.mu) stop 'ERROR: too many slownesses'



C**********************************************************************C
C 5. Vorberechnungen fuer Langsamkeit                                  C
C     -Schrittweite der Langsamkeit Du                                 C
C     -Korrektur von uwil und uwir auf die Schrittweitenskala          C
C**********************************************************************C
C     Schrittweite fuer u
      Du=(umax-umin)/DBLE(Nu-1)




C**********************************************************************C
C 6. Vorberechnungen fuer Frequenz und Zeit                            C
C     -Zeitlaenge TL des Seismogramms und Schrittweite Df der Frequenz C
C     -Berechnung der Frequenzen fr(f) und Zeiten t(f)                 C
C      und Umrechnung in Kreisfrequenz w                               C
C     -Korrektur von fmin,fwil,fwir,fmax auf die Schrittweitenskala    C
C      der Frequenz ,wegen Anpassung an die Fouriertransformation      C
C     -weitere Frequenz und Zeitdaten: Nf,Nff,FL,Ny,Fny                C
C     -Fensterfunktion ftap(f) der Frequenz mit Cos-Taper (verwendbar  C
C      als Frequenzfilter).                                            C
C**********************************************************************C

C     Zeitlaenge des Seismogramms und Schrittweite der Frequenz
      TL=SL*Dt
      Df=1.D0/TL

C     Berechnung der Frequenzen und Zeiten
      DO 200 f=1,SL
         fr(f)=DBLE(f-1)*Df
          t(f)=DBLE(f-1)*Dt
C        Umrechnung in Kreisfrequenz w
         w(f)=2.D0*pi*fr(f)
  200 CONTINUE  

c----------------------------------------------------------------------
c
c check fmax to be less than Fny
c
      Fny=(float(SL)/2.)*Df
      if (fmax.gt.Fny) then
        print *,'ERROR: frequency range exceeds Nyquist frequency'
        print *,'ERROR: Fny: ',Fny,'  fmax: ',fmax
        stop
      endif

C     Korrektur von fmin,fwil,fwir,famx auf die Schrittweitenskala der
C     Frequenz ,wegen Anpassung an die Fouriertransformation
      fmi=IDINT(fmin/Df)+1
      fwl=IDINT(fwil/Df)+1
      fwr=IDINT(fwir/Df)+1
      fma=IDINT(fmax/Df)+1

c use read values from matrix instead
      fmi=ifmi
      fma=ifma

      fmin=fr(fmi)
      fwil=fr(fwl)
      fwir=fr(fwr)
      fmax=fr(fma)

C     weitere Frequenz- und Zeitdaten:Nf,Nff,FL,Ny,Fny
      Nf=fma-fmi+1   
      Nff=fwr-fwl+1
      FL=1.D0/Dt
      Ny=SL/2+1
      Fny=fr(Ny)




C**********************************************************************C
C 7. Vorberechnungen fuer Empfaenger                                   C
C     -Umrechnung der Winkel phi(E) in Bogenmass phiB(E)               C
C     -Winkel- und Tensorparameter K0(E),K1(E),K2(E),K3(E),L1(E),L2(E) C
C**********************************************************************C

      if (typ.eq.1) then
c in case of a moment tensor source
c this formulation in fact ignores the receiver azimuth angle
C       Winkel- und Tensorparameter
        K0=Mzz
        K1=Mxz
        K2=0.5D0*(Myy-Mxx)
        K3=Mxx
        L1=Myz
        L2=Mxy
        print *,'WARNING: results will only be correct for explosions'
      else
c not a moment tensor source (single force)
        K0=0.d0
        K1=0.d0
        K2=0.d0
        K3=0.d0
        L1=0.d0
        L2=0.d0
      endif

c----------------------------------------------------------------------
c 
c spherical is NOT done by grema
      if (radius.gt.0.d0) stop 'ERROR: only flat models!'

c----------------------------------------------------------------------
c 
c evaluate selections for velocity dispersion
c 
c if reference frequency evuals zero or the dominant
c period value in main configuartion file is set to a negative
c value we will ignore velocity dispersion
c
c
      if (nuref.le.0.d0) then
        disperse=.false.

      else
c calculate with velocity dispersion
        disperse=.true.

c check frequency again (kind of paranoia)
        if (.not.(fr(fmi).gt.0.d0))
     &    stop 'ERROR: velocity dispersion: smallest frequency is zero'

c precalculate velocity factors now
        do f=fmi,fma
          dispfac(f)=1.d0/pi*log(fr(f)/nuref)+IME/2.d0
          if (cl_debug) print *,'DEBUG: fr, dispfac ',fr(f),dispfac(f)
        enddo

c please check Q-values against frequency range now!
c this must be done to avoid negative or zero velocities for low
c frequencies and small Q-values
c
c determine the samllest allow Q-factor for the given lowest frequency
c so that dispfac >-1.d0
        help=-1.d0*log(fr(fmi)/nuref)/pi                
        if (cl_debug) print *,'DEBUG: smallest allowed Q: ',help
        if ((qah.le.help).or.(qbh.le.help)) then
          print *,'ERROR: Q-values must not be smaller than ',help,
     &      ' for smallest frequency ',fr(fmi)
          stop 'ERROR: Q too small for velocity dispersion'
        endif
      endif
      if (cl_debug) print *,'DEBUG: nuref ',nuref

C**********************************************************************C
C 8. Vorberechnungen fuer Schichtmodell                                C
C     -Dimensionierung der Seismogramme und Spektren mit der Dimensio- C
C      nierungsvariablen M0dim (M0 -> M0dim) auf:                      C
C      Spektren:Verschiebungsdichte[cm*s],Geschwindigkeitsdichte[cm],  C
C               Beschleunigungsdichte[cm/s]                            C
C      Seismogramme:Verschiebung[cm],Geschwindigkeit[cm/s],Beschleu-   C
C               nigung[cm/(s*s)]                                       C
C     -Schichtdicken d(i)                                              C
C     -Herdschicht h                                                   C
C     -komplexe Wellengeschwindigkeiten                                C
c
c units are changed:
c   displacement [m], particle velocity [m/s] and acceleration [m/s^2] 
c   moment [N*m]
c 
C**********************************************************************C

      if (typ.eq.1) then
c moment tensor source
C       Dimensionierung
c        M0dim=M0/1.D20
c changed to (as 1N=1e5dyn)
        M0dim=M0/1.d15
      else
c single force
        M0dim=M0/1.d12
      endif


C     Komplexe Wellengeschwindigkeiten(hier gehen die Q-Faktoren ein)

c calulate frequency independent complex velocities
      if (.not.(disperse)) then
        alphCh=alphah*(1.D0+ 0.5D0*IME/Qah)
        betaCh= betah*(1.D0+ 0.5D0*IME/Qbh)
        alphCtop=alphatop*(1.D0+ 0.5D0*IME/Qatop)
        betaCtop= betatop*(1.D0+ 0.5D0*IME/Qbtop)
      endif


C**********************************************************************C
C 9. Normierte Anregungsfunktion g(f) des Herdes                       C
C  - Aperiodizitaetsfaktor ap                                          C   
C  - Impuls-Seismogramm fuer The=Thd=0 (frequenzbegrenzt durch ftap)   C
C  - Abspeichern der normierten Herdfunktion im Zeitbereich auf Feld g C
C  - Fouriertransformation in den Frequenzbereich(Aufruf von DFORK)    C
C  - Typ der Anregungsfunktion(typ=1 Verschiebung,typ=2 GeschwindigkeitC
C    typ=3 Beschleunigung)                                             C
C  - Frequenzfilterung mit ftap(f)                                     C
C**********************************************************************C
C     Aperiodizitaetsfaktor(nur relevant fuer die Spektren) 
c      ap=1.D0
c 
c normalizing factor for fourier transform 
c this is needed in case we want to interpret the spectra as
c answers to a delta pulse
c
c DFORK evaluates the Fourier Sum normalizing with factor 1/sqrt(N).
c A time limited Fourier Transform is equivalent to evaluating a
c sum with factor dt. A frequency limited Fourier Backtransform is
c equivalent to evaluating the Backsum with factor df=1/(SL*dt).
c
      ap=DSQRT(DBLE(SL))*Dt

C     Impuls-Anregung fuer The=Thd=0 (frequenzbegrenzt)
        DO 260 f=1,Ny
           g(f)=DCMPLX(1.)
  260   CONTINUE


C     spektrale Anregungsfunktionen fuer Verschiebung,Geschwindigkeit
C     oder Beschleunigung (mit Beruecksichtigung der Aperiodizitaet)
C     fuer das Teilspektrum bis zur Nyqistfrequenz,zusaetzlich wird 
C     der Frequenzfilter ftap(f) angelegt.
      DO 280 f=1,Ny
C        Anregung fuer Bodenverschiebung(spektrale Integration)
C        mit Zuweisung fuer Frequenz und Kreisfrequenz 0 
         IF(outsig.EQ.1) THEN
           fr(1)=1.D-30
           w(1)=2.D0*pi*fr(1)
           g(f)=g(f)/(IME*w(f)) *ftap(f)
C        Anregung fuer Bodengeschwindigkeit
         ELSE IF(outsig.EQ.2) THEN
           g(f)=g(f) *ftap(f)
C        Anregung fuer Bodenbeschleunigung(spektrale Differentiation)
         ELSE IF(outsig.EQ.3) THEN
           g(f)=g(f)*IME*w(f) *ftap(f)
         ENDIF
  280 CONTINUE


c======================================================================
c 
c do reports to FREE block and to stdout
c 
c we do not use the free block for syving but for terminal output
c it was most easy to copy this block unchanged
c
c----------------------------------------------------------------------
c 
c report on general configuration (main file)
c
      sff_nfree=0
c version
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=version

c input files
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='input files:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='    main file: '//
     &  mainfile(1:min(64,index(mainfile, ' ')-1))
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='   '//text
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  matrix file: '//
     &  matrixfile(1:min(64,index(matrixfile, ' ')-1))
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  source file: '//
     &  sourcefile(1:min(64,index(sourcefile, ' ')-1))
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='   '//sourcetext
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='receiver file: '//
     &  receiverfile(1:min(64,index(receiverfile, ' ')-1))
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='   '//receivertext

c information on matrix file
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  matrix was created by:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='    '//matversion
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  matrix comes from configuration:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='    '//mattext
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  matrix has source depth of:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='    '//matsrctext
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  matrix comes from model:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='    '//matmodtext
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  parameters set by matrix file:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=
     &  '    dt, SL, fmin, fmax, Nu, umin, umax, ZQ, radius, fmi, fma, nuref'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=
     &  '  the matrix file contains the material properties of the' 
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=
     &  '   receiver layer and source layer and depth of source layer.'
   
c slowness range
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='slowness range:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 400) 
     &  'umin[s/km]', 'uwil[s/km]', 'uwir[s/km]', 'umax[s/km]', 'Nu'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 410) umin,uwil,uwir,umax,nu
  400 format(5(a12))
  410 format(4(1x,f11.6),1x,i11)

c frequency range
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='frequency range:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 430)
     &  'fmin[Hz]', 'fwil[Hz]', 'fwir[Hz]', 'fmax[Hz]'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 440) fmin,fwil,fwir,fmax
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 450) '[fmin, fmax] Nf', Nf
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 450) '[fwil, fwir] Nff', Nff
  430 format(4a12)
  440 format(4(1x,f11.6))
  450 format('number of frequencies in ',a,i5)

c some more on seismogram sampling
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='seismogram parameters:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 460) SL
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 470) Dt, Df
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 480) TL, FNy 
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='traveltime reduction:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 490) Vred, Tli, Tre
  460 format('      seismogram length:',i6)
  470 format('  sampling interval [s]:',f11.6,t37,
     &       'frequency interval [Hz]:',f11.6)
  480 format('  seismogram length [s]:',f11.6,t37,
     &       ' nyquist frequency [Hz]:',f11.6)
  490 format('Vred [km/s]:',F8.4,'     Tr [s]:',F6.2,
     &          '     Tl [s]:',F6.2/)

c information on velocity dispersion
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      if (disperse) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'velocity dispersion is switched on'
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        write(sff_free(sff_nfree), 491) nuref
      else
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'velocity dispersion is switched off'
      endif
  491 format('  reference frequency is [Hz]:',f10.3)
      
c----------------------------------------------------------------------
c 
c basic output to stdout
c
      sff_freebase=sff_nfree 
      if (cl_vlevel.gt.lev1) then
        print 51,' '
        print 51,'Configuration:'
        print 51,'--------------'
        print 51,' '
        do i=2,sff_freebase
          print 50,sff_free(i)
        enddo
      endif
   50 format(a80)
   51 format(a)
   52 format(/a,1x,a)

c----------------------------------------------------------------------
c 
c more specific information on model etc.
c
c output earth model
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='earth model parameters:'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (radius.lt.0.) then
        sff_free(sff_nfree)='  model is interpreted as flat'
      else
        write(sff_free(sff_nfree), 290) radius
  290   format('  earth has radius of ',f10.3,'km')
      endif
      if (nuref.lt.0.d-20) then
        sff_free(sff_nfree)='  model is frequency independent'
      else
        write(sff_free(sff_nfree), 291) nuref
  291   format('  reference frequency for model parameters: ',f10.3,'Hz')
      endif
c output layering
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 306)
     &  'layer',' ','top[km]','Vp[km/s]','Vs[km/s]',
     &  'rho[g/cm^3]','Qalpha','Qbeta'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 303) 'receiver: ',
     &  alphatop, betatop, rhotop, qatop, qbtop
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 305) 'source: ',
     &  zh,alphah, betah, rhoh, qah, qbh

  301 format(2x,i5,1x,4(f9.4,1x),f11.4,2(1x,f8.1))
  302 format(2x,i5,1x,2(f9.6,1x),2(f9.5,1x),f11.4,2(1x,f8.1))
  303 format(2x,a15,10x,1x,2(f9.5,1x),f11.4,2(1x,f8.1))
  304 format(2x,a5,1x,f9.6,1x,10x,2(f9.5,1x),f11.4,2(1x,f8.1))
  305 format(2x,a15,1x,f10.4,1x,2(f9.4,1x),f11.4,2(1x,f8.1))
  306 format(2x,a5,1x,4(a9,1x),a11,2(1x,a8))

c output on source model
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='source parameters:'
c srcsig
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (srcsig.eq.1) then
        sff_free(sff_nfree)='  source signal is compiled function:'
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)='  '//hfktstr(1:78)
      elseif (srcsig.eq.2) then
        sff_free(sff_nfree)='  source signal is delta pulse'
      elseif (srcsig.eq.2) then
        sff_free(sff_nfree)='  source signal will be read from file'
      else
        sff_free(sff_nfree)='  WARNING undefined srcsig'
      endif
c outsig
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (outsig.eq.1) then
        sff_free(sff_nfree)='  source signal will be integrated once'
      elseif (outsig.eq.2) then
        sff_free(sff_nfree)='  source signal will be taken as is'
      elseif (outsig.eq.2) then
        sff_free(sff_nfree)='  source signal will be differentiated once'
      else
        sff_free(sff_nfree)='  WARNING undefined outsig'
      endif
c typ
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (typ.eq.1) then
        sff_free(sff_nfree)='  source is given by moment tensor'
      elseif (typ.eq.2) then
        sff_free(sff_nfree)='  source is given as a vertical single force'
      elseif (typ.eq.2) then
        sff_free(sff_nfree)='  source is given by force unit vector'
      else
        sff_free(sff_nfree)='  WARNING undefined typ'
      endif
c moment tensor 
      if (typ.eq.1) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),330) ZQ,Mxx
  330   FORMAT(8X, '      depth[km]:',F11.6,T45,' Mxx=',F7.3)
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),340) h,Myy
  340   FORMAT(8X, '       in layer:',I11,T45,' Myy=',F7.3)    
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),350) M0,Mzz
  350   FORMAT(8X, '   moment [N*m]:',E11.4,T45,' Mzz=',F7.3)
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),360) Mxy 
  360   FORMAT(8X,T45,' Mxy=',F7.3)
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),365) The,Mxz
  365   FORMAT(8X, '    onset Ts[s]:',F11.5,T45,' Mxz=',F7.3)
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),370) Thd,Myz
  370   FORMAT(8X, ' duration Td[s]:',F11.5,T45,' Myz=',F7.3/)
      elseif (typ.eq.2) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),331) ZQ,0.
  331   FORMAT(8X, '      depth[km]:',F11.6,T45,'  Fx=',F7.3)
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),341) h,0.
  341   FORMAT(8X, '       in layer:',I11,T45,'  Fy=',F7.3)    
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),351) M0,1.
  351   FORMAT(8X, '      force [N]:',E11.4,T45,'  Fz=',F7.3)
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        WRITE(sff_free(sff_nfree),361) The,Thd
  361   FORMAT(8X, '    onset Ts[s]:',F11.5,T45,' duration Td[s]:',F11.5)
      endif
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='  resulting seismogram units are: '//outunits

c----------------------------------------------------------------------
c 
c specific output to stdout
c
      if (cl_vlevel.gt.lev3) then
        do i=sff_freebase+1,sff_nfree
          print 50,sff_free(i)
        enddo
      endif


C**********************************************************************C
C                                                                      C
C Hauptteil des Programms:Berechnung des Seismogrammes                 C
C                                                                      C
C***********************************************************************



C~~~~~Grosse Langsamkeitsschleife ueber u ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C
      DO 4000 l=1,Nu

C**********************************************************************C
C 11.Vorberechnungen von Ausdruecken derselben Langsamkeit u:          C
C     -Langsamkeitsparameter u, und sein Quadrat uQ                    C
C     -vertikale Langsamkeiten aller Schichten                         C
C     -Reflektions- und Transmissionskoeffizienten aller Schichten     C
C     -Fensterfunktion utap der Langsamkeit mit Cos-Taper              C
C     -Gewichtungsfaktor gew und Multiplikationsfaktor DDu fuer Trapez-C
C      regel                                                           C
C**********************************************************************C

C     Langsamkeitsparameter u
      u=umin+DBLE(l-1)*Du
      uQ = u*u
      if (cl_rprogress) print 500,l,u
  500 format(3x,'slowness #',i5.5,3x,f10.6,' s/km')

c 
c in the case of velocity dispersion we have to move the complete
c interface matrix calculation block inside the frequency look
c
      if (.not.(disperse)) then
C     Berechnung der vert. Langsamkeiten aller Schichten i:
C     Der Realteil soll fuer w>0 positiv, der Imaginaerteil negativ 
C     sein.Die Wurzelfunktion CSQRT arbeitet so, dass der Realteil der
C     gezogenen Wurzel stets positiv ist und der Imaginaerteil mit dem
C     Vorzeichen versehen wird,das der Imaginaerteil des Arguments hat.
C     Weil der Imaginaerteil des Arguments zur Bildung der vertikalen
C     Langsamkeiten negativ ist, wird obige Forderung erfuellt.
        H11= 1.D0/(alphCh*alphCh)
        H22= 1.D0/(betaCh*betaCh)
        ah = SQRT(H11-uQ)
        bh = SQRT(H22-uQ)
        H11= 1.D0/(alphCtop*alphCtop)
        H22= 1.D0/(betaCtop*betaCtop)
        atop = SQRT(H11-uQ)
        btop = SQRT(H22-uQ)
      endif


C     Fensterfunktion utap der Langsamkeit mit Cos-Taper
      IF (umin.LE.u .and. u.LT.uwil) THEN
         utap=0.5D0*(1.D0-cos(pi*(u-umin)/(uwil-umin)))
      ELSE IF(uwil.LE.u .and. u.LE.uwir) THEN
         utap=1.D0
      ELSE IF (uwir.LT.u .and. u.LE.umax) THEN
         utap=0.5D0*(1.D0+cos(pi*(u-uwir)/(umax-uwir)))
      ENDIF

C     Gewichtungsfaktor fuer Trapezregel.Der erste und letzte Funk-
C     tionswert werden nur halb gewichtet(gew=0.5)
      gew=1.D0
      IF ( l.EQ.1  .OR. l.EQ.Nu ) gew=0.5D0
C     getaperte und gewichtete Integrationsschrittweite fuer Trapezregel
      DDu=DCMPLX(Du*gew*utap)

c----------------------------------------------------------------------
c read matrix
      read(matrixlu, err=98) 
     &  (ITR11(f), ITR12(f), ITR21(f), ITR22(f),
     &   IRM11(f), IRM12(f), IRM21(f), IRM22(f),
     &   Itr(f), Irm(f), f=fmi,fma)

C~~~~~Grosse Frequenzschleife ueber w ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C
      DO 3000 f=fmi,fma

c 
c using velocity dispersion we have calculate the complex velocities
c each interface matrix for each frequency
c 
      if (disperse) then
        alphCh=alphah*(1.D0+dispfac(f)/Qah)
        betaCh= betah*(1.D0+dispfac(f)/Qbh)
        H11= 1.D0/(alphCh*alphCh)
        H22= 1.D0/(betaCh*betaCh)
        ah = SQRT(H11-uQ)
        bh = SQRT(H22-uQ)
        alphCtop=alphatop*(1.D0+dispfac(f)/Qatop)
        betaCtop= betatop*(1.D0+dispfac(f)/Qbtop)
        H11= 1.D0/(alphCtop*alphCtop)
        H22= 1.D0/(betaCtop*betaCtop)
        atop = SQRT(H11-uQ)
        btop = SQRT(H22-uQ)
      endif

C**********************************************************************C
C 15. Berechnung der Oberflaechenamplituden B0(i),D0(i) mit i=0,1,2    C
C     und F0(k) mit k=1,2.                                             C
C     Zuerst werden die Quellamplituden AQ(i),CQ(i),EQ(k) und BQ(i),   C
C     DQ(i),FQ(k) bestimmt und daraus dann die Oberflaechenamplituden  C
C     mit Hilfe der Reflektivitaeten und Transmissivitaeten des Herdes C
C     berechnet .                                                      C
C**********************************************************************C

C     Bestimmung der Amplitudenexponenten
      hilf=IME*w(f)*(ZQ-zh)
      earg=ah*hilf
      ea=EXP(earg)
      earg=bh*hilf
      eb=EXP(earg)

C     source amplitudes
      if (typ.eq.1) then

c moment tensor source
        AQ(0)=IME*ah*ea
        AQ(1)=2.D0*u*ea
        AQ(2)=IME*u*u/ah*ea 
        CQ(0)=IME*u*eb
        CQ(1)=(u*u/bh-bh)*eb
        CQ(2)=-IME*u*eb
        EQ(1)=eb/(betaCh*betaCh)
        EQ(2)=EQ(1)*IME*u/bh

        BQ(0)=IME*ah/ea
        BQ(1)=-2.D0*u/ea
        BQ(2)=IME*u*u/(ah*ea)
        DQ(0)=-IME*u/eb
        DQ(1)=(u*u/bh-bh)/eb
        DQ(2)=IME*u/eb
        FQ(1)=-1.D0/(betaCh*betaCh*eb)
        FQ(2)=-FQ(1)*IME*u/bh

      elseif (typ.eq.2) then

c vertical single force
        AQ(0)=u*ea
        AQ(1)=dcmplx(0.d0)
        AQ(2)=dcmplx(0.d0)
        CQ(0)=u*u*eb/bh
        CQ(1)=dcmplx(0.d0)
        CQ(2)=dcmplx(0.d0)
        EQ(1)=dcmplx(0.d0)
        EQ(2)=dcmplx(0.d0)

        BQ(0)=-u/ea
        BQ(1)=dcmplx(0.d0)
        BQ(2)=dcmplx(0.d0)
        DQ(0)=u*u/(bh*eb)
        DQ(1)=dcmplx(0.d0)
        DQ(2)=dcmplx(0.d0)
        FQ(1)=dcmplx(0.d0)
        FQ(2)=dcmplx(0.d0)

      endif

c calculate surface amplitudes
      DO 1300 i=0,2
C        Multiplikation von RM(inus) mit Vektor [AQ(i),CQ(i)] und
C        Addition zu Vektor [BQ(i),DQ(i)]; abgelegt auf [I11,I22].
         I11=BQ(i)+IRM11(f)*AQ(i)+IRM12(f)*CQ(i)
         I22=DQ(i)+IRM21(f)*AQ(i)+IRM22(f)*CQ(i)
C        Multiplikation der Matrix [H11,H12,H21,H22] mit [I11,I22]
         B0(i)=ITR11(f)*I11+ITR12(f)*I22
         D0(i)=ITR21(f)*I11+ITR22(f)*I22
 1300 CONTINUE


C     Berechnung der Oberflaechenamplitude F0(k)
      DO 1400 k=1,2
         F0(k)=Itr(f)*(FQ(k)+Irm(f)*EQ(k))
 1400 CONTINUE
   
 

C**********************************************************************C
C 16. Vorberechnungen fuer die Berechnung der Integrandenglieder       C
C**********************************************************************C
      In1=u*B0(0)+btop*D0(0)
      In2=u*B0(2)+btop*D0(2)
      In3=u*B0(1)+btop*D0(1)
      In4=atop*B0(0)-u*D0(0)
      In5=atop*B0(2)-u*D0(2)
      In6=atop*B0(1)-u*D0(1)
      if (typ.eq.1) then
c moment tensor source
        FFI=w(f)*w(f)/(4.D0*pi*rhoh)*M0dim
      elseif (typ.eq.2) then
c single vertical force
        FFI=w(f)/(4.d0*pi*rhoh)*M0dim
      endif



C**********************************************************************C
C 17. Berechnung der einzelnen Integrandenglieder der spektralen Ver-  C
C     schiebungskomponenten fuer eine Langsamkeit,aber fuer alle Fre-  C
C     quenzen und Empfaenger:                                          C
C       -zuerst Aufruf der Besselfunktionen                            C
C       -Fernfeldintegrandenglieder IDFr(f,E),IDFphi(f,E),IDFz(f,E)    C
C       -Nahfeldintegrandenglieder  IDNr(f,E),IDNphi(f,E),IDNz(f,E)    C
C***********************************************************************

c calculate integrands

      if (typ.eq.1) then
c for a moment tensor source
        hilf=(K0*In4+K3*In5)
        green(f,l)=FFI*IME*u*hilf
      elseif (typ.eq.2) then
c for a vertical single force
        green(f,l)=FFI*IME*In4
      endif
c rescale units for slowness in s/m
      green(f,l)=green(f,l)/max(u,1.d-100)*1.e6

      om(f)=w(f)
 3000 CONTINUE
C~~~~~Ende der grossen Frequenzschleife~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C

c rescale slowness to s/m
      slo(l)=u*1.e-3
 4000 CONTINUE
C~~~~~Ende der grossen Langsamkeitsschleife~~~~~~~~~~~~~~~~~~~~~~~~~~~~C
  
c----------------------------------------------------------------------
c 
c close matrix file
c
      if (readmatrix) then
        if (cl_vlevel.gt.lev2) print 52,'closing ',
     &    matrixfile(1:index(matrixfile, ' ')-1)
        close(matrixlu, err=96)
      endif

c write green code (easy to use)
      print *,'opening green file ',basename(1:index(basename,' ')),
     &    ' - overwrite mode'
      open(lu, file=basename, form='unformatted', err=88)
      call tf_magic(cmagic, magic)
      write(lu, err=87) magic
      write(lu, err=87) nf, nu
      write(lu, err=87) (om(i), i=1,nf), (slo(i), i=1,nu)
      write(lu, err=87) ((green(i,j), i=1,nf), j=1,nu)
      close(lu, err=86)
c 

c======================================================================
c 
c end of action 
c
      stop
c----------------------------------------------------------------------
c 
c work out error conditions
c 
   99 stop 'ERROR: opening file'
   98 stop 'ERROR: reading file'
   97 stop 'ERROR: reading file - unexpected end of file'
   96 stop 'ERROR: closing file'
   95 stop 'ERROR: writing file'
   88 stop 'ERROR: opening green file'
   87 stop 'ERROR: writing green file'
   86 stop 'ERROR: closing green file'
      END
 
C *** Ende REFSEIS ****************************************************C



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

      PI=3.14159265358979d0
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
      CW=EXP(CARG)
      DO 8 I=M,LX,ISTEP
      CTEMP=CW*CX(I+L)
      CX(I+L)=CX(I)-CTEMP
8     CX(I)=CX(I)+CTEMP
      L=ISTEP
      IF(L.LT.LX) GOTO 6
      RETURN
      END


c======================================================================
c 
c give basic information
c 
      subroutine grema_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  MSL, Mf, sff_maxfree)
c 
      character version*(*), mainfile*(*)
      integer lev1, lev2, lev3, lev4
      integer MSL, Mf, sff_maxfree
c 
      integer iargc
c 
      print *, version
      print *, 'Usage: grema [-d] [-v level] [-o basename] [-c]'
      print *, '              [-s select] -m file file'
      print *, '   or: grema -help'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, mainfile)
      if (mainfile.eq.'-help') then
        call refmet_intro
        print *,' '
        print *,' NOTICE: This is not refmet this is GREMA.'
        print *,' NOTICE: This will need a precalculated response'
        print *,'         matrix to serve you with a green matrix!'
        print *,'         (use refmat to calculate a response matrix)'
        print *,' '
        print *,'commandline parameters are:'
        print *,' '
        print *,'-d           Give debugging output.'
        print *,'-v level     Set verbosity level. The parameter may be'
        print *,'             any integer value. The higher the value'
        print *,'             the more output will be produced.'
        print *,'-o basename  Define output files basename.'
        print *,'             (default is: grema.out)'
        print *,'-m file      Name of matrix file created by refmat.' 
        print *,' '
        print *,'file         Is the name of the main configuration file.'
        print *,'             It contains the names of the three file'
        print *,'             containing the earth model, the source model'
        print *,'             and the receiver coordinates. In addition there'
        print *,'             must be given some numerical parameters for the'
        print *,'             calculation.'
        print *,' '
        print *,'This programs uses a precalculated response matrix (see '
        print *,'refmat) for layered media to build synthetic green matrices. '
        print *,'Therefore we do not use a modelfile. You must provide this '
        print *,'program with a main configuration file which must set '
        print *,'a source configuration and a receiver configuration file. '
        print *,'Only vertical component expansion coefficients will'
        print *,'be calculated. Source functions are ignored. You may'
        print *,'use only explosion or single vertical force sources.'
        print *,' '
        print *,'The following parameters give in your configuration will '
        print *,'be overwritten by the values stored in the response matrix '
        print *,'file: '
        print *,' '
        print *,' - the sampling interval '
        print *,' - the seismogram length '
        print *,' - the minimum and maximum frequency '
        print *,' - the minimum and maximum slowness '
        print *,' - the number of slowness steps'
        print *,' - the source depth'
        print *,' - the earth radius'
        print *,' - and (as you did expect) all earth model parameters'
        print *,' '
        print *,'Tapers in frequency and slowness domain are ignored.'
        print *,' '
        call comments
        print *,' '
        print *,'Verbosity levels are: '
        print *,'  =',lev1,'    no output'
        print *,'  >',lev1,'    report basic configuration'
        print *,'  >',lev2,'    report reading and writing files'
        print *,'  >',lev3,'    model, receivers and source are reported'
        print *,'  >',lev4,'    report on results'
        print *,'The same values but with negative sign will cause a'
        print *,'report on calculation progress.'
        print *,' '
        print *,'Array dimensions compiled into this version:'
        print *,'      maximum number of samples: ',MSL
        print *,'  maximum number of frequnecies: ',Mf
        print *,'   maximum number of FREE lines: ',sff_maxfree
c call other info routine
        call refmet_maininf
        call refmet_rcvinf
        stop
      endif
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine comments()
      print *,'For complete information on the structure of the four'
      print *,'input files look at the source code of refmet.f or at'
      print *,'the provided example files.'
      print *,' '
      print *,'* A model with earth radius negative will be interpreted'
      print *,'  as a flat model (no amplitude correcting will be applied).'
      print *,'* The model must contain a top halfspace and a bottom.'
      print *,'  halfspace.'
      print *,' '
      print *,'The physical units are:'
      print *,' '
      print *,'     layer velocity:  km/s'
      print *,'            density:  g/cm^3'
      print *,'   depth & distance:  km'
      print *,'              angle:  degrees'
      print *,'               time:  seconds'
      print *,'          frequency:  Hz'
      print *,'           slowness:  s/km'
      print *,'       displacement:  m'
      print *,'  particle velocity:  m/s'
      print *,'       acceleration:  m/s^2'
      print *,'              force:  N'
      print *,'             moment:  N*m'
      print *,' '
      print *,'The calculated expansion coeffcients will be appropriate'
      print *,'for slowness in s/m and frequency in cps and the following'
      print *,'expansion formula:'
      print *,' '
      print *,'   U(w,r) = int_0^infinity G(w,u) J_0(u*w*r) u du'
      print *,' '
      print *,'   w    is the angular frequency'
      print *,'   r    is the epicentral distance in m'
      print *,'   u    is the slowness in s/m'
      print *,'   U    is the vertical displacement component'
      print *,'   G    are the calculated expansion coefficients'
      print *,' '
      print *,'Here are some essential comments on the source function,'
      print *,'the source units and the corresponding seismogram units:'
      print *,' '
      print *,'  REFMET expects a source function in the form A*s(t)'
      print *,'  with s(t) defining the time dependence of the source'
      print *,'  and A being the amplitude. A is equal to M0 [N*m] for a'
      print *,'  moment tensor source and equal to F0 [N] for a single'
      print *,'  force source. '
      print *,'    The function hfkt in the source code (or the read in'
      print *,'  time series) is expected to be s''(t) which is the'
      print *,'  first time derivative of s(t). In this case the'
      print *,'  seismograms (without any further source integration or'
      print *,'  derivation) will be particle velocity in meters per'
      print *,'  second. Integration will lead to displacement in meters.'
      print *,'  Derivation will lead to accelaration in meters per'
      print *,'  squaresecond.'
      print *,'    If you give s(t) as the time dependence hfkt the'
      print *,'  seismogram units will be m*s (once integrated),'
      print *,'  m (unchanged source) or m/s (once derived).'
      print *,'    The terms "derivation" and "integration" apply to'
      print *,'  your selection in the source configuration file.'
      print *,' '
      print *,'Information on coordinate system:'
      print *,'  The positive z-axis points downwards into the halfspace.'
      print *,'  The r-axis points away from the epicenter. The transverse'
      print *,'  component points to the right when looking in positive'
      print *,'  r-direction. Azimuthal angles are counted clockwise'
      print *,'  positive, when looking in positive z-direction. The'
      print *,'  r-direction falls onto the x-direction for azimuthal'
      print *,'  angle zero and falls onto the y-direction for azimuthal'
      print *,'  angle 90degrees.'
      print *,' '
      print *,'  All vector components point into the direction of the'
      print *,'  corresponding coordinate axis:'
      print *,'    Positive forces point downward into the halfspace.'
      print *,'    Z-component seismograms are positive for downward'
      print *,'    movement. R-component seismograms are positive for'
      print *,'    outward movement. Transverse component seismograms'
      print *,'    are positive for rightward movements when looking'
      print *,'    outwards.'
      print *,' '
      print *,'Information on velocity dispersion:'
      print *,'  This program version is capable of velocity dispersion.'
      print *,'  This means that the complex layer velocities depend on'
      print *,'  frequency relative to a model given reference frequency.'
      print *,'  As one of the main advantages of the reflectivity method is'
      print *,'  that the interface coefficients are frequency independent'
      print *,'  and have to be calculated only once for each slowness.'
      print *,'  You will loose this advantage when using velocity'
      print *,'  dispersion.'
      print *,'  Whether dispersion is calculated or not depends on your'
      print *,'  earth model. If there is a positive reference frequency set'
      print *,'  velocity dispersion will be used.'
      return
      end
