c this is <bfotide.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1959 by Jon Berger, Russ Evans, and Dan McKenzie
c Copyright (c) 1974 by Walter Zuern
c Copyright (c) 1978 by David Young
c
c Earth response to tidal forces based on Longmans code
c
c This program calculates the response of an elastic earth to tidal
c forces. The Earth's elasticity is defined by Love-numbers in
c subroutine TIDEPT. The program computes acceleration, strain and tilt.
c The calculation is based on ephemerides based on Longman's theory.
c
c See:
c I.M. Longman, 1959. Formulas for Computing the Tidal Accelerations due
c to the Moon and the Sun. Journal of Geophysical Research, Vol. 64, No.
c 12, 2351 - 2355.
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
c    07/01/2014   V1.0   Thomas Forbriger: incorporated in TFSoftware
c
c ============================================================================
c
      program bfotide
c  this program uses TIDEPT to compute three components of the earth
c  tide signal on an elastic earth.
c  besides vital variables,format statements 108,109 should be modified
c  first differences are formed
      double precision ts,dt,t,tim69
      integer opt
      character*80 fname
      print*,'Enter output file name: '
      read(*,'(a)')fname
      open(3,file=fname)
c
      print*,' '
      print*,'Enter latitude and longitude of your station in '
      print*,'  decimal degrees: '
      read(*,*) glat, glong
c
      print*,' '
      print*,'Chose between - acceleration (e.g. gravity) (opt=1)'
      print*,'              - tilt (opt=2)'
      print*,'              - strain (opt=3)'
      print*,'Enter opt: '
      read(*,*)opt

      print*,' '
      print*,'Azimuth convention for sensors: '
      print*,'  for acceleration: east from north '
      print*,'  for strainmeter: east from north '
      print*,'          i.e. 0.0 deg is north strain'
      print*,'              45.0 deg is north-east strain'
      print*,'              90.0 deg is east strain'
      print*,'  for tiltmeter  : north from east, '
      print*,'          i.e. 0.0 deg is east tilt'
      print*,'              90.0 deg is north tilt'
      print*,' '
      print*,' Enter time step in seconds: '
      read(*,*)dt
c  timestep in hours
      dt=dt/3600.d0
c
      print*,' '
      print*,' Enter start time of synthetic tidal record. '
      print*,'   (for march 1. 1995 enter: 1995 3 1): '
      read(*,*)iyr,imonth,iday
c  elapse time of first point in hours since jan 01,1969,0.0 hrs GMT
      ts= TIM69(iyr,imonth,iday)
c
      print*,' '
      print*,' Set scale for output. '
      print*,'  if scale=1.e+6 then acceleration in microgals '
      print*,'                      tilt         in microrads '
      print*,'                      strain       in microstrain '
      print*,'  for nanogals and nanostrains use scale=1.e+9 '
      print*,'  for tilt in msecs arc use  scale=0.206264806e+09 '
      print*,' Enter scale: '
      read(*,*)scale
c     
      print*,' '
      print*,' Enter length of time series (hours): '
      read(*,*)xlen
      ndata = xlen/dt
      print*,' You have asked for ',ndata,' samples. '
      t=ts
      print*,' '
      print*,' tides on an elastic earth model (earthtd)'
      print*,' '
      print*,'    north latitude =',glat
      print*,'    east longitude =',glong
      if (opt.eq.1) then
         print*,' Computing acceleration components: Z, N, E '
         print*,' gravity (gals*scale**(-1)) '
      elseif (opt.eq.2) then
         print*,' Computing tilt components: N, E '
         print*,' tilt (rads*scale**(-1)) '
      elseif (opt.eq.3) then
         print*,' Computing strain components: N, N-E, E '
         print*,' strain (strain*scale**(-1)) '
      endif
      print*,' scale= ',scale

      print*,' start of series at: ',ts,
     *       ' hours from JAN 01, 1969 0.0 hrs GMT'
      print*,' time step= ',dt,' hours. '
      print*,' '
      print*,' 1. point = 0000h GMT on',iyr,imonth,iday
c
c if tilt is also computed with 1.000e+06 for scale, then result is also
c in microgals
      lorder=0
      do  1  i=1,ndata
         if (opt.eq.1) then
c acceleration
            call tidept(t,xz,glat,glong,0.0,scale,lorder,1)
            call tidept(t,xn,glat,glong,90.0,scale,lorder,2)
            call tidept(t,xe,glat,glong,0.0,scale,lorder,2)
            xn = 981.0*xn
            xe = 981.0*xe
            write(3,'(f15.3,3f10.2)') t-ts,xz,xn,xe
         elseif(opt.eq.2) then
c tilt
            call tidept(t,xn,glat,glong,90.0,scale,lorder,2)
            call tidept(t,xe,glat,glong,0.0,scale,lorder,2)
            write(3,'(f15.3,2f10.2)') t-ts,xn,xe
         elseif(opt.eq.3) then
c strain
            call tidept(t,xn,glat,glong,90.0,scale,lorder,2)
            call tidept(t,xne,glat,glong,45.0,scale,lorder,2)
            call tidept(t,xe,glat,glong,0.0,scale,lorder,2)
            write(3,'(f15.3,3f10.2)') t-ts,xn,xne,xe
         endif

         t=t+dt
   1  continue
      close(3)
      stop
      end

      SUBROUTINE TIDEPT (TIME,RESULT,GLAT,GLONG,ORIENT,SCALE,
     * LORDER,OPT)
C    BERGER,EVANS,MCKENZIE
C MODIFIED BY YOUNG 16.1.78 TO RETURN STRAIN VALUE AT ONE TIME
C ONLY.  SEE SUBROUTINE EARTHTD FOR DETAILS OF VARIABLES.
C     STRAINMETER AZIMUTH IS POSITIVE EAST FROM NORTH
      REAL LAMDA,   LL,         H,K,L,KR,MU,MUT,MUL,MUTT,MULL,MUTL,ELL,
     1  ETT,ETL
      DOUBLE PRECISION  SAW,CAW,RBARR
      DOUBLE PRECISION CZ,SZ,Z,X,THFPSI,CNM,SNM,CW,SW,CNU,SNU,NU
      DOUBLE PRECISION TIME,TT,PSIG,PSIM,HR,AW
      DOUBLE PRECISION  PI20,HALFPI,TWOPI,AZT,HS,T2,PS,T,PSIS,HM,PM,NM,
     1  ES,SIG,LS,LM,TANP
      INTEGER OPT
      DIMENSION  H(6),K(6),L(6),KR(7),P(7),PP(7),PPP(7)
      H(1)=0.612
      H(2)=0.290
      H(3)=0.175
      H(4)=0.129
      H(5)=0.107
      H(6)=0.095
      K(1)=0.302
      K(2)=0.093
      K(3)=0.042
      K(4)=0.025
      K(5)=0.017
      K(6)=0.013
      L(1)=0.083
      L(2)=0.014
      L(3)=0.010
      L(4)=0.008
      L(5)=0.007
      L(6)=0.005
      JS=2
      JE=3
      IF(LORDER.EQ.0)  GO TO5
      JE=LORDER
      JS=JE
  5   CONTINUE
      GTSUN=0.
      TTSUN=0.
      STNSUN=0.
      LAMDA=GLONG
      THETA=90.0-GLAT
      AZS=-ORIENT
      AZT=-ORIENT
      PI20=62.83185307169D0
      HALFPI=1.570796326793D0
      TWOPI=6.283185307169D0
      RE=6.37122E+8
      G=982.02
      AZT=AZT*0.0174532925199D0
      AZS=AZS*0.0174532925199D0
      CAZT=COS(AZT)
      SAZT=SIN(AZT)
      CAZS=COS(AZS)
      SAZS=SIN(AZS)
      TT=TIME+604848.0D0
      THETA=THETA*0.0174532925199D0
      LAMDA=LAMDA*0.0174532925199D0
      CTHETA=COS(THETA)
      STHETA=SIN(THETA)
      HR=DMOD(TIME,24.0D0)
      T=(TT+12.0D0)/876600.0D0
      T2=T*T
      HS=4.881627982482D0+628.3319508731D0*T
      HS=HS+0.523598775578D-5*T2
      HS=DMOD(DMOD(HS,PI20)+PI20,PI20)
      PS=4.908229466993D0+T*0.0300052641669D0
      PS=PS+0.790246300201D-5*T2
      ES=0.01675104D0- 0.00004180D0*T-0.000000126D0*T2
      PSIG=0.2617993877971D0*(HR-12.0D0)+HS
      IF(LORDER.NE.0.AND.LORDER.NE.2)   GO TO 82
      LS=HS+2.0D0*ES*DSIN(HS-PS)+1.25D0*ES**2*DSIN(2.0D0*(HS-PS))
      CZ=0.397980654630D0*DSIN(LS)
      SZ=DSQRT(1.0D0-CZ**2)
      Z=DATAN2(SZ,CZ)
      X=0.5D0*(LS+Z-HALFPI)
      THFPSI=1.52386101015D0*(DSIN(X)/DCOS(X))
      PSIS=2.0D0*DATAN(THFPSI)
      PSIS=DMOD(PSIS+TWOPI,TWOPI)
      RBARR=1.D0+ES*DCOS(HS-PS)+ES**2*DCOS(2.D0*(HS-PS))
      LL=PSIS-PSIG
      SLL=SIN(LAMDA-LL)
      CLL=COS(LAMDA-LL)
      CALFA=CTHETA*CZ+STHETA*SZ*CLL
      XI=4.2635E-5*RBARR
      CC=2.120E14*XI
      KR(2)=CC*XI*XI
      KR(3)=KR(2)*XI
      KR(4)=KR(3)*XI
      MU=CALFA
      P(2)=0.5*(3.*MU*MU-1.)
      P(3)=0.5*MU*(5.*MU*MU-3.)
      P(4)=0.25*(7.*MU*P(3)-3.*P(2))
      PP(2)=3.*MU
      PP(3)=1.5*(5.*MU*MU-1.)
      PP(4)=0.25*(7.*(P(3)+MU*PP(3))-3.*PP(2))
      PPP(2)=3.
      PPP(3)=15.*MU
      PPP(4)=7.5*(7.*MU*MU-1.)
      MUT=-STHETA*CZ+CTHETA*SZ*CLL
      MUTT=-MU
      MUL=-STHETA*SZ*SLL
      MULL=-STHETA*SZ*CLL
      MUTL=-CTHETA*SZ*SLL
      J=2
      GO TO(50,60,70),OPT
 50   GTSUN=0.
      DEL=1.+2.*H(J-1)/J-(J+1)*K(J-1)/J
      GTSUN=GTSUN+DEL*J*KR(J)*P(J)*G/RE
      GO TO 80
 60   TTSUN=0.
      DIM=1.+K(J-1)-H(J-1)
      TTSUN=TTSUN+(DIM*KR(J)*PP(J)/RE)*(MUT*SAZT+MUL*CAZT/STHETA)
      GO TO 80
 70    ELL=0.
      ETL=0.
      ETT=0.
      M=J-1
      ELL=ELL+KR(J)*(L(M)*(PP(J)*MUT*CTHETA/STHETA+(PPP(J)*
     1MUL*MUL+PP(J)*MULL)/(STHETA*STHETA))+H(M)*P(J))
      ETT=ETT+KR(J)*(L(M)*(PPP(J)*MUT*MUT+PP(J)*MUTT)+
     1  H(M)*P(J))
      ETL=ETL+2.*(KR(J)*L(M)*(PPP(J)*MUL*MUT+PP(J)*MUTL-
     1  CTHETA*PP(J)*MUL/STHETA))/STHETA
      STNSUN=(ELL*SAZS*SAZS+ETL*SAZS*CAZS+ETT*CAZS*CAZS)/RE
 80   CONTINUE
 82   HM=4.720008893870D0+8399.709274530D0*T
      HM=HM+0.345575191895D-4*T2
      HM=DMOD(DMOD(HM,PI20)+PI20,PI20)
      PM=5.835152597865D0+71.01804120839D0*T
      PM=PM-0.180108282526D-3*T2
      NM=4.523601611611D0-33.75714629423D0*T
      NM=NM+0.362640633469D-4*T2
      CNM=DCOS(NM)
      SNM=DSIN(NM)
      CW=0.91369D0-0.03569D0*CNM
      SW=DSQRT(1.0D0-CW**2)
      SNU=0.08968D0*SNM/SW
      CNU=DSQRT(1.0D0-SNU**2)
      NU=DATAN(SNU/CNU)
      SAW=0.39798D0*SNM/SW
      CAW=CNM*CNU+0.91739D0*SNM*SNU
      AW=2.0D0*DATAN(SAW/(1.0D0+CAW))
      AW=DMOD(AW+TWOPI,TWOPI)
      SIG=HM-NM+AW
      LM=SIG+0.109801D0*DSIN(HM-PM)+0.003768D0*DSIN(2.0D0*(HM-PM))
     1 -0.224412D0*ES*DSIN(HS-PS)+0.02045D0*DSIN(HM-2.0D0*HS+PM)
     2 +0.010809D0*DSIN(2.0D0*(HM-HS))+0.000653D0*DSIN(3.0D0*HM-2.0D0*HS
     3  -PM) +0.000451D0*DSIN(2.0D0*HM-3.0D0*HS+PS)
      CZ=DSIN(LM)*SW
      WM=DATAN2(SW,CW)
      SZ=DSQRT(1.0D0-CZ**2)
      ZM=DATAN2(SZ,CZ)
      X=0.5D0*(LM+ZM-HALFPI)
      TANP=(DSIN(X)/DCOS(X))*DSIN(0.5D0*(HALFPI+WM))/DSIN(0.5D0*(HALFPI-
     1  WM))
      PSIM=2.0D0*DATAN(TANP)
      PSIM=DMOD(PSIM+TWOPI,TWOPI)
      RBARR=1.0D0+0.05467D0*DCOS(HM-PM)+0.003011D0*DCOS(2.0D0*(HM-PM))
     1 -0.000141D0*DCOS(HS-PS)+0.009271D0*DCOS(HM-2.0D0*HS+PM)
     2+0.007759D0*DCOS(2.0D0*(HM-HS))+0.000633D0*DCOS(3.D0*HM-2.D0*HS-PM
     3) +0.000328D0*DCOS(2.D0*HM-3.D0*HS+PS)
      LL=NU+PSIM-PSIG
      SLL=SIN(LAMDA-LL)
      CLL=COS(LAMDA-LL)
      CALFA=CTHETA*CZ+STHETA*SZ*CLL
      XI=1.65933E-2*RBARR
      CC=7.834E6*XI
      KR(2)=CC*XI*XI
      J=3
      KR(J)=KR(J-1)*XI
      MU=CALFA
      P(1)=MU
      P(2)=0.5*(3.*MU*MU-1.)
      J=2
      P(J+1)=((2*J+1)*MU*P(J)-J*P(J-1))/(J+1)
      PP(1)=1.
      PP(2)=3*MU
      PPP(1)=0.
      PPP(2)=3.
      J=2
      PP(J+1)=((2*J+1)*(P(J)+MU*PP(J))-J*PP(J-1))/(J+1)
      PPP(J+1)=((2*J+1)*(2*PP(J)+MU*PPP(J))-J*PPP(J-1))/(J+1)
      MUT=-STHETA*CZ+CTHETA*SZ*CLL
      MUTT=-MU
      MUL=-STHETA*SZ*SLL
      MULL=-STHETA*SZ*CLL
      MUTL=-CTHETA*SZ*SLL
      GO TO (100,110,120),OPT
 100   GTMUN=0.
      DO  105  J=JS,JE
      DEL=1.+2*H(J-1)/J-(J+1)*K(J-1)/J
 105  GTMUN=GTMUN+DEL*J*KR(J)*P(J)*G/RE
      GO TO 130
 110   TTMUN=0.
      DO  115  J=JS,JE
      DIM=1.+K(J-1)-H(J-1)
 115  TTMUN=TTMUN+(DIM*KR(J)*PP(J)/RE)*(MUT*SAZT+MUL*CAZT/STHETA)
      GO TO 130
 120  ELL=0.
      ETL=0.
      ETT=0.
      DO 125 J=JS,JE
      M=J-1
      ELL=ELL+KR(J)*(L(M)*(PP(J)*MUT*CTHETA/STHETA+(PPP(J)*
     1  MUL*MUL+PP(J)*MULL)/(STHETA*STHETA))+H(M)*P(J))
      ETT=ETT+KR(J)*(L(M)*(PPP(J)*MUT*MUT+PP(J)*MUTT)+H(M)*P(J))
      ETL=ETL+2*(KR(J)*L(M)*(PPP(J)*MUL*MUT+PP(J)*MUTL-
     1 CTHETA*PP(J)*MUL/STHETA))/STHETA
 125  STNMUN=(ELL*SAZS*SAZS+ETL*SAZS*CAZS+ETT*CAZS*CAZS)/RE
 130  CONTINUE
      GO TO (140,150,160),OPT
140   RESULT=(GTSUN+GTMUN)*SCALE
      GO TO 170
150   RESULT=(TTSUN+TTMUN)*SCALE
      GO TO 170
160   RESULT=(STNSUN+STNMUN)*SCALE
170   CONTINUE
 180  CONTINUE
      RETURN
      END

        Double Precision Function TIM69(myr,mon,mday)
c  function to return the number of days from the beginning of 1969
c  to a given later date.
c  intended to be used to find the input variable for the program TIDEPT
c  and its relatives,which need time in hours from January 1,1969 0.0 hours
c  G.M.T.
c  this version goes wrong after 2100 and before 1800
c
      double precision st69,hours
      dimension iam(12)
      data iam/0,31,59,90,120,151,181,212,243,273,304,334/
      lday=mday-1
      kyr=myr-1969
      lyr=kyr
      if(lyr.lt.0)  lyr=lyr-3
      st69=kyr*365+iam(mon)+lday+lyr/4
      if(myr.eq.myr/4*4.and.mon.gt.2) st69=st69+1.0d0
      if(myr.le.1900.and.mon.le.2)  st69=st69+1.0d0
      hours = st69*24.00d0
      tim69=hours
      return
      end
c
c ----- END OF bfotide.f ----- 
