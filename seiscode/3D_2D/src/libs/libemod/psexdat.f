c this is <psexdat.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 1996 by Thomas Forbriger (IfG Stuttgart)
c Copyright (c) 1995 by Gerhard Mueller (IMG Frankfurt)
c
c some soubroutines to read and write psexpl.dat files
c psexpl.dat files are used by reflectivity programs developed by
c Gerhard Mueller. Part of the code is copied from psexpl
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
c    17/04/1996   V1.0   Thomas Forbriger
c    23/02/2009   V1.1   commented some unused statements to satisfy gfortran
c
c ============================================================================
c
c  subroutine to read a psexpl parameter-file
c
      subroutine psex_readmod(filename, 
     &   maxlay, maxsou, maxr, nlay, d, iss, dentif,
     &   fr, gam, zz, aa, bb, rrho, qqa, qqb, 
     &   m ,nh, xs, ys ,zs, ts, es, nn, npts, dt, t,
     &   fu, fwil, fwir, fo, tsigma, cmin, cmi, ct,
     &   ctt, cma, cmax, nu, vred, tmin, mdeck, iq,
     &   nr, r, azi)
c  declare parameters
c  (see documentation of psexpl for declaration of names)
      integer maxlay, maxsou, nlay, maxr
      character*(*) filename
      integer iss(10)
      character*60 dentif
      real fr, gam
      real zz(maxlay), aa(maxlay), bb(maxlay), rrho(maxlay)
      real qqa(maxlay), qqb(maxlay), d(maxlay)
      integer m, nh
      real xs(maxsou), ys(maxsou), zs(maxsou), ts(maxsou), es(maxsou)
      integer nn, npts
      real dt, t, fu, fwil, fwir, fo, tsigma
      real cmin, cmi, ct, ctt, cma, cmax
      integer nu
      real vred, tmin
      integer mdeck
      integer iq
      integer nr
      real r(maxr), azi(maxr)
c  declare internal variables
      integer i
      logical debug
c  report
      debug=.FALSE.
c  read file
      if (debug) print *,'psex_readmod file: ',filename
      open(20, file=filename, err=99)
      read(20, 50, err=97, end=96) 
     &     (iss(i), i=1,10), dentif
   50 format(10i1,a10)
      call psex_ERDMOD(20,maxlay,maxsou,
     &     zZ,D,aA,bB,rRHO,qQA,qQB,nlay,M,NH,XS,YS,ZS,TS,ES,
     &     fr,GAM)
      read(20, 55, err=97, end=96)
     &     nn, npts, dt, t, fu, fwil, fwir, fo, tsigma
   55 format(2i5,7f10.4)
      read(20, 56, err=97, end=96)
     &     cmin,cmi,ct,ctt,cma,cmax,nu
   56 format(6f10.3,i10)
      read(20, 57, err=97, end=96)
     &     vred, tmin, mdeck
   57 format(2f10.3,i10)
      if (debug) print *,'*'
      read(20, 58, err=97, end=96) iq
   58 format(i10)
      call psex_readrec(20, maxr, nr, r, azi, iss)
      close(20, err=98)
      return
   99 stop 'ERROR: opening parameterfile'
   98 stop 'ERROR: closing parameterfile'
   97 stop 'ERROR: reading parameterfile'
   96 stop 'ERROR: reading parameterfile - unexpected end'
c   95 stop 'ERROR: too many layers'
c   94 stop 'ERROR: too many sources'
c   93 stop 'ERROR: writing report-file'
      end
c----------------------------------------------------------------------
c
c  subroutine to write a psexpl parameter-file
c
      subroutine psex_writemod(filename, 
     &   maxlay, maxsou, maxr, nlay, iss, dentif,
     &   fr, gam, zz, aa, bb, rrho, qqa, qqb,
     &   m ,nh, xs, ys ,zs, ts, es, nn, npts, dt, t,
     &   fu, fwil, fwir, fo, tsigma, cmin, cmi, ct,
     &   ctt, cma, cmax, nu, vred, tmin, mdeck, iq,
     &   nr, r, azi)
c  declare parameters
c  (see documentation of psexpl for declaration of names)
      integer maxlay, maxsou, nlay, maxr
      character*(*) filename
      integer iss(10)
      character*60 dentif
      real fr, gam
      real zz(maxlay), aa(maxlay), bb(maxlay), rrho(maxlay)
      real qqa(maxlay), qqb(maxlay)
      integer m, nh
      real xs(maxsou), ys(maxsou), zs(maxsou), ts(maxsou), es(maxsou)
      integer nn, npts
      real dt, t, fu, fwil, fwir, fo, tsigma
      real cmin, cmi, ct, ctt, cma, cmax
      integer nu
      real vred, tmin
      integer mdeck
      integer iq
      integer nr
      real r(maxr), azi(maxr)
c  declare internal variables
      integer i
      logical debug
c  report
      debug=.FALSE.
c  write file
      open(20, file=filename, err=99)
      write(20, '(10i1,a10)', err=97) 
     &     (iss(i), i=1,10), dentif
      write(20, '(2f10.3)', err=97) fr, gam
      write(20, '(f10.7,5f10.3,i5)', err=97)
     &     0.,aa(1),bb(1),rrho(1),qqa(1),qqb(1),0
      write(20, '(f10.7,5f10.3,i5,/,f10.7,5f10.3,i5)', err=97)
     &     (zz(i+1),aa(i),bb(i),rrho(i),qqa(i),qqb(i),1,
     &     zz(i+1),aa(i+1),bb(i+1),rrho(i+1),qqa(i+1),qqb(i+1),0,
     &     i=1,nlay-1)
      write(20, '(a10)', err=97) '                   '
      write(20, '(2i10)', err=97) m,nh
      write(20, '(4f10.4,g10.4)', err=97)
     &     (xs(i), ys(i), zs(i), ts(i), es(i), i=1, nh)
      if (debug) print *,(xs(i), ys(i), zs(i), ts(i), es(i), i=1, nh)
      write(20, '(2i5,7f10.4)', err=97)
     &     nn, npts, dt, t, fu, fwil, fwir, fo, tsigma
      write(20, '(6g10.3,i10))', err=97)
     &     cmin,cmi,ct,ctt,cma,cmax,nu
      write(20, '(2f10.3,i10)', err=97)
     &     vred, tmin, mdeck
      write(20, '(i10)', err=97) iq
      call psex_writerec(20, maxr, nr, r, azi, iss)
      close(20, err=98)
c report
      return
   99 stop 'ERROR: opening parameterfile'
   98 stop 'ERROR: closing parameterfile'
   97 stop 'ERROR: writing parameterfile'
c   95 stop 'ERROR: too many layers'
c   94 stop 'ERROR: too many sources'
c   93 stop 'ERROR: writing report-file'
      end
c----------------------------------------------------------------------
c 
c  erdmod (this the modified subroutine from psexpl that reads
c          the multi-layer-model and converts it to a normal layer model)
c
c     06/12/2007  zs was declared twice (once as scalar) ­ discarded second
c                 declaration
c
        SUBROUTINE psex_ERDMOD(funit,maxlay,maxsou,
     &     Z,D,A,B,RHO,QA,QB,NOL,M,NH,XS,YS,ZS,TS,ES,
     *     fr,GAM)
c 
c delclare parameters
c
        real z(maxlay),d(maxlay),a(maxlay),b(maxlay),rho(maxlay)
        real qa(maxlay), qb(maxlay)
        integer nol,m,nh,maxlay,maxsou
        real xs(maxsou), ys(maxsou), zs(maxsou), ts(maxsou)
        real es(maxsou)
        real fr, gam
        integer funit
c 
c declare variables
c
        integer imaxlay
        parameter(imaxlay=100)
        real zz(imaxlay), aa(imaxlay), bb(imaxlay)
        real rrho(imaxlay), qqa(imaxlay), qqb(imaxlay)
        integer nhs(imaxlay)
        real anhs, aii
        integer i, nzz,jj ,ii
        real dz,da,db,drho,dqa,dqb,zss
        real as,bs,rhos,qas,qbs
        logical debug
c 
C
C       read earth model in multi-layer format
C
        debug=.FALSE.
        READ(funit,8900,end=96,err=97)  FR,GAM
8900    FORMAT(2F10.3)
c8950    format(/'fr, gam: ',2f10.3)
        I=0
100     I=I+1
        if (i.gt.imaxlay) goto 98
        READ(funit,9000,end=96,err=97)  
     &    ZZ(I),AA(I),BB(I),RRHO(I),QQA(I),QQB(I),NHS(I)
9000    FORMAT(6F10.3,I5)
        IF(RRHO(I).EQ.0.)  GO TO 200
        IF(QQA(I).EQ.0.)  QQA(I)=10.**6
        IF(QQB(I).EQ.0.)  QQB(I)=10.**6
        IF(QQB(I).EQ.1.)  QQB(I)=4.*QQA(I)/9.
        GO TO 100
200     NZZ=I-1
        if (debug) write(*,9100)
        if (debug) write(*,9200) (i,zz(i),aa(i),bb(i),
     &             rrho(i),qqa(i),qqb(i),nhs(i), i=1,nzz)
9100    FORMAT(/' input model (I,ZZ,AA,BB,RRHO,QQA,QQB,NHS):')
9200    FORMAT(I10,6G10.3,I10)
C
C       source coordinates
C
C       ES=STAERKEFAKTOR (ES=1 ENTSPRICHT EINEM MOMENT VON
C       10**25 DYN CM)
C
        READ(funit,9300,end=96,err=97)  M,NH
        if (nh.gt.maxsou) goto 95
9300    FORMAT(2I10)
        READ(funit,9350,end=96,err=97)  
     &     (XS(I),YS(I),ZS(I),TS(I),ES(I),I=1,NH)
        if (debug) write(*,9400)
        if (debug) write(*,9450) (XS(I),YS(I),ZS(I),TS(I),ES(I),I=1,NH)
9350    FORMAT(5F10.4)
9400    FORMAT(/' source coordinates (I,XS,YS,ZS,TS,ES):')
9450    FORMAT(I10,5F10.4)
c9480    FORMAT(/' layer containing sources:',I5)
c9600    FORMAT(10F10.4)
C
C       convert multi-layer model to simple layer model
C
c400     JJ=0
        JJ=0
        DO  600  I=2,NZZ
        IF(NHS(I).EQ.0)  GO TO 600
        ANHS=FLOAT(NHS(I))
        DA=(AA(I)-AA(I-1))/ANHS
        DB=(BB(I)-BB(I-1))/ANHS
        DRHO=(RRHO(I)-RRHO(I-1))/ANHS
        DQA=(QQA(I)-QQA(I-1))/ANHS
        DQB=(QQB(I)-QQB(I-1))/ANHS
        DZ=(ZZ(I)-ZZ(I-1))/ANHS
        AS=AA(I-1)-DA/2.
        BS=BB(I-1)-DB/2.
        RHOS=RRHO(I-1)-DRHO/2.
        QAS=QQA(I-1)-DQA/2.
        QBS=QQB(I-1)-DQB/2.
        ZSS=ZZ(I-1)-DZ
        DO  500  II=1,NHS(I)
        JJ=JJ+1
        if (jj.gt.maxlay) goto 94
        AII=FLOAT(II)
        A(JJ)=AS+AII*DA
        B(JJ)=BS+AII*DB
        RHO(JJ)=RHOS+AII*DRHO
        QA(JJ)=QAS+AII*DQA
        QB(JJ)=QBS+AII*DQB
        Z(JJ)=ZSS+AII*DZ
500     D(JJ)=DZ
600     CONTINUE
        JJ=JJ+1
        if (jj.gt.maxlay) goto 94
        A(JJ)=AA(NZZ)
        B(JJ)=BB(NZZ)
        RHO(JJ)=RRHO(NZZ)
        QA(JJ)=QQA(NZZ)
        QB(JJ)=QQB(NZZ)
        Z(JJ)=ZZ(NZZ)
        D(JJ)=10.**6
        NOL=JJ
c9700    FORMAT(/' simple layer model (I,Z,D,A,B,RHO,QA,QB):')
c9800    FORMAT(I10,7F10.3)
c9801    format(/)
        RETURN
c   99   stop 'ERROR: writing report file'
   98   stop 'ERROR: too many multi-layers'
   97   stop 'ERROR: reading model file'
   96   stop 'ERROR: reading model file - unexpected end'
   95   stop 'ERROR: too many sources'
   94   stop 'ERROR: too many simple layers'
        END
c----------------------------------------------------------------------
c
c  readrec - read receiver information
c
      subroutine psex_readrec(funit, maxr, nr, r, az, iss)
c 
c declare parameters
c
      integer funit, maxr, nr, iss(10)
      real r(maxr), az(maxr)
c
c declare valriables
c
      real dr, rmin, rmax, azi
      integer i
c
        READ(funit,9501,err=97,end=96)  NR,RMIN,RMAX,AZI
        if (nr.gt.maxr) goto 98
9501    FORMAT(I10,3F10.4)
        IF(ISS(6).EQ.0)  GO TO 950
        DO 890  I=1,NR
        READ(funit,9520,err=97,end=96)  R(I),AZ(I)
9520    FORMAT(2F10.4)
890     CONTINUE
        GO TO 1050
950     DR=0.
        IF(NR.GT.1)  DR=(RMAX-RMIN)/FLOAT(NR-1)
        DO  1000  I=1,NR
        R(I)=RMIN+FLOAT(I-1)*DR
1000    AZ(I)=AZI
1050    continue
c9550    FORMAT(/' receiver coordinates (r,az):')
c9600    FORMAT(2F10.3)
c9801    format(/)
      return
c   99   stop 'ERROR: writing report file'
   98   stop 'ERROR: too many receivers'
   97   stop 'ERROR: reading model file'
   96   stop 'ERROR: reading model file - unexpected end'
      end
c----------------------------------------------------------------------
c
c  writerec - read receiver information
c
      subroutine psex_writerec(funit, maxr, nr, r, az, iss)
c 
c declare parameters
c
      integer funit, maxr, nr, iss(10)
      real r(maxr), az(maxr)
c
c declare valriables
c
      real rmin, rmax, azi
      integer i
c
        rmin=r(1)
        rmax=r(nr)
        azi=r(1)
        write(funit,9501,err=97)  NR,RMIN,RMAX,AZI
9501    FORMAT(I10,3F10.4)
        IF(ISS(6).EQ.0)  GO TO 1050
        DO 890  I=1,NR
        write(funit,9520,err=97)  R(I),AZ(I)
9520    FORMAT(2F10.4)
890     CONTINUE
1050    continue
c9550    FORMAT(/' receiver coordinates (r,az):')
c9600    FORMAT(2F10.3)
c9801    format(/)
      return
c   99   stop 'ERROR: writing report file'
   97   stop 'ERROR: writing model file'
      end
c
c ----- END OF psexdat.f ----- 
