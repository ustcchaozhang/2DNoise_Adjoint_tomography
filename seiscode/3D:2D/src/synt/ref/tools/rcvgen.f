c this is <rcvgen.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 1995 by Jörg Dalkolmo
c Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
c
c create a receiver file for given earthquake and station coordinates
c
c Some parts of the source code are copied from gemini by Jörg Dalkolmo.
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
c    16/09/2003   V1.0   Thomas Forbriger
c    01/12/2016   V1.1   increase precision of output values
c
c ============================================================================
c
      program rcvgen
c
      character*(*) version
      parameter(version=
     &  'RCVGEN   V1.1   create rcv file for given '//
     &  'earthquake and station coordinates')
c
      real slong,slat,rlat,rlong,delta,phi,bazi
      character*80 stationfile, rcvfile
      character*79 comment
      character*5 code
      integer lu, nr, i
      parameter(lu=10)
c
c values to remember
      integer maxrcv, nrcv
      parameter(maxrcv=50)
      real outdelta(maxrcv), outazi(maxrcv)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v,2h-c/
      data opthasarg/2*.FALSE.,.TRUE./
      data optarg/3*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.4)) then
        print *,version
        print *,'Usage: rcvgen stationfile rcvfile slat slon'
        print *,'              [-c comment]'
        print *,'   or: rcvgen -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'stationfile  name of input file'
        print *,'             station coordinates in epi-format'
        print *,'rcvfile      name of output file'
        print *,'             refmet control file to be created'
        print *,'slat         source latitude'
        print *,'slon         source latitude'
        print *,' '
        print *,'Format of station file:'
        print *,'One receiver per line; each line has four columns;'
        print *,'columns are read in Fortran free format (i.e. are'
        print *,'separated by spaces or commas'
        print *,' '
        print *,'1st column: number of station (ignored)'
        print *,'2nd column: name of station (ignored)'
        print *,'3rd column: latitude of station'
        print *,'4th column: longitude of station'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(5, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      comment=optarg(3)
c
c------------------------------------------------------------------------------
c go
      call getarg(3, stationfile)
      read(stationfile, *) slat
      call getarg(4, stationfile)
      read(stationfile, *) slong
      call getarg(1, stationfile)
      call getarg(2, rcvfile)
c 
      open(unit=lu, file=stationfile, err=99)
c
      nrcv=0
    1 read(lu, *, err=98, end=97) nr, code, rlat, rlong
      if (nr.gt.998) goto 2
      print *, 'Breite und Laenge des Empfangers:'
      print *, code,': ', rlat,rlong
c
c
      call epitra(slat,slong,rlat,rlong,delta,phi,bazi)
c
      print *, 'Epizentraldistanz:',delta
      print *, 'Azimut           :',phi
      print *, 'BackAzimut       :',bazi
      nrcv=nrcv+1
      outdelta(nrcv)=delta
      outazi(nrcv)=phi
      goto 1
    2 close(lu, err=96)
c 
c----------------------------------------------------------------------
c Output
      open(unit=lu, file=rcvfile, err=95)
      write(lu, '(a)', err=94) comment
      write(lu, '(/5a10)',err=94) 'mode','Vred','Tli','Tre','NE'
      write(lu, '(i10,3f10.2,i10)',err=94) 4, 22., 0., 30., nrcv
      write(lu, '(/2a10)',err=94) 'delta','phi'
      write(lu, '(f15.8,1x,f10.3)',err=94) 
     &  (outdelta(i), outazi(i), i=1,nrcv)
      close(lu, err=93)
c
      stop
   99 stop 'ERROR: opening station coordinates'
   98 stop 'ERROR: reading station coordinates'
   97 stop 'ERROR: reading station coordinates - unexpected end'
   96 stop 'ERROR: closing station coordinates'
   95 stop 'ERROR: opening output'
   94 stop 'ERROR: writing output'
   93 stop 'ERROR: closing output'
      end

c----------------------------------------------------------------------

      subroutine epitra(SourceLat,SourceLong,RecLat,RecLong,delta,azi,bazi)
c.....                                                        ..........
c                                                                      .
c *** Transforms from spherical to source coordinates; angles          .
c      are given in degrees.                                           .
c      tets ...      latitude (theta) of the source                    .
c      phis ...      longitude (phi) of the source                     .
c      tete ...      latitude of the receiver                          .
c      phie ...      longitude of the receiver                         .
c      delta ...      epicentral distance                              .
c      azi ...      azimuth, measured clockwise from north             .
c      bazi ...     back-azimuth, measured clockwise from north        .
c                                                                      .
c.......................................................................
c
      real delta,azi,SourceLat,RecLat,SourceLong,RecLong,bazi
      double precision pi,tete,phie,tets,phis,ctete,ctets,stete,stets,cdel,
     &      sdelta,cal,sal,gamma,d2r,r2d
     &      ,cbe,sbe
      parameter(pi=3.14159265359d0,d2r=Pi/180.d0,r2d=1.d0/d2r)


c-----------------------------------------------------------------
c  Transform from latitude to colatitude                          |
c Transform from degrees to radians and abbreviate sine and       |
c cosine.                                                         |
c-----------------------------------------------------------------

      tets = 90.d0 - dble(SourceLat)
      tete = 90.d0 - dble(RecLat)
      if (tets.eq.0.d0) then 
         delta = sngl(tete)
         azi   = 180.-Reclong
         bazi = 0.
         return
      else if (tets.eq.180.d0 ) then
         delta = 180. - sngl(tete)
         azi   = RecLong
         bazi  = 180.
         return
      else if (tete.eq.0.d0 ) then
         delta = sngl(tets)
         azi = 0.
         bazi = 180.
         return
      endif
      tets = tets*d2r
      phis = dble(SourceLong)*d2r
      tete = tete*d2r
      phie = dble(RecLong)*d2r
      ctets = dcos(tets)
      ctete = dcos(tete)
      stets = dsin(tets)
      stete = dsin(tete)

c-----------------------------------------------------------------
c Use cosine theorem on the sphere and check for antipode.        |
c-----------------------------------------------------------------

      cdel  = ctets*ctete+stets*stete*dcos(phie-phis)
      delta = sngl(dacos(cdel)*r2d)
      sdelta=dsqrt((1.-cdel)*(1.+cdel))
      if(sdelta.eq.0.d0)then
           azi=0.
           bazi=0.
           return
      endif

c-----------------------------------------------------------------
c Use cosine and Sine theorem to get azimut and back-azimut.      |
c-----------------------------------------------------------------

      cal=(ctete-ctets*cdel)/(stets*sdelta)
      sal=stete*dsin(phie-phis)/sdelta
      if(cal.gt.1.d0) cal=1.d0
      if(cal.lt.-1.d0) cal=-1.d0
      gamma=dacos(cal)
      if(sal.ge.0.d0) then
        azi=sngl(gamma)
      else
        azi=sngl(2.d0*pi-gamma)
      endif
      azi = azi*sngl(r2d)
      cbe=(ctets-ctete*cdel)/(stete*sdelta)
      sbe=stets*dsin(phie-phis)/sdelta
      if(cbe.gt.1.d0) cbe=1.d0
      if(cbe.lt.-1.d0) cbe=-1.d0
      gamma=dacos(cbe)
      if(sbe.ge.0.d0) then
        bazi=sngl(2.d0*pi-gamma)
      else
        bazi=sngl(gamma)
      endif
      bazi = bazi*sngl(r2d)


      end
c
c ----- END OF rcvgen.f ----- 
