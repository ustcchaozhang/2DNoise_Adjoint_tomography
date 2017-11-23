c this is <queko.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1999 by Joerg Dalkolmo (IfG Stuttgart)
c
c Calculate relative coordinates from absolute coordinates on a sphere
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
c    02/02/99   V1.0   Joerg Dalkolmo
c
c==============================================================================
c
      program queko
c
c Berechnet Quellkoordinaten (Epizentraldistanz, Azimut)
c aus geographischen Koordinaten von Quelle und Empfaenger
c
      real slong,slat,rlat,rlong,delta,phi,bazi

c
      print *, 'Eingabe Breite und Laenge der Quelle:'
      read(*,*) slat,slong
      print *, 'Eingabe Breite und Laenge des Empfangers:'
      read(*,*) rlat,rlong
c
c
      call epitra(slat,slong,rlat,rlong,delta,phi,bazi)
c
      print *, 'Epizentraldistanz:',delta
      print *, 'Azimut           :',phi
      print *, 'BackAzimut       :',bazi
c
      stop 
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
c ----- END OF queko.f -----
