c this is <gr_rtkc.f>
c------------------------------------------------------------------------------
c $Id$
c
c This is the Reflection- Transmission-Coefficient Subroutine
c written by Joachim Ungerer to be used in his reflectivity program refseis.
c
c This is part of a library version of the original code 'refseis.f'
c
c Copyright 1990 by Joachim Ungerer
c
c refseis.f is published as part of the diploma thesis of Joachim Ungerer
c without a specific license
c
c----------------------------------------------------------------------
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c    24/07/97   V1.0   Thomas Forbriger
c    29/07/97   V1.1   just calculating PSV-case
c    26/03/02   V1.2   prepare code to be used with complex frequencies
c
c==============================================================================
c
C23456789012345678901234567890123456789012345678901234567890123456789012
C******* Joachim Ungerer ********** Stuttgart *****17.3.90*************C
C                                                                      C
C Subroutine RTKC                                                      C
C                                                                      C
C Berechnet die Reflektions- und Transmissionskoeffizienten einer      C
C ebenen Trennflaeche zwischen zwei homogenen Halbraeumen; fuer        C
C komplexe Wellengeschwindigkeiten.                                    C
C                                                                      C
C Berechnung mit doppelter Genauigkeit:                                C
C   DOUBLE COMPLEX muss implementiert sein                             C
C                                                                      C
C Variablendokumentation:                                              C
C                                                                      C
C         Uebergabevariablen:                                          C
C         u       horizontale Langsamkeit                              C
C         a1,b1   vert.Langsamkeiten des oberen Halbraumes             C
C         betaC1  S-Wellengeschw. des      "        "     (komplex)    C
C         rho1    Dichte des               "        "                  C
C         a2,b2   vert.Langsamkeiten des unteren Halbraumes            C
C         betaC2  S-Wellengeschw. des      "        "     (komplex)    C
C         rho2    Dichte des               "        "                  C
C                                                                      C
C         Rueckgabevariablen:                                          C
C         Rppd,Rpsd,Tppd,Tpsd    fuer  P-Welle von oben                C 
C         Rssd,Rspd,Tssd,Tspd    fuer SV-Welle      "                  C
C         rd,td                  fuer SH-Welle      "                  C
C         Rppu,Rpsu,Tppu,Tpsu    fuer  P-Welle von unten               C
C         Rssu,Rspu,Tssu,Tspu    fuer SV-Welle      "                  C
C         ru,tu                  fuer SH-Welle      "                  C
C                                                                      C
C         Rechenvariablen:                                             C
C          ........        gehen aus der Deklaration und               C
C                          ihrer Berechnung hervor                     C
C                                                                      C
C**********************************************************************C


      SUBROUTINE gr_rtkc(u,a1,b1,betaC1,rho1,a2,b2,betaC2,rho2,
     &                Rppd,Rpsd,Tppd,Tpsd,Rssd,Rspd,Tssd,Tspd,
     &                Rppu,Rpsu,Tppu,Tpsu,Rssu,Rspu,Tssu,Tspu)
c 
c original (including SH)
c      SUBROUTINE RTKC(u,a1,b1,betaC1,rho1,a2,b2,betaC2,rho2,
c     &                Rppd,Rpsd,Tppd,Tpsd,Rssd,Rspd,Tssd,Tspd,rd,td,
c     &                Rppu,Rpsu,Tppu,Tpsu,Rssu,Rspu,Tssu,Tspu,ru,tu)


C Deklaration der Variablen ********************************************

C     Variablen fuer beide Faelle
      COMPLEX*16  u,uQ
      REAL*8      rho1,rho2,rho12

      COMPLEX*16  Rppd,Rpsd,Tppd,Tpsd,Rssd,Rspd,Tssd,Tspd,
     &            Rppu,Rpsu,Tppu,Tpsu,Rssu,Rspu,Tssu,Tspu,
     &            betaC1,betaC2,a1,a2,b1,b2,a1b1,a2b2,a1b2,a2b1,ab,
     &            abm,rhoabm,C,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,
     &            mue1,mue2
c      COMPLEX*16  Rppd,Rpsd,Tppd,Tpsd,Rssd,Rspd,Tssd,Tspd,rd,td,
c     &            Rppu,Rpsu,Tppu,Tpsu,Rssu,Rspu,Tssu,Tspu,ru,tu,
c     &            betaC1,betaC2,a1,a2,b1,b2,a1b1,a2b2,a1b2,a2b1,ab,
c     &            abm,rhoabm,C,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,
c     &            mue1,mue2,mue1b1,mue2b2,muebN,muebP

C     nur fuer Einfall von oben
      COMPLEX*16  D1D,D2D,DD,D1,D2,D3

C     nur fuer Einfall von unten
      COMPLEX*16  D1U,D2U,DU,U1,U2,U3          



C Berechnungen ********************************************************C

C Berechnungen der Rechenvariablen fuer beide Faelle
      uQ    = u*u
      mue1  = rho1*betaC1*betaC1
      mue2  = rho2*betaC2*betaC2
c      mue1b1= mue1*b1
c      mue2b2= mue2*b2
c      muebN = mue1b1-mue2b2
c      muebP = mue1b1+mue2b2
      rho12 = rho1*rho2
      a1b1  = a1*b1
      a2b2  = a2*b2
      a1b2  = a1*b2
      a2b1  = a2*b1
      ab    = a1b1*a2b2
      abm   = a1b2-a2b1
      rhoabm= 2.D0*rho12*abm
      C     = 2.D0*(mue1-mue2)
      C0    = C*uQ
      C1    = C0-rho1
      C2    = C0+rho2
      C3    = C1+rho2
      C4    = C2*a1-C1*a2
      C5    = C2*b1-C1*b2
      C6    = C3*C3*uQ
      C7    = C*C0*ab
      C8    = C1*C1*a2b2+rho12*a2b1
      C9    = C2*C2*a1b1+rho12*a1b2
      C10   = C3+C*a2b1
      C11   = C3+C*a1b2

C Koeffizienten fuer Einfall von oben
C     Rechenvariablen
      D1D   = C6+C8
      D2D   = C7+C9
      DD    = D1D+D2D
      D1    = 2.D0*a1/DD
      D2    = 2.D0*b1/DD
      D3    = C3*C2+C*C1*a2b2
C     Koeffizienten
      Rppd  = (D2D-D1D)/DD
      Rpsd  =-u*D1*D3
      Tppd  = rho1*D1*C5
      Tpsd  =-u*rho1*D1*C10
      Rssd  = Rppd-rhoabm/DD
      Rspd  = u*D2*D3
      Tssd  = rho1*D2*C4
      Tspd  = u*rho1*D2*C11
c      rd    = muebN/muebP
c      td    = rd + 1.D0

C Koeffizienten fuer Einfall von unten     
C     Rechenvariablen
      D1U   = C6+C9
      D2U   = C7+C8
      DU    = D1U+D2U
      U1    = 2.D0*a2/DU
      U2    = 2.D0*b2/DU
      U3    = C3*C1+C*C2*a1b1
C     Koeffizienten
      Rppu  = (D2U-D1U)/DU
      Rpsu  = u*U1*U3
      Tppu  = rho2*U1*C5
      Tpsu  =-u*rho2*U1*C11
      Rssu  = Rppu+rhoabm/DU
      Rspu  =-u*U2*U3
      Tssu  = rho2*U2*C4
      Tspu  = u*rho2*U2*C10
c      ru    =-muebN/muebP
c      tu    = ru + 1.D0

      RETURN
      END

C***Ende von RTKC *****************************************************C
c
c ----- END OF gr_rtkc.f -----
