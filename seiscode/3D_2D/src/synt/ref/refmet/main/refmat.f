c this is <refmat.f> originally by J. Ungerer 1990
c======================================================================
c $Id$
c
c Reflectivity Method
c
c Copyright 1990 by Joachim Ungerer
c
c Modifcations:
c Copyright 1997, 2010 by Thomas Forbriger
c
c This program calculates synthetic seismograms using the reflectivity
c method. The solution contains the full wavefield including far- and
c nearfield for a moment tensor point source. The original code comes
c from J. Ungerer (refseis.f 1990). It was changed and supplemented by 
c T. Forbriger (refmet.f 1997).
c
c M"uller, G., 1985.
c   The reflectivity method: a tutorial, J. Geophys., 58, 153--174.
c
c Ungerer, J., 1990.
c   Berechnung von Nahfeldseismogrammen mit der Reflektivit"atsmethode, 
c   Diplomarbeit, Institut f"ur Geophysik der Universit"at Stuttgart.
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
c======================================================================
c 
c this is a special version build just to calculate the matrix elements
c
c----------------------------------------------------------------------
c
c REVISIONS and CHANGES (only major changes are reported in detail)
c
c 13/02/1997   V1.0   * copied from refmet.f
c 14/02/1997   V1.1   * reduced code to what is needed when calculating the
c                       matrix while keeping full compatibility with
c                       refmet configuration files
c 10/06/1997   V1.2   * this code now uses a specific usage information
c                       refmat_basinf
c 16/09/2003   V1.3   increased length of command-line arguments
c           
c======================================================================
      PROGRAM refmat

      character*70 version
      parameter(version='REFMAT   V1.3   Reflectivity Matrix')

c array dimension declaration
      integer msl, mf, ms
      PARAMETER  (MSL=4100) 
      PARAMETER  (Mf=MSL/2+1)
      PARAMETER  (MS=250)
c----------------------------------------------------------------------
c
c essential comment on source function, source units and seismogram units:
c
c The source is defined by a source function s(t) and its amplitude A
c A equals M0 (double couple / moment tensor) or F0 (force). The real source
c function is interpreted as being A*s(t) and has the units N*m (moment
c tensor) or N (force). The program expects hfkt (or the read in function
c from file) to be s'(t) (with s'(t) meaning the time derivative of s(t)).
c In this case the seismograms will have the units m/s (particle velocity).
c If you set hfkt to s(t) the seismograms will have the units m (displacement).
c 
c----------------------------------------------------------------------
c 
c IMPORTANT!
c The following comments on variable declaration were not and will
c not be updated. On changes in variable declarations and variable 
c meanings refer to the comments below the original comment block.
c
C23456789012345678901234567890123456789012345678901234567890123456789012
C*******************Joachim Ungerer***********Stuttgart**02.10.1990*****
C                                                                      C
C Programm REFSEIS (Version 02.10.1990)                                C
C entwickelt auf der SUN 4/110 Workstation,geschrieben in SUN-FORTRAN  C
C                                                                      C
C Version 0.1 Oct 22, 1994 Dimensionierung w(MSL) anstatt w(Mf), da    c
c          sonst Fedlueberschreitungen auftreten koennen (H. Langer).  c
C                                                                      C 
C Teil der Dipolmarbeit von Joachim Ungerer                            C
C Angefertigt am Institut fuer Geophysik der Universitaet Stuttgart    C
C                                                                      C
C Moeglichkeiten und Grenzen von REFSEIS.F:                            C
C Das Programm REFSEIS.F erzeugt fuer eine verallgemeinerte Punkt-     C
C quelle beliebiger Anregung, welche sich innerhalb eines horizontal   C
C geschichteten Mediums befindet vollstaendige (Raum- und Oberflaechen-C
C wellen) synthetische Nah- und Fernfeldseismogramme. Grundlage des    C
C Programms sind die Modellannahmen und Ergebnisse der Diplomarbeit    C
C von Joachim Ungerer [Ung 90].                                        C
C Das Medium darf beliebig geschichtet sein.Als Spezialfall der Schich-C
C tung kann auch der Vollraum modelliert werden. Die Schichtparameter  C
C umfassen die wellengeschwindigkeiten von P- und S-Wellen, die Dichte C
C der Schicht, ihre Tiefe und ihre Q-Faktoren fuer P- und S-Wellen. Im C
C Rahmen des verwendeten akausalen Daempfungsmodelles [Ung 90,Kap.VI.2]C
C lassen sich je nach Stationierung und Schichtparameter auch starke   C
C Daempfungseffekte befriedigend beruecksichtigen [Ung 90,Anhang 2].   C
C Der daempfungsfreie Fall laesst sich durch sehr grosse Q-Faktoren    C
C annaehern.                                                           C
C REFSEIS.F berechnet die Seismogramme gesondert in Nah- und Fernfeld- C
C beitraege gemaess [Ung 90,Tabelle V.1]. Je nach Wahl koennen Ver-    C
C schiebungs(Typ=1)-,Geschwindigkeits(Typ=2)- oder Beschleunigungs-    C
C seismogramme(Typ=3) erstellt werden. Eine geeignete AnregungsfunktionC
C fuer den Herd ist im Programm fest installiert(Programmteil Nr.1)    C
C [Ung 90, Gleichung VI.9b]. Nach Bedarf kann sie ausgetauscht werden. C
C Zusatzlich zu den Seismogrammen koennen auch ihre zugehoerigen Spek- C
C tren ausgegeben werden. Unabhaengig von der zeitlichen Anregung ist  C
C auch die Moeglichkeit vorhanden frequenzbegrenzte Impulsseismogramme C
C (Seismogramme infolge einer Anregung durch einen Delta-Impuls) bzw.  C
C die Uebertragungsfunktion (Impulsspektrum) des Modells zu erstellen. C
C     Die Seismogramme beziehen sich auf ein Zylinderkoordinatensystem C
C mit dem Ursprung im Epizentrum. An den Empfangsstationen erhalten    C
C wir somit die drei Seismogrammkomponenten: Radial(r), Azimutal(phi), C
C Vertikal(z).                                                         C
C Die Anzahl der Empfaenger darf prinzipiell beliebig sein. Ihre Loka- C
C lisation bezueglich der Quelle wird durch die Epizentralentfernung   C
C und ihren azimutalen Winkel beschrieben.                             C
C Fuer die Montage der Seismogramme und auch zur Verkuerzung der Re-   C
C chenzeit kann es vorteilhaft sein sie auf gewuenschte Einsatzzeiten  C
C zu reduzieren. Entsprechende Moeglichkeiten sind in REFSEIS.F vor-   C
C handen.                                                              C
C Hinsichtlich der Lokalisation des Herdes besteht nur eine einzige    C
C Einschraenkung. Er darf nicht direkt an der Oberflaeche platziert    C
C werden, da die analytischen Ausdruecke, die dem Programm zugrunde    C
C liegen hierfuer nicht teilweise nicht mehr definiert sind [Ung 90,   C
C Gleichung IV.29].                                                    C
C Art (Explosionsquelle oder Double-Couple) und Orientierung des HerdesC
C werden durch die kartesischen Komponenten seines Momententensors be- C
C schrieben.                                                           C
C Schwierigkeiten bei der numerischen Umsetzung der Reflektivitaets-   C
C methode bereitet vor allem die Integration der Langsamkeitsintegrale.C
C Die Integralausdrucke sind ja nur exakt fuer eine nach oben unbe-    C
C schraenkte Integrationsgrenze.Numerisch aber muessen wir geeignet    C
C abbrechen. Infolge der numerisch begrenzten Integration, die in REF- C
C SEIS.F mit der Trapezregel ausgefuehrt wird, treten daher in den     C
C Seismogrammen verschiedene Stoerungen auf. Sowohl ein zu klein ge-   C
C waehlter Integrationbereich wie auch eine zu grob angesetzte Inte-   C
C grationsgenauigkeit koennen -je nach Tiefe der Quelle,Epizentralent- C
C fernung des Empfaengers, Q-Faktoren der Schichtung- ui betraechtli-  C
C chen Amplitudenverfaelschungen und Stoerphasen fuehren [Ung 90,An-   C
C hang 1].                                                             C
C Zwar koennen diese Stoerungen nie vollkommen beseitigt werden, doch  C
C in Abstimmung von Integrationbereich,Integrationgenauigkeit,Herd-    C
C tiefe, Epizentralentfernung und Daempfungsparameter(Q-Faktoren) las- C
C sen sich mit REFSEIS.F brauchbare Seismogramme erzeugen, die es ge-  C
C statten das Nah- und fernfeldverhalten von Modellbeben zu unter-     C
C suchen.                                                              C
C                                                                      C
C                                                                      C
C Programm-Kurzdokumentation(weiteres im Listing):                     C
C REFSEIS.F berechnet nach der Reflektivitaetsmethode die Seismogramm- C
C komponenten EINER verallgemeinerten Punktquell der Staerke M0 (seis- C
C seismisches Moment) in der Tiefe ZQ innerhalb eines horizontal ge-   C
C schichteten Mediums.                                                 C
C Art und Orientierung der Quelle werden durch die Komponenten ihres   C
C Momententensors festgelegt.Die Anregung der Quelle erfolgt durch     C
C eine definierbare Herdfunktion (siehe Programmteil Nr.1)             C
C Die Seismogrammkomponenten werden getrennt berechnet als:            C
C      Fernfeldkomponenten SFr,SFphi,SFz                               C
C      Nahfeldkomponenten  SNr,SNphi,SNz                               C
C Sie beziehen sich auf ein Zylinderkoordinatensystem mit Ursprung im  C
C Epizentrum.                                                          C
C Die Seismogramme koennen mit der Reduktionsgeschwindigkeit Vred auf  C
C auf eine empfaengerabhaengige Ersteinsatzzeit reduziert und auch     C
C empfaengerunabhaengig nach links(Tli) oder rechts(Tre) verschoben    C
C werden.                                                              C
C Als Seismogrammtypen koennen erstellt werden:                        C
C      -Verschiebungsseismogramme(Typ=1)                               C
C      -Geschwindigkeitsseismogramme(Typ=2)                            C
C      -Beschleunigungsseismogramme(Typ=3)                             C
C                                                                      C
C                                                                      C
C Eingabe ueber Datei:     refseis.dat (Eingabefile)                   C
C Ausgabe auf die Dateien: refseis.erg (Ergebnisfile)                  C
C                          refspek.plt (Plotfile fuer Spektren)        C
C                          refseis.plt (Plotfile fuer Seismogramme)    C
C                                                                      C
C Die Dateien refspek.plt und refseis.plt koennen optional erstellt    C
C werden. Sie dienen als Eingabefiles fuer das Programm REFPLOT.F      C
C (Version 01.10.1990), das die graphissche Darstellung der Seis-      C
C mogramme bzw. Spektren besorgt.                                      C
C                                                                      C
C Die Ergebnisdatei refseis.erg enthaelt das aufbereitete SchichtmodellC
C Ueber die Schalter(i) lassen sich auf ihr optional ausdrucken:       C
C  (1) die vertikalen Langsamkeiten der Schichten, sowie               C
C      die Reflektions- und Transmissionskoeffizienten d.Trennflaechen C
C  (2) die Reflektivitaeten und Transmissivitaeten des Herdes:         C
C          Reflektivitaetsmatrix    RM(inus) = (RM11,RM12,RM21,RM22)   C
C                    "              RP(lus)  = (RP11,RP12,RP21,RP22)   C
C          Reflektivitaetsskalar    rm(inus) = rm                      C
C                    "              rp(lus)  = rp                      C
C          Transmissivitaetsmatrix  TP(lus)  = (TP11,TP12,TP21,TP22)   C
C          Transmissivitaetsskalar  tp(lus)  = tp                      C
C  (3) die Oberflaechenamplituden B0(0,1,2),D0(0,1,2),F0(1,2)          C
C  (4) normierte Impulsspektren(komplex) VF,VN in [s*s/(g*cm)]*1E-20   C
C  (5) Spektren(Betraege) BSF,BSN fuer die Herdstaerke M0              C
C  (7) Fernfeld- und Nahfeldseismogramme in[cm,cm/s,cm/(s*s)]          C
C                                                                      C
C                                                                      C
C benoetigte Subroutinen:                                              C
C       RTKC      zur Berechnung der Reflektions- und Transmisions-    C
C                 koeffizienten                                        C
C       DFORK     schnelle Fouriertransformation                       C
C       MATINV    Matrizeninversion                                    C
C       MATMUL    Matrizenmultiplikation                               C
C Die Subroutinen stehen am Ende des Listings                          C
C                                                                      C
C Abweichungen vom Sprachstandard:                                     C
C besondere Funktionen:                                                C
C       d_j0,d_j1,d_jn  Besselfunktionen 0-ter,1-ter und n-ter Ordnung C
C                       (sind in SUN Fortran so aufzurufen und als     C
C                        DOUBLE PRECISION zu deklarieren)              C
C       dtime(tarray)   Bestimmung der Rechenzeit                      C
C                                                                      C
C erweiterte Befehle und Funktionen:                                   C
C      Die Berechnungen finden mit doppelter Genauigkeit statt.        C
C      Daher laeuft dieses Programm nur, wenn in FORTRAN folgende      C
C      Befehle implementiert sind:  DOUBLE COMPLEX bzw COMPLEX*16,     C
C                                   CDABS,CDEXP,CDSQRT,DCMPLX,DCONJG,  C
C                                   DREAL                              C
C                                                                      C
C                                                                      C
C Bem1:Da die Fouriertransformierte der Verschiebung bei der Frequenz  C
C      Null eine Polstelle besitzt wird fuer die Verschiebungsberech-  C
C      nung gesetzt: fr(1)=1.D-30                                      C
C                                                                      C
C Bem2:Die in den Seismogrammen moeglicherweise auftretenden Stoer-    C
C      phasen(siehe [Ung 90,Anhang 1]) sind meist durch eine Erweiter- C
C      ung des Langsamkeitsbereiches oder eine Steigerung der Integra- C
C      tionsgenauigkeit Nu zu beseitigen.                              C
C                                                                      C
C                                                                      C
C Variablendokumentation:                                              C
C (Die Variablennamen sind weitgehend in Anlehnung an die Diplomarbeit C
C  gewaehlt)                                                           C
C                                                                      C
C    Eingabevariablen:                                                 C
C          Die Eingabe des Modells sollte nach der Vorlage des Ein-    C
C          gabefiles refseis.dat erfolgen.                             C
C          Bis auf die Schaltervariable werden alle Eingabevariablen   C
C          formatiert eingelesen, sie sind deshalb rechtsbuendig in    C
C          ihre jeweiligen Eingabefelder zu setzen.                    C
C          text$         Kopfzeile fuer Text                           C
C      I.  Schalter (0=nein, 1=ja, 2=ja und Ende)                      C
C          Sch(1)        vert.Langsamkeiten,Ref.u.Transmissionskoeff.  C
C          Sch(2)        Reflektivitaeten und Transmissivitaeten       C
C          Sch(3)        Oberflaechenamplituden                        C
C          Sch(4)        normierte Impuls-Spektren(komplex)            C
C          Sch(5)        endgueltige Spektren(Betraege)                C
C          Sch(6)        Plot-File fuer die endgueltigen Spektren      C
C          Sch(7)        Endgueltige Nah- und Fernfeldseismogramme     C
C          Sch(8)        Plot-File fuer Seismogramme                   C
C          Sch(9)        noch frei                                     C
C          Sch(10)       noch frei                                     C
C      II. Schichtdaten                                                C
C          z(i)          Tiefe der Schichtdecke der i-ten Schicht [km] C
C                        (die "nullte" Schicht repraesentiert den obe- C
C                        ren Halbraum,z.B. Atmosphaere,ihre Tiefe ist  C
C                        definitionsgemaess Null)                      C
C          alpha(i)      P-Wellengeschw.[km/s] in Schicht i            C
C          beta(i)       S-   "                "                       C
C          rho(i)        Dichte[g/ccm] der Schicht i                   C
C          Qa(i)         Q-Faktor fuer P-Wellen                        C
C          Qb(i)            "       "  S-Wellen                        C
C          Bem: Grosse Schichtdicken koennen zum Ueberlauf durch Ex-   C
C          ponentialterme fuehren. Abhilfe durch Einfuegen virtueller  C
C          Trennflaechen.                                              C
C      III.Herddaten                                                   C
C          ZQ            Tiefe[km] der ErdbebenQuelle                  C
C          M0            Staerke des Momententensors in [dyn*cm]       C
C          Mxx,Myy,Mzz   normierter Momententensor  M=(ni*fj+nj*fi)    C
C          Mxy,Mxz,Myz   (Bem:seismischer Momententensor MS=M0*M)      C
C          Bem1: Ein grosser Abstand des Herdes zur Schichtdecke kann  C
C          zum Ueberlauf durch die Exponentialterme ea,eb fuehren. Ab- C
C          hilfe durch Einfuegen einer virtuellen Trennflaeche nahe    C
C          ueber dem Herd.                                             C
C          Bem2: Die Umrechnung von Streichen(S),Fallen(F) und Dislo-  C
C          kationswinkel(D) beim Double-Couple in die Vektoren f und n C
C          erfolgt durch: f=(cosD*cosS+cosF*sinD*sinS,cosD*sinS-cosF*  C
C          sinD*cosS,-sinD*sinF); n=(sinF*sinS,-sinF*cosS,cosF)        C
C          [siehe hierzu auch Aki/Richards S.106 ff]                   C
C      IV. Daten fuer Normierte Anregungsfunktion                      C
C          typ      1=Verschiebung,2=Geschwindigkeit,3=Beschleunigung  C
C          The      Einsatzzeit des Herdes                             C
C          Thd      Dauer des Zeitverlaufs von The bis Endwert Null    C
C                   (Momentenanstiegszeit)                             C
C          Bem: Impulsanregung fuer The=Thd=0                          C
C      V.  Reduktionsdaten                                             C
C          Vred     Reduktionsgeschwindigkeit[km/s] (bei Vred=0 keine  C
C                   empfaengerabhaengige Reduktion),die zugehoerige    C
C                   Reduktionszeit ist hypozentralbezogen.             C
C          Tli      Zeitverschiebung[s] des Seismogramms nach rechts   C
C          Tre      Zeitverschiebung[s] des Seismogramms nach links    C
C      VI. Empfaengerdaten                                             C
C          r(E)      Radialentf.[km] des Empfaengers E vom Epizentrum  C
C          phi(E)    Winkel[Grad] des Empfaengers E zum Epizentrum     C
C     VII. Langsamkeitsdaten                                           C
C          umin,umax  Langsamkeitsbereich [s/km]                       C
C          uwil,uwir  Langsamkeits-Window links,rechts [s/km]          C
C                     (Ueblich sind umin=uwil=0, uwir sollte reziprok  C
C                     der kleinsten Wellengeschwindigkeit der Herd-    C
C                     schicht oder groesser sein)                      C
C          Nu         Zahl der gleichabstaendigen Langsamkeit.(NU muss C
C                     mindestens 2 sein)                               C
C          Bem:Langsamkeitsbereich und Integrationsgenauigkeit Nu sind C
C          die masgebenden Groessen um moeglichst stoerungsfreie Seis- C
C          mogramme zu erzeugen. Das rechteckigen Langsamkeitswindow   C
C          [uwil,uwir] wird beidseitig mit einem Cos-Taper bis zu den  C
C          jeweiligen Grenzen [umin,umax] auf Null heruntergezogen ->  C
C          evt. Trennung von Raum und Oberflaechenwellen durch Wahl    C
C          geeigneter Langsamkeitsbereiche.                            C
C     VIII.Frequenz- und Zeitdaten                                     C
C          fmin,fmax     Frequenzbereich [Hz](physikalisch)            C
C          fwil,fwir     Frequenz-Window links,rechts [Hz]             C 
C          Dt            zeitliches Abtastintervall [sec]              C
C          SL            Seismogrammlaenge (Zweierpotenz)=Anzahl der   C
C                        Stuetzstellen des Seismogrammes(maximal=MSL)  C
C          Bem: das rechteckige Frequenzwindow [fwil,fwir] wird beid-  C
C          seitig mit einem Cos-Taper bis zu den Grenzen [fmin,fmax]   C
C          auf Null heruntergezogen -> Frequenzfilter                  C
C                                                                      C
C    Laufvariablen                                                     C
C          E       fuer Empfaenger                                     C
C          f       fuer Frequenzen, teilweise auch fuer Zeiten         C
C          i       fuer Schichten, und als Zaehlindex                  C
C          k       Zaehlindex                                          C
C          l       fuer Langsamkeiten                                  C
C                                                                      C
C    Parameter(koennen nach Belieben im Programm eingestellt werden)   C
C          IME     Imaginaere Einheit                                  C
C          ME      Max.Zahl der Empfaenger                             C
C          Mf          "        physikalischen Frequenzen(=MSL/2+1)    C
C                               bis zur Nyquistfrequenz Fny            C
C          MS          "        Schichten                              C
C          MSL         "        Stuetzstellen (Max.Seismogrammlaenge)  C
C          hin,rueck  -1.,+1. zur Steuerung der Fouriertransformation  C
C          pi      =3.14159265358979                                   C
C                                                                      C
C    Charaktervariablen                                                C
C          typ$    ='Verschiebung','Geschwindigkeit' oder              C
C                   'Beschleunigung', je nach Schalter(9)              C
C          impuls$ ='Impulsanregung'                                   C
C                                                                      C
C    Variablen fuer Rechenzeit                                         C
C          dtime    Funktion zur Bestimmung der Rechenzeit             C
C          tarray   ihre zweidim. Variable(User-Zeit,System-Zeit)      C
C          time     Aufnahmevariable fuer Rechenzeit                   C
C                                                                      C
C    Hilfsvariablen zur Zwischenspeicherung                            C
C          help[reell],hilf,gew                                        C
C          H11,H12,H21,H22                                             C
C          I11,I12,I21,I22                                             C
C                                                                      C
C    Rechenvariablen(fest)                                             C
C          ap   =Wurzel(SL)*Dt Aperiodizitaetsfaktor fuer Fourierkoeff.C
C          alphC(i)  komplexe P-Wellengeschwindigkeit                  C
C          betaC(i)     "     S-Wellen     "                           C
C          d(i)      Dicke der Schicht i in [km]                       C
C          Df        Schrittweite der Frequenz,Frequenzintervall       C
C          Du        Schrittweite der Langsamkeit u                    C
C          DDu       getaperte und gewichtete Integrationsschrittweite C
C          FL        Frequenzlaenge des vollstaendigen Spektrums       C
C          fmi,fma   Index der Frequenzen fmin,fmax                    C
C          Fny       Nyquistfrequenz (fmax muss < Fny sein)            C
C          fr(f)     f-te Frequenz in [Hz]                             C
C          fwl,fwr   Index der Frequenzen fwil,fwir                    C
C          h         Schicht in der der Herd sich befindet             C
C          M0dim     =M0/1.D20 zur Dimensionierung der Seismogramme    C
C          n         Zahl der Schichten                                C
C          NE        Zahl der Empfaenger                               C
C          Nf        Zahl der phys. Frequenzen im Bereich [fmax,fmin]  C
C          Nff        "    "    "       "      im Fenster [fwil,fwir]  C
C          Ny        Index der Nyquistfrequenz Fny (max.phys.Freqenz)  C
C          phiB(E)   Winkel des Empfaengers E in Bogenmass             C
C          t(f)      f-te Zeit in [s], d.h. Zeitwerte des Seismogramms C
C          TL        Zeitlaenge des Seismogramms (Periode)             C
C          tred(f,E) reduzierte Zeitskala                              C
C                                                                      C
C          Variablen der normierten Herdfunktion                       C
C          hfkt(tvar,T1,T2) Definiton der Herdfunktion                 C
C          tvar           ihre Zeitvariable                            C
C          T1,T2          ihre Begrenzungsparameter(T1=The,T2=The+Thd) C
C          g(f)           Feld in dem hfkt je Frequenz abgelegt wird   C
C          Die Dimension der Herdfunktion richtet sich nach ihrer In-  C
C          terpretation:als Verschiebung [ ]  ->  Bodenverschiebung    C
C                       als 1.Ableitung [1/s] ->  Bodengeschwindigkeit C
C                       als 2.Ableitung [1/(s*s)]->  "  beschleunigung C
C                                                                      C
C    Rechenvariablen(veraenderlich)                                    C
C          a(i)     vert.Langsamkeit der P-Wellen in Schicht i [s/km]  C
C          b(i)      "        "      der S-Wellen       "              C
C          E11(i),E22(i)     Phasenmatrix der Schicht i (Diagonalel.)  C
C          ea         =exp[IME*w*a(h)*(ZQ-z(h))]                       C
C          eb         =   "      b(h)    "                             C
C          earg       Argument fuer Exponentialfunktionen              C
C          ftap(f)    Fensterfunktion der Frequenz mit Cos-Taper zur   C
C                     Verwendung als Frequenzfilter,das Rechteckwindow C
C                     [fwil,fwir] wird beidseitig mit einem Cos-Taper  C
C                     bis zu den Frequenzen fmax bzw. fmin auf Null    C
C                     heruntergezogen.                                 C
C          red        Reduktionsfaktor:red=exp[i*w*(R(E)/Vred+Tli-Tre] C
C                     mit R(E)=Wurzel[ZQ*ZQ+r(E)*r(E)]=Hypozentralentf.C
C          u          horizontale Langsamkeit in [s/km]                C
C          uQ         ihr Quadrat                                      C
C          utap       Fensterfunktion der Langsamkeit mit Cos-Taper    C
C                     zum Glaetten der Stoerphasen                     C
C          w(f)       Kreisfrequenz Omega [1/s]                        C
C                                                                      C
C          Reflektions und Transmissionskoeffizienten d.Trennflaeche i C
C          Rppd(i),Rpsd(i),Tppd(i),Tpsd(i)  fuer  P-Wellen von oben    C
C          Rssd(i),Rspd(i),Tssd(i),Tspd(i)  fuer SV-Wellen      "      C
C          rd(i),td(i)                      fuer SH-Wellen      "      C
C          Rppu(i),Rpsu(i),Tppu(i),Tpsu(i)  fuer  P-Wellen von unten   C
C          Rssu(i),Rspu(i),Tssu(i),Tspu(i)  fuer SV-Wellen      "      C
C          ru(i),tu(i)                      fuer SH-Wellen      "      C
C                                                                      C
C          Reflektivitaets- und Transmissivitaetsmatrizen              C
C          RTD11(i),RTD12(i) Reflektivitaetsmatrix f.Einfall von oben, C
C        & RTD21(i),RTD22(i) fuer unteren Stapel ab Decke Schicht i    C 
C          RBU11(i),RBU12(i) Reflektivitaetsmatrix f.Einfall v. unten, C
C        & RBU21(i),RBU21(i) fuer oberen Stapel ab Boden Schicht i     C
C          RTU11(i),RTU12(i) Reflektivitaetsmatrix f.Einfall v. unten, C
C        & RTU21(i),RTU22(i) fuer oberen Stapel ab Decke Schicht i     C
C          TTUo11(i),TTUo12(i) Transmissivitaetsmatrix f.Einf.v.unten, C
C        & TTUo21(i),TTUo22(i) ab Decke Schicht i bis Decke Schicht 0  C
C          S11(i),S12(i)     Schichtmatrix von Schicht i,zur Berech-   C
C        & S21(i),S22(i)     nung der Transmissivitaetsmatrix TTUo     C
C                                                                      C
C          Reflektivitaets- und Transmissivitaetsskalare               C
C          rtd(i)   SH-Reflektivitaet ab Decke Schicht i abwaerts      C
C          rbu(i)   SH-Reflektivitaet ab Boden Schicht i aufwaerts     C
C          rtu(i)   SH-Reflektivitaet ab Decke Schicht i    "          C
C          ttuo(i)  SH-Transmissivitaet ab Decke Schicht i  "          C
C          s(i)     Schichtskalar von Schicht i,fuer Skalar TTU        C  
C                                                                      C
C          Reflektivitaeten und Transmissivitaeten der Herdschicht     C
C          RM11,RM12,RM21,RM22   Reflektivitaetsmatrix RM(inus)        C
C          RP11,RP12,RP21,RP22   Reflektivitaetsmatrix RP(lus)         C
C          TP11,TP12,TP21,TP22   Transmissivitaetsmatrix TP(lus)       C
C          rm                    SH-Reflektivitaet rm(inus)            C
C          rp                    SH-Reflektivitaet rp(lus)             C
C          tp                    SH-Transmissivitaet tp(lus)           C
C                                                                      C
C          Amplituden der Wellenfelder                                 C
C          AQ(0,1,2)  Quell-Amplituden des abwaertslaufenden           C
C          CQ(0,1,2)  Wellenfeldes der Quelle                          C
C          EQ(1,2)         "                                           C
C          BQ(0,1,2)  Quell-Amplituden des aufwaertslaufenden          C
C          DQ(0,1,2)  Wellenfeldes der Quelle                          C
C          FQ(1,1)         "                                           C
C          B0(0,1,2)  Oberflaechenamplituden                           C
C          D0(0,1,2)           "                                       C
C          F0(1,2)             "                                       C
C                                                                      C
C          Empfaengerabhaengige Variablen                              C
C          K0(E),K1(E)  Faktoren des Winkelanteils,enthalten auch      C
C          K2(E),K3(E)  die Komponenten des Momententensors            C
C          L1(E),L2(E)                                                 C
C          jarg         Argument fuer Besselfunktionen                 C
C          J0,J1,J2     zur Aufnahme der Werte der Besselfunktionen    C
C                       (d_j0,d_j1,d_jn, wobei n=2)                    C
C                                                                      C
C          Integrationsvariablen                                       C
C          In1...In6  Hilfsvariablen zur Berechnung der Integranden    C
C          FFI        =w*w/(4pi*rho(h))  Fernfeld-Integrandenfaktor    C
C          NFI        =w/(r*4pi*rho(h))  Nahfeld-    "                 C
C          Integrandenglieder je Langsamkeit fuer f-te Frequenz        C
C          des E-ten Empfaengers:                                      C
C          IDFr(f,E)  Int.glied der radialen    Fernfeldkomponente     C
C          IDNr(f,E)     "             "        Nahfeldkomponente      C
C          IDFphi(f,E)   "        transversalen Fernfeldkomponente     C
C          IDNphi(f,E)   "             "        Nahfeldkomponente      C
C          IDFz(f,E)     "        vertikalen    Fernfeldkomponente     C
C          IDNz(f,E)     "             "        Nahfeldkomponente      C
C                                                                      C
C          normierte Teil-Impulsspektren fuer f-te Frequenz des E-ten  C
C          Empfaengers, fuer Verschiebung in [s*s/(g*cm)]* 1E-20:      C
C          VFr(f,E)   Radiales      Fernfeld-Impulsspektrum            C
C          VNr(f,E)      "           Nahfeld-      "                   C
C          VFphi(f,E) Transversales Fernfeld-      "                   C
C          VNphi(f,E)    "           Nahfeld-      "                   C
C          VFz(f,E)   Vertikales    Fernfeld-      "                   C
C          VNz(f,E)      "           Nahfeld-      "                   C
C                                                                      C
C          End-Spektren(Betraege) fuer Staerke M0:                     C
C          BSFr(f,E),BSFphi(f,E),BSFz(f,E)                             C
C          BSNr(f,E),BSNphi(f,E),BSNz(f,E)                             C
C          in [cm*s],[cm],[cm/s]                                       C
C                                                                      C
C          Transformationsvariable fuer Fouriertransformation          C
C          cxfr(f),cxfphi(f),cxfz(f),cxnr(f),cxnphi(f),cxnz(f)         C
C                                                                      C
C                                                                      C
C    Ergebnissvariablen                                                C
C          Vollstaendige Seismogramme  bzw. Spektren                   C
C          SFr(f,E),SFphi(f,E),SFz(f,E)   Fernfeld                     C
C          SNr(f,E),SNphi(f,E),SNz(f,E)   Nahfeld                      C
C          fuer Bodenverschiebung in     [cm]         bzw. [cm*s]      C
C          fuer Bodengeschwindigkeit in  [cm/s]       bzw. [cm]        C
C          fuer Bodenbeschleunigung in   [cm/(s*s)]   bzw. [cm/s]      C
C**********************************************************************C
c======================================================================
c changes and supplements in parameter declaration after the 16/01/1997
c----------------------------------------------------------------------
c
c 16/01/1997 * all variables starting with cl_ are related to the
c              commandline subroutine tflib_cmdline
c            * removed Sch(10) and time, dtime, tarray
c            * extended dimension of VXx, the arrays SXx and BSXx
c              are removed IDXx are now simple scalars
c            * radius holds the earths radius
c            * outsig now holds former typ, typ now selects different
c              source model types, srcsig selects different source
c              time series
c            * changed arrays qa and qb from integer to real type
c            * use d_j0, d_j1, d_jn from file lmath.c (they are intrinsic
c              in Sun Fortran)
c            * invented all variables needed to create sff-data-files
c              these are fdata and idata and the FREE-block variables
c              sff_free, sff_maxfree, sff_nfree
c            * changed seismogram units to m, m/s, m/s^2 and moment
c              units to N*m (instead of formerly cm, cm/s, cm/s^2 and dyn*cm)
c 21/01/97   * removed typstr, impuls
c 23/01/97   * outunits now contains the userspecified seismogram units
c V1.0 finished
c            * M0 may now have the unit Newton to be the amplitude of
c              a single force
c 27/01/97   * ampcorr, delta and correx handle the flat earth
c              transformation amplitude correction
c            * hfktstr hold a character variable readable version
c              of hfkt
c 31/01/97   * cl_comeach selects second output-file sorting
c 09/02/97   * nuref now contains the reference frequency, dispfac
c              contains the factors for velocity dispersion, domfac
c              contains the velocity factor at the dominant period,
c              disperse is set true if velocity dispersion has to be
c              evaluated, domperset contains the dominant period selected
c              in main file, domper contains the final dominant period
c 10/02/97   * cl_comsel will contain the output component selection
c            * new help variables are: line and help2 for intermediate use
c 11/02/97   * removed domper and domfac and domperset as we have to
c              calculate fully frequency dependent, removed help2
c 13/02/97   * matrixfile holds the name of the optional coefficient file
c              and writematrix is a flag that indicates the coefficients
c              should be written to disk and matrixlu is the logical file unit
c            * The arrays C1,... C8 are now used for matrix holding and!
c              for fourier transform of seismogram components
c 13/02/97   * changed C1,... C8 to single precision
c
c======================================================================
c used libraries and external subroutines
c----------------------------------------------------------------------
c
c libtf.a       containes the commandline parameter routine
c               tflib_cmdline
c libemod.a     from here we get the functions efa_z and efa_r to
c               transform source depth
c 
c some files explicitly concerned to refmat:
c
c refmat_basinf.f     gives help information
c refmat_intro.f      some additional text to basinf
c refmat_comments.f   some additional text to basinf
c refmet_rmain.f      reads main configuration file
c refmet_rmod.f       reads earth model
c refmet_rsource.f    reads source configuration
c
c======================================================================
c parameter declaration

      REAL*8      hin,rueck,pi
      PARAMETER  (hin=-1.D0,rueck=1.D0,pi=3.14159265358979d0)
      COMPLEX*16  IME
      PARAMETER  (IME=(0.D0,1.D0))

      CHARACTER text*70,modeltext*72, sourcetext*72,
     &          outunits*8

      INTEGER   f,fmi,fma,fwl,fwr,h,i,l,n,Nf,Nff,
     &          Nu,Ny,SL,typ,outsig,srcsig
      real      qa(0:MS), qb(0:MS)

      REAL*8    alpha(0:MS),beta(0:MS),d(0:MS),rho(0:MS),z(0:MS),
     &          Du,u,umin,umax,uwil,uwir,uQ,ZQ,
     &          Df,Dt,fmin,fmax,fwil,fwir,FL,Fny,TL,radius,
     &          fr(MSL),t(MSL),w(MSL),help,
     &          M0,Mxx,Myy,Mzz,Mxy,Mxz,Myz,
     &          The, Thd

      COMPLEX*16  a(0:MS),b(0:MS),alphC(0:MS),betaC(0:MS),
     &            Rppd(MS),Rpsd(MS),Rssd(MS),Rspd(MS),rd(MS),
     &            Tppd(MS),Tpsd(MS),Tssd(MS),Tspd(MS),td(MS),
     &            Rppu(MS),Rpsu(MS),Rssu(MS),Rspu(MS),ru(MS),
     &            Tppu(MS),Tpsu(MS),Tssu(MS),Tspu(MS),tu(MS),
     &            RTD11(0:MS),RTD12(0:MS),RTD21(0:MS),RTD22(0:MS),
     &            RBU11(0:MS),RBU12(0:MS),RBU21(0:MS),RBU22(0:MS),
     &            RTU11(0:MS),RTU12(0:MS),RTU21(0:MS),RTU22(0:MS),
     &            TTUo11(0:MS),TTUo12(0:MS),TTUo21(0:MS),TTUo22(0:MS),
     &            rtd(0:MS),rbu(0:MS),rtu(0:MS),ttuo(0:MS),
     &            S11(0:MS),S12(0:MS),S21(0:MS),S22(0:MS),s(0:MS),
     &            RM11,RM12,RM21,RM22,rm,RP11,RP12,RP21,RP22,rp,
     &            TP11,TP12,TP21,TP22,tp

      COMPLEX*16  H11,H12,H21,H22,I11,I12,I21,I22,hilf,earg,
     &            E11(0:MS),E22(0:MS)

      COMPLEX     OTR11(Mf),OTR12(Mf),OTR21(Mf),
     &            OTR22(Mf),ORM11(Mf),ORM12(Mf),
     &            ORM21(Mf),ORM22(Mf),Otr(Mf),Orm(Mf)

c      DOUBLE PRECISION  jarg,J0,J1,J2,d_j0,d_j1,d_jn

c matrix writing
      character*80 matrixfile
      logical writematrix
      integer matrixlu
      parameter(matrixlu=20)
c velocity dispersion
      double precision nuref
      complex*16 dispfac(Mf)
      logical disperse
c earth flattening approximation
      double precision efa_z, efa_r
c sff FREE
      integer sff_maxfree, sff_nfree, sff_freebase
      parameter(sff_maxfree=100+MS)
      character*80 sff_free(sff_maxfree)
c files
      character*80 mainfile, modelfile, sourcefile, receiverfile
c verbosity
      integer lev1,lev2,lev3,lev4
      parameter(lev1=0,lev2=1,lev3=2,lev4=3)
c commandline
      integer cl_maxopt, cl_lastarg, iargc
      parameter(cl_maxopt=3)
      character*2 cl_optid(cl_maxopt)
      character*80 cl_optarg(cl_maxopt)
      logical cl_optset(cl_maxopt), cl_opthasarg(cl_maxopt)
c command line options and parameter values
      integer cl_vlevel
      logical cl_debug, cl_rprogress
c here are the keys to our commandline options
      data cl_optid/2h-d,2h-v,2h-m/
      data cl_opthasarg/.FALSE.,2*.TRUE./
      data cl_optarg/1h-,1h1,1h-/


c======================================================================
c 
c basic setup part
c 
c----------------------------------------------------------------------
c give basic information
      call refmat_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  0, MS, MSL, Mf, sff_maxfree, 'no source')

c      call buggy

c set options
      call tf_cmdline(1, cl_lastarg,
     &     cl_maxopt, cl_optid, cl_optarg, cl_optset, cl_opthasarg)
      cl_debug=cl_optset(1)
      read(cl_optarg(2), '(i10)') cl_vlevel
      writematrix=cl_optset(3)
      if (writematrix) matrixfile=cl_optarg(3)
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
c read model file
      call refmet_rmod(modelfile, modeltext, MS, nuref,
     &  radius, n, z, alpha, beta, rho, qa, qb, cl_vlevel, cl_debug, lev2)

c----------------------------------------------------------------------
c read source configuration
      call refmet_rsource(sourcefile, sourcetext, outunits,
     &  typ, outsig, srcsig, The, Thd, ZQ, M0, Mxx, Myy, Mzz, Mxy, Mxz, Myz,
     &  cl_vlevel, lev2, cl_debug)


C**********************************************************************C
C 5. Vorberechnungen fuer Langsamkeit                                  C
C     -Schrittweite der Langsamkeit Du                                 C
C     -Korrektur von uwil und uwir auf die Schrittweitenskala          C
C**********************************************************************C
C     Schrittweite fuer u
      Du=(umax-umin)/DBLE(Nu-1)
C     Korrektur von uwil und uwir auf die Schrittweitenskala
      uwil=DBLE( IDINT((uwil-umin)/Du) )*Du+umin
      uwir=DBLE( IDINT((uwir-umin)/Du) )*Du+umin


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

c----------------------------------------------------------------------
c 
c evaluate selections for velocity dispersion
c 
c if reference frequency evuals zero or the dominant
c period value in main configuartion file is set to a negative
c value we will ignore velocity dispersion
c

      if (nuref.le.0.d0) then
        disperse=.false.

      else
c calculate with velocity dispersion
        disperse=.true.

c be aware of freuqency zero:
        if (fmi.eq.1) then
          fmi=2
          fwl=max(fmi,fwl)
          fmin=fr(fmi)
          fwil=fr(fwl)
          print *,'NOTICE: I did set the minimum frequency to 1./T=df'
        endif
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
        do i=1,n
          if ((qa(i).le.help).or.(qb(i).le.help)) then
            print *,'ERROR: Q-values must not be smaller than ',help,
     &        ' for smallest frequency ',fr(fmi)
            stop 'ERROR: Q too small for velocity dispersion'
          endif
        enddo
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

c
C     Berechnung der Schichtdicken der Schichten 1 bis n-1
      DO 230 i=0,n-1
         d(i)=z(i+1)-z(i)
  230 CONTINUE
C     Die Dicke des unteren Halbraumes (Schicht n) ist keine Rechen-
C     groesse ,fuer den Kontrollausdruck wird sie auf Null gesetzt
      d(n)=0. 

C     Bestimmung der Herdschicht h (liegt der Herd auf der Trenn-
C     flaeche z(i), gehoert er der Schicht i an) 
      IF(ZQ.LT.z(n)) THEN
        i=0
  240   i=i+1
        IF(z(i)-ZQ.LE.0.) GOTO 240
        h=i-1
      ELSE IF(ZQ.GE.z(n))THEN
        h=n
      ENDIF

C     Komplexe Wellengeschwindigkeiten(hier gehen die Q-Faktoren ein)

c calulate frequency independent complex velocities
      if (.not.(disperse)) then
        do i=0,n
           alphC(i)=alpha(i)*(1.D0+ 0.5D0*IME/Qa(i))
           betaC(i)= beta(i)*(1.D0+ 0.5D0*IME/Qb(i))
        enddo
      endif

c----------------------------------------------------------------------
c
c open coefficient file and write header
c
      if (.not.(writematrix)) stop 'ERROR: refmat MUST write a matrix!'
      if (cl_vlevel.gt.lev2) print 52,'opening ',
     &  matrixfile(1:index(matrixfile, ' ')-1)
      open(matrixlu, form='unformatted', status='new',
     &  file=matrixfile, err=99)
      write(matrixlu, err=95) version
      write(matrixlu, err=95) fmi, fma
      write(matrixlu, err=95) dt, SL, fmin, fwil, fwir, fmax
      write(matrixlu, err=95) Nu, umin, uwil, uwir, umax
      write(matrixlu, err=95) ZQ, radius
      write(matrixlu, err=95) qa(h), qb(h), alpha(h), beta(h), rho(h), z(h)
      write(matrixlu, err=95) qa(0), qb(0), alpha(0), beta(0), rho(0)
      write(matrixlu, err=95) text, modeltext, sourcetext
      write(matrixlu, err=95) nuref


c----------------------------------------------------------------------
c 
c if we use a spherical earth model we have to transform the
c depth of the source
c 
      if (radius.gt.0.d0) ZQ=efa_z(radius, radius-ZQ)

c======================================================================
c 
c do reports to FREE block and to stdout
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
      sff_free(sff_nfree)='   model file: '//
     &  modelfile(1:min(64,index(modelfile, ' ')-1))
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='   '//modeltext
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
      sff_free(sff_nfree)='  matrix file: '//
     &  matrixfile(1:min(64,index(matrixfile, ' ')-1))

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
  460 format('      seismogram length:',i6)
  470 format('  sampling interval [s]:',f11.6,t37,
     &       'frequency interval [Hz]:',f11.6)
  480 format('  seismogram length [s]:',f11.6,t37,
     &       ' nyquist frequency [Hz]:',f11.6)

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
     &  'layer','top[km]','thick[km]','Vp[km/s]','Vs[km/s]',
     &  'rho[g/cm^3]','Qalpha','Qbeta'
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      write(sff_free(sff_nfree), 303) 'top halfspace: ',
     &  alpha(0), beta(0), rho(0), qa(0), qb(0)
      if (n.gt.1) then
        do i=1,n-1
          sff_nfree=min(sff_maxfree, sff_nfree+1)
          if ((z(i).gt.1.).or.(d(i).gt.1)) then
            write(sff_free(sff_nfree), 301)
     &        i, z(i), d(i), alpha(i), beta(i), rho(i), qa(i), qb(i)
          else
            write(sff_free(sff_nfree), 302)
     &        i, z(i), d(i), alpha(i), beta(i), rho(i), qa(i), qb(i)
          endif
        enddo
      endif
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (z(n).gt.1.) then
        write(sff_free(sff_nfree), 305) 'hs: ',
     &    z(n), alpha(n), beta(n), rho(n), qa(n), qb(n)
      else
        write(sff_free(sff_nfree), 304) 'hs: ',
     &    z(n), alpha(n), beta(n), rho(n), qa(n), qb(n)
      endif

  301 format(2x,i5,1x,4(f9.4,1x),f11.4,2(1x,f8.1))
  302 format(2x,i5,1x,2(f9.6,1x),2(f9.5,1x),f11.4,2(1x,f8.1))
  303 format(2x,a15,10x,1x,2(f9.5,1x),f11.4,2(1x,f8.1))
  304 format(2x,a5,1x,f9.6,1x,10x,2(f9.5,1x),f11.4,2(1x,f8.1))
  305 format(2x,a5,1x,f9.4,1x,10x,2(f9.4,1x),f11.4,2(1x,f8.1))
  306 format(2x,a5,1x,4(a9,1x),a11,2(1x,a8))

c output on source model
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (radius.gt.0.d0) then
        WRITE(sff_free(sff_nfree),362) (radius-efa_r(radius, ZQ))
      else
        WRITE(sff_free(sff_nfree),362) ZQ
      endif
  362 FORMAT('           source depth: ',f11.6,' km')



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
        DO 590 i=0,n
           H11= 1.D0/(alphC(i)*alphC(i))
           H22= 1.D0/(betaC(i)*betaC(i))
           a(i) = SQRT(H11-uQ)
           b(i) = SQRT(H22-uQ)
  590   CONTINUE

C     Berechnung er Reflektions und Transmissionskoeffizienten fuer
C     die Trennflaeche i+1 (Aufruf von RTKC)
        DO 600 i=0,n-1
           CALL RTKC (u,a(i),b(i),betaC(i),rho(i),
     &              a(i+1),b(i+1),betaC(i+1),rho(i+1),
     &              Rppd(i+1),Rpsd(i+1),Tppd(i+1),Tpsd(i+1),Rssd(i+1),
     &              Rspd(i+1),Tssd(i+1),Tspd(i+1),rd(i+1),td(i+1),
     &              Rppu(i+1),Rpsu(i+1),Tppu(i+1),Tpsu(i+1),Rssu(i+1),
     &              Rspu(i+1),Tssu(i+1),Tspu(i+1),ru(i+1),tu(i+1))
  600   CONTINUE           
      endif



C~~~~~Grosse Frequenzschleife ueber w ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C
      DO 3000 f=fmi,fma

c 
c using velocity dispersion we have calculate the complex velocities
c each interface matrix for each frequency
c 
      if (disperse) then
        do i=0,n
          alphC(i)=alpha(i)*(1.D0+dispfac(f)/Qa(i))
          betaC(i)= beta(i)*(1.D0+dispfac(f)/Qb(i))
          H11= 1.D0/(alphC(i)*alphC(i))
          H22= 1.D0/(betaC(i)*betaC(i))
          a(i) = SQRT(H11-uQ)
          b(i) = SQRT(H22-uQ)
        enddo
        do i=0,(n-1)
            CALL RTKC(u,a(i),b(i),betaC(i),rho(i),
     &                a(i+1),b(i+1),betaC(i+1),rho(i+1),
     &                Rppd(i+1),Rpsd(i+1),Tppd(i+1),Tpsd(i+1),Rssd(i+1),
     &                Rspd(i+1),Tssd(i+1),Tspd(i+1),rd(i+1),td(i+1),
     &                Rppu(i+1),Rpsu(i+1),Tppu(i+1),Tpsu(i+1),Rssu(i+1),
     &                Rspu(i+1),Tssu(i+1),Tspu(i+1),ru(i+1),tu(i+1))
        enddo
      endif

C**********************************************************************C
C 12. Vorberechnung von Ausdruecken derselben Frequenz w:              C
C     -Phasenmatrizen                                                  C
C**********************************************************************C

C     Phasenmatrizen der Schichten 0 bis n-1 
      DO 900 i=0,n-1
         hilf=-IME*d(i)*w(f)
         earg=a(i)*hilf
         E11(i)=EXP(earg)
         earg=b(i)*hilf
         E22(i)=EXP(earg)
  900 CONTINUE


C**********************************************************************C
C 13. Reflektivitaeten RM(inus) und rm(inus) fuer Welleneinfall von    C
C     oben (pro Langsamkeit):                                          C
C     Berechnet wird im Rekursionsverfahren die Reflektivitaetsmatrix  C
C     RTD(h),sowie die SH-Reflektivitaet rtd(h) fuer die Herdschicht h,C
C     das sind aber gerade RM(inus) und rm(inus).                      C
C     Die Reflektivitaeten bis zu den jeweiligen Schichten i werden    C
C     fuer die aktuelle Langsamkeit zwischengespeichert, so dass auch  C
C     alle RTD(i) und rt(i) bekannt sind (dadurch auch erweiterbar auf C
C     mehrere Herde).                                                  C
C**********************************************************************C

C     Startbedingung fuer Rekursion
C     fuer RTD 
      RTD11(n)=(0.,0.)
      RTD12(n)=(0.,0.)
      RTD21(n)=(0.,0.)
      RTD22(n)=(0.,0.)
C     fuer rtd
      rtd(n)  =(0.,0.)

C     Rekursion von Schicht n-1 bis h  
      DO 1000 i=n-1,h,-1

C        Rekursion fuer RTD
         CALL MATMUL(RTD11(i+1),RTD12(i+1),RTD21(i+1),RTD22(i+1),
     &               Rppu(i+1) ,Rspu(i+1) ,Rpsu(i+1) ,Rssu(i+1) ,
     &               H11       ,H12       ,H21       ,H22       )
         H11=1.D0-H11
         H12=  -H12
         H21=  -H21
         H22=1.D0-H22
         CALL MATINV(H11,H12,H21,H22, I11,I12,I21,I22)
         CALL MATMUL(Tppu(i+1),Tspu(i+1),Tpsu(i+1),Tssu(i+1),
     &               I11      ,I12      ,I21      ,I22      ,
     &               H11      ,H12      ,H21      ,H22      )         
         CALL MATMUL(H11       ,H12       ,H21       ,H22       ,
     &               RTD11(i+1),RTD12(i+1),RTD21(i+1),RTD22(i+1),
     &               I11       ,I12       ,I21       ,I22       )
         CALL MATMUL(I11      ,I12      ,I21      ,I22      ,
     &               Tppd(i+1),Tspd(i+1),Tpsd(i+1),Tssd(i+1),
     &               H11      ,H12      ,H21      ,H22      )
         H11=Rppd(i+1)+H11
         H12=Rspd(i+1)+H12
         H21=Rpsd(i+1)+H21
         H22=Rssd(i+1)+H22
         RTD11(i)=E11(i)*E11(i)*H11
         RTD12(i)=E11(i)*E22(i)*H12
         RTD21(i)=E11(i)*E22(i)*H21
         RTD22(i)=E22(i)*E22(i)*H22
C        Ende Rekursion RTD

C        Rekursion fuer rtd 
         hilf =tu(i+1)*rtd(i+1)*td(i+1)/(1.D0-rtd(i+1)*ru(I+1))
         rtd(i)=(rd(i+1)+hilf)*E22(i)*E22(i)

 1000 CONTINUE
C     Ende Rekursion fuer RTD und rtd 


C     Reflektivitaeten RM(inus) und rm(inus) der Herdschicht
      RM11=RTD11(h)
      RM12=RTD12(h)
      RM21=RTD21(h)
      RM22=RTD22(h)
      rm  =rtd(h)




C**********************************************************************C
C 14. Reflektivitaeten RP(lus) und rp(lus) sowie Transmissivitaeten    C
C     TP(lus) und tp(plus),fuer Welleneinfall von unten:               C
C     Berechnet werden im Rekursionsverfahren die Reflektivitaeten     C
C     RTU(h), rtu(h), sowie die Transmissivitaeten TTUo(h),ttuo(h) des C
C     oberen Stapelbereichs (Schicht 0 bis h-1); das sind aber gerade  C
C     RP,rp und TP,tp.                                                 C
C     Die Reflektivitaeten und Transmissivitaeten ab Decke Schicht 0   C
C     (Bezugsnivaeu des oberen Halbraumes) bis zu den Schichten i      C
C     werden zwischengespeichert.                                      C
C**********************************************************************C

C     Startbedingung fuer Rekursion
      RBU11(0)=(0.,0.)
      RBU12(0)=(0.,0.)
      RBU21(0)=(0.,0.)
      RBU22(0)=(0.,0.)
      rbu(0)=(0.,0.)

      TTUo11(0)=(1.,0.)
      TTUo12(0)=(0.,0.)
      TTUo21(0)=(0.,0.)
      TTUo22(0)=(1.,0.)
      ttuo(0)=(1.,0.)


C     Rekursion von Schicht 0 bis Schicht h-1
      DO 1100 i=0,h-1

C        Rekursion fuer TTUo(i) zuerst
         CALL MATMUL (Rppd(i+1),Rspd(i+1),Rpsd(i+1),Rssd(i+1),
     &                RBU11(i) ,RBU12(i) ,RBU21(i) ,RBU22(i) ,
     &                H11      ,H12      ,H21      ,H22      )
         H11 =1.D0-H11
         H12 =  -H12
         H21 =  -H21
         H22 =1.D0-H22
         CALL MATINV (H11,H12,H21,H22,I11,I12,I21,I22)
         H11 =E11(i)*I11
         H12 =E11(i)*I12
         H21 =E22(i)*I21
         H22 =E22(i)*I22
         CALL MATMUL(H11      ,H12      ,H21      ,H22      ,
     &               Tppu(i+1),Tspu(i+1),Tpsu(i+1),Tssu(i+1),
     &               S11(i)   ,S12(i)   ,S21(i)   ,S22(i)   )
         CALL MATMUL(TTUo11(i),TTUo12(i) ,TTUo21(i)   ,TTUo22(i)  ,
     &               S11(i)   ,S12(i)    ,S21(i)      ,S22(i)     ,
     &             TTUo11(i+1),TTUo12(i+1),TTUo21(i+1),TTUo22(i+1)) 
C        Ende Rekursion TTUo 


C        Rekursion fuer RTU(i) [liefert auch RBU(i) fuer TTUo(i)]
         CALL MATMUL(RBU11(i) ,RBU12(i) ,RBU21(i) ,RBU22(i) ,
     &               Rppd(i+1),Rspd(i+1),Rpsd(i+1),Rssd(i+1),
     &               H11      ,H12      ,H21      ,H22      )
         H11=1.D0-H11
         H12=  -H12
         H21=  -H21
         H22=1.D0-H22
         CALL MATINV(H11,H12,H21,H22,I11,I12,I21,I22)
         CALL MATMUL(Tppd(i+1),Tspd(i+1),Tpsd(i+1),Tssd(i+1),
     &               I11,I12,I21,I22,       H11,H12,H21,H22 ) 
         CALL MATMUL(H11     ,H12     ,H21     ,H22     ,
     &               RBU11(i),RBU12(i),RBU21(i),RBU22(i),
     &               I11     ,I12     ,I21     ,I22     )
         CALL MATMUL(I11      ,I12      ,I21      ,I22      ,
     &               Tppu(i+1),Tspu(i+1),Tpsu(i+1),Tssu(i+1),
     &               H11      ,H12      ,H21      ,H22      )
         RTU11(i+1)=Rppu(i+1)+H11
         RTU12(i+1)=Rspu(i+1)+H12
         RTU21(i+1)=Rpsu(i+1)+H21
         RTU22(i+1)=Rssu(i+1)+H22

C        neue RBU werden auch gebraucht zur Rekursion von TTUo
         RBU11(i+1)=E11(i+1)*E11(i+1)*RTU11(i+1)
         RBU12(i+1)=E11(i+1)*E22(i+1)*RTU12(i+1)
         RBU21(i+1)=E11(i+1)*E22(i+1)*RTU21(i+1)
         RBU22(i+1)=E22(i+1)*E22(i+1)*RTU22(i+1) 
C        Ende Rekursion fuer  RTU
 
 
C        Rekursion fuer ttuo
         s(i)=tu(i+1)/(1.D0-rd(i+1)*rbu(i)) *E22(i)
         ttuo(i+1)=ttuo(i)*s(i)

C        Rekursion fuer rtu
         rtu(i+1)=ru(i+1)+td(i+1)*tu(i+1)*rbu(i)/(1.D0-rd(i+1)*rbu(i))
         rbu(i+1)=rtu(i+1)*E22(i+1)*E22(i+1)

 1100 CONTINUE
C     Ende der Rekursion fuer RTU,TTUO,rtu,ttuo



C     Reflektivitaeten RP(lus),rp(lus) der Herdschicht
      RP11 = RTU11(h)
      RP12 = RTU12(h)
      RP21 = RTU21(h)
      RP22 = RTU22(h)
        rp = rtu(h)
   
C     Transmissivitaeten TP(lus),tp(lus) der Herdschicht 
      TP11 = TTUo11(h)
      TP12 = TTUo12(h)
      TP21 = TTUo21(h)
      TP22 = TTUo22(h)
        tp = ttuo(h)


C**********************************************************************C
C 15. Berechnung der Oberflaechenamplituden B0(i),D0(i) mit i=0,1,2    C
C     und F0(k) mit k=1,2.                                             C
C     Zuerst werden die Quellamplituden AQ(i),CQ(i),EQ(k) und BQ(i),   C
C     DQ(i),FQ(k) bestimmt und daraus dann die Oberflaechenamplituden  C
C     mit Hilfe der Reflektivitaeten und Transmissivitaeten des Herdes C
C     berechnet .                                                      C
C**********************************************************************C



C     Berechnung der Oberflaechenamplituden B0(i),D0(i):
C     - zuerst Berechnung der inversen Matrix der unendlichen
C     Matrizenreihe
      CALL MATMUL(RM11,RM12,RM21,RM22,RP11,RP12,RP21,RP22,
     &            H11,H12,H21,H22)
      H11=1.D0-H11
      H12=  -H12
      H21=  -H21
      H22=1.D0-H22
      CALL MATINV(H11,H12,H21,H22,I11,I12,I21,I22)
C     - dann Multiplikation mit TP(lus) nach [H11,H12,H21,H22]
      CALL MATMUL(TP11,TP12,TP21,TP22,I11,I12,I21,I22,
     &            H11,H12,H21,H22)

c----------------------------------------------------------------------
c 
c catch matrix elemts (here we've got them)
c
       OTR11(f)=H11
       OTR12(f)=H12
       OTR21(f)=H21
       OTR22(f)=H22
       ORM11(f)=RM11
       ORM12(f)=RM12
       ORM21(f)=RM21
       ORM22(f)=RM22
       Otr(f)=tp/(1.d0-rm*rp)
       Orm(f)=rm


 3000 CONTINUE
C~~~~~Ende der grossen Frequenzschleife~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C

c----------------------------------------------------------------------
c 
c write matrix coefficients for one slowness in on block
c
      write(matrixlu, err=95) 
     &  (OTR11(f),OTR12(f),OTR21(f),OTR22(f), 
     &   ORM11(f),ORM12(f),ORM21(f),ORM22(f),
     &   Otr(f), Orm(f), f=fmi,fma)

 4000 CONTINUE
C~~~~~Ende der grossen Langsamkeitsschleife~~~~~~~~~~~~~~~~~~~~~~~~~~~~C
  
c----------------------------------------------------------------------
c 
c close matrix file
c
      if (writematrix) then
        if (cl_vlevel.gt.lev2) print 52,'closing ',
     &    matrixfile(1:index(matrixfile, ' ')-1)
        close(matrixlu, err=96)
      endif


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
      END
 
C *** Ende REFSEIS ****************************************************C





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


      SUBROUTINE RTKC(u,a1,b1,betaC1,rho1,a2,b2,betaC2,rho2,
     &                Rppd,Rpsd,Tppd,Tpsd,Rssd,Rspd,Tssd,Tspd,rd,td,
     &                Rppu,Rpsu,Tppu,Tpsu,Rssu,Rspu,Tssu,Tspu,ru,tu)


C Deklaration der Variablen ********************************************

C     Variablen fuer beide Faelle
      REAL*8      u,uQ,rho1,rho2,rho12

      COMPLEX*16  Rppd,Rpsd,Tppd,Tpsd,Rssd,Rspd,Tssd,Tspd,rd,td,
     &            Rppu,Rpsu,Tppu,Tpsu,Rssu,Rspu,Tssu,Tspu,ru,tu,
     &            betaC1,betaC2,a1,a2,b1,b2,a1b1,a2b2,a1b2,a2b1,ab,
     &            abm,rhoabm,C,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,
     &            mue1,mue2,mue1b1,mue2b2,muebN,muebP

C     nur fuer Einfall von oben
      COMPLEX*16  D1D,D2D,DD,D1,D2,D3

C     nur fuer Einfall von unten
      COMPLEX*16  D1U,D2U,DU,U1,U2,U3          



C Berechnungen ********************************************************C

C Berechnungen der Rechenvariablen fuer beide Faelle
      uQ    = u*u
      mue1  = rho1*betaC1*betaC1
      mue2  = rho2*betaC2*betaC2
      mue1b1= mue1*b1
      mue2b2= mue2*b2
      muebN = mue1b1-mue2b2
      muebP = mue1b1+mue2b2
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
      rd    = muebN/muebP
      td    = rd + 1.D0

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
      ru    =-muebN/muebP
      tu    = ru + 1.D0

      RETURN
      END

C***Ende von RTKC *****************************************************C









C23456789012345678901234567890123456789012345678901234567890123456789012
C Subroutine fuer Matrizeninversion
C Bem: DOUBLE COMPLEX muss implementiert sein
 
      SUBROUTINE MATINV(A11,A12,A21,A22,B11,B12,B21,B22)
      COMPLEX*16   A11,A12,A21,A22,B11,B12,B21,B22,D
      D=1.D0/(A11*A22-A12*A21)
      B11= A22*D
      B12=-A12*D
      B21=-A21*D
      B22=A11*D
      RETURN
      END



C23456789012345678901234567890123456789012345678901234567890123456789012
C Subroutine fuer Matrizenmultiplikation
C Bem: DOUBLE COMPLEX muss implementiert sein

      SUBROUTINE MATMUL(A11,A12,A21,A22,B11,B12,B21,B22,C11,C12,C21,C22)
      COMPLEX*16   A11,A12,A21,A22,B11,B12,B21,B22,C11,C12,C21,C22
      C11=A11*B11+A12*B21
      C12=A11*B12+A12*B22
      C21=A21*B11+A22*B21
      C22=A21*B12+A22*B22
      RETURN
      END


