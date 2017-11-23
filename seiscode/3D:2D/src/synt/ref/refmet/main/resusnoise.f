c this is <resusnoise.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 1990 by Joachim Ungerer
c
c Modifcations:
c Copyright 2007, 2010 by Thomas Forbriger
c
c NOTICE: It appears to be too complex to implement this here just for some
c tests. I will rather make a modification of gresy.f from the gremlin stuff.
c
c Create noise seismograms from refmat coefficients
c
c this is a version derived from resus.f
c convolves the data of each receiver with gaussian noise
c all receivers will be stacked immediatly
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
c    04/05/2007   V1.0   Thomas Forbriger
c
c ============================================================================
c
c this is <resus.f> originally by J. Ungerer 1990
c======================================================================
c
c Reflectivity Method
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
c----------------------------------------------------------------------
c 
c this is a special version calculating seismograms from a matrix-file
c
c======================================================================
      PROGRAM resusnoise

      character*70 version
      parameter(version=
     &  'RESUS   V1.0   reflectivity stacked noise')

c array dimension declaration
      integer msl, mf, maxrec
c      integer me, msl, mf
c      PARAMETER  (ME=10)
      PARAMETER  (maxrec=1000)
      PARAMETER  (MSL=2050) 
      PARAMETER  (Mf=MSL/2+1)
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
c              and readmatrix is a flag that indicates the coefficients
c              should be written to disk and matrixlu is the logical file unit
c            * The arrays C1,... C8 are now used for matrix holding and!
c              for fourier transform of seismogram components
c 14/02/97   * removed all variables that are not needed calculating
c              seismomgrams (model etc)
c 20/02/97   * deltaflat introduced for appropriate amplitude correction
c 22/10/97   * introduced campcorr to build phase shifted seismograms
c              behind 180 degrees
c 19/06/98   * introduced behindap to hold information about receiver
c              locations behind antipode
c 16/02/99   * removed array campcorr
c            * introduced hilbfact
c 30/03/99   * introduced filtering variables
c
c======================================================================
c used libraries and external subroutines
c----------------------------------------------------------------------
c
c libtf.a       containes the commandline parameter routine
c               tf_cmdline
c lmath.c       delivers the three interfaces d_j0, d_j1, d_jn to the
c               libmath.a (C-style) functions j0, j1, jn (to be used
c               with f2c-Fortran and gcc.
c libemod.a     from here we get the functions efa_z and efa_r to
c               transform source depth
c 
c some files explicitly concerned to resus:
c
c resusnoise_basinf.f gives help information
c refmet_intro.f      some additional text to basinf
c refmet_comments.f   some additional text to basinf
c refmet_rmain.f      reads main configuration file
c refmet_rsource.f    reads source configuration
c refmet_rrcv.f       reads receiver configuration
c refmet_output.f     write seismograms to disk
c refmet_wtrace.f     write one seismic trace in SFF-Format
c refmet_preptrace.f  subroutines to prepare SFF-Data array
c
c======================================================================
c parameter declaration

      REAL*8      hin,rueck,pi
      PARAMETER  (hin=-1.D0,rueck=1.D0,pi=3.14159265358979d0)
      COMPLEX*16  IME
      PARAMETER  (IME=(0.D0,1.D0))

      CHARACTER text*70, sourcetext*72, receivertext*72, 
     &          outunits*8,line*80

      INTEGER   E,f,fmi,fma,fwl,fwr,h,i,k,l,NE,Nf,Nff,
     &          Nu,Ny,SL,typ,outsig,srcsig

      REAL*8    Du,u,umin,umax,uwil,uwir,utap,uQ,gew,ZQ,
     &          Df,Dt,fmin,fmax,fwil,fwir,FL,Fny,TL,radius,
     &          fr(MSL),ftap(Mf),t(MSL),w(MSL),
     &          phi(maxrec),phiB(maxrec),r(maxrec),FFI,NFI,
     &          ap,hfkt,Thd,The,T1,T2,tvar,
     &          tred(MSL),Tli,Tre,Vred,help,
     &          M0,M0dim,Mxx,Myy,Mzz,Mxy,Mxz,Myz,
     &          K0,K1,K2,K3,L1,L2

      COMPLEX*16  ah,bh,alphCh,betaCh,alphCtop,betaCtop,atop,btop

      COMPLEX*16  H11,H22,I11,I22,hilf,
     &            DDu,ea,eb,earg,red,g(MSL),
     &            In1,In2,In3,In4,In5,In6,
     &            AQ(0:2),BQ(0:2),CQ(0:2),DQ(0:2),EQ(2),FQ(2),
     &            B0(0:2),D0(0:2),F0(2)

      COMPLEX*16  IDFr,IDFphi,IDFz,
     &            IDNr,IDNphi,IDNz,
     &            VFr(MSL),VFphi(MSL),VFz(MSL),
     &            VNr(MSL),VNphi(MSL),VNz(MSL), C(MSL)

      COMPLEX     IRM11(Mf),IRM12(Mf),IRM21(Mf),
     &            IRM22(Mf),ITR11(Mf),ITR12(Mf),
     &            ITR21(Mf),ITR22(Mf),irm(Mf),itr(Mf)

      DOUBLE PRECISION  jarg,J0,J1,J2,d_j0,d_j1,d_jn

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
c distance correction
      real*8 ampcorr(ME), delta, deltaflat
      integer correx
      logical behindap
c output signal filters
      integer fil_maxstage,fil_hpstages,fil_lpstages
      parameter(fil_maxstage=20)
      real fil_hpf(fil_maxstage)
      real fil_lpf(fil_maxstage)
      integer fil_hpo(fil_maxstage)
      integer fil_lpo(fil_maxstage)
      complex tf_hpbut, tf_lpbut, fil_coeff
c hilbert tranformation
      double complex hilbfact
c sff FREE
      integer sff_maxfree, sff_nfree, sff_freebase
      parameter(sff_maxfree=100+2*fil_maxstage)
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
      parameter(cl_maxopt=10)
      character*2 cl_optid(cl_maxopt)
      character*80 cl_optarg(cl_maxopt)
      logical cl_optset(cl_maxopt), cl_opthasarg(cl_maxopt)
c command line options and parameter values
      integer cl_vlevel
      character*80 cl_comsel
      logical cl_debug, cl_rprogress, cl_comeach, cl_phaseshift
      logical cl_coortrans
c here are the keys to our commandline options
      data cl_optid/2h-d,2h-v,2h-o,2h-c,2h-s,2h-m,2h-p,2h-i,2h-l,2h-h/
      data cl_opthasarg/.FALSE.,2*.TRUE.,.FALSE.,2*.TRUE.,2*.FALSE.,2*.TRUE./
      data cl_optarg/1h-,1h1,9hresus.out,1h-,2h  ,5*1h-/

C**********************************************************************C
C 1. Herdfunktion (als Funktionsdefinition)                            C
C                                                                      C
C Die zugrunde liegende Momentenfunktion ist entnommen aus:            C
C Bruestle,Mueller:Physics of the earth 32,83 und lautet:              C
C M(t)=Mo*9/16* {[1-cos(pi*t/T)] + 1/9*[cos(3*pi*t/T)-1]}              C
C Wir gehen aus von ihrer 1.Ableitung:3*pi/4T*{[sin(pi*t/T)]**3} ,um   C
C daraus die Bodengeschwindigkeit direkt, die Bodenverschiebung und    C
C Bodenbeschleunigung indirekt ueber die Fouriertransformation zu bere-C
C chnen(siehe Programmteil Nr.9). Die Polstelle der Verschiebungstrans-C
C formierten an der Stelle fr(1)=0 wird abgefangen durch fr(1)=1.D-30).C
C Bei Einbau neuer Momentenfunktionen ist lediglich zu beachten, dass  C
C sie als Geschwindigkeitsfunktionen behandelt  werden. Ihre Anfangs-  C
C und Endwerte muessen Null sein.                                      C
C**********************************************************************C
C     Geschwindigkeitsanregung(1.Ableitung d. Bruestle-Mueller-Funktion)
C     hfkt(tvar,T1,T2)=0.75*pi/(T2-T1)*dsin(pi*(tvar-T1)/(T2-T1))**3
 

C**********************************************************************C
C 1. Herdfunktionen (als Funktionsdefinitionen)                        C
C**********************************************************************C
CC    Geschwindigkeitsanregung(1.Ableitung d. Bruestle-Mueller-Funktion)
      hfkt(tvar,T1,T2)=0.75*pi/(T2-T1)*dsin(pi*(tvar-T1)/(T2-T1))**3
      hfktstr='hfkt(tvar,T1,T2)=0.75*pi/(T2-T1)*dsin(pi*(tvar-T1)/(T2-T1))**3'
CC    smooth blow
c      hfkt(tvar,T1,T2)=dsin(pi*(tvar-T1)/(T2-T1))**3
c      hfktstr='hfkt(tvar,T1,T2)=dsin(pi*(tvar-T1)/(T2-T1))**3'
CC    Geschwindigkeitsanregung(1.Ableitung d. Brune Modell)              
c      hfkt(tvar,T1,T2)=1./T2* DEXP(-(tvar-T1)/(T2-T1))
c      hfktstr='hfkt(tvar,T1,T2)=1./T2* DEXP(-(tvar-T1)/(T2-T1))'

c======================================================================
c 
c basic setup part
c 
c----------------------------------------------------------------------
c give basic information
      call resusnoise_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  1, maxrec, MSL, Mf, sff_maxfree, hfktstr)

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
      cl_phaseshift=cl_optset(7)
      cl_coortrans=((cl_optset(7)).or.(cl_optset(8)))
c 
      fil_hpstages=0
      fil_lpstages=0
      if (cl_optset(9)) then
        read (cl_optarg(9), *) fil_lpstages
        if (fil_lpstages.gt.fil_maxstage) 
     &    stop 'ERROR: too many lowpass filter stages'
        read (cl_optarg(9), *) fil_lpstages, 
     &    (fil_lpf(i),fil_lpo(i),i=1,fil_lpstages)
      endif
      if (cl_optset(10)) then
        read (cl_optarg(10), *) fil_hpstages
        if (fil_hpstages.gt.fil_maxstage) 
     &    stop 'ERROR: too many highpass filter stages'
        read (cl_optarg(10), *) fil_hpstages, 
     &    (fil_hpf(i),fil_hpo(i),i=1,fil_hpstages)
      endif
c 
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

c----------------------------------------------------------------------
c read receiver configuration
      call refmet_rrcv(receiverfile, receivertext,
     &  Vred, Tli, Tre, NE, maxrec, r, phi, radius,
     &  cl_vlevel, lev2, cl_debug)
      if (cl_debug) then
        print *,'DEBUG receivers ',(r(E), E=1,NE)
        print *,'DEBUG angles ',(phi(E), E=1,NE)
      endif


c----------------------------------------------------------------------
c set output component selection in case of vertical single force
      if (typ.eq.2) then
        line='TTNZNRNTFZFRFT'//cl_comsel
        cl_comsel=line
        print *,'NOTICE: set component selection to ',
     &    cl_comsel(1:(index(cl_comsel, ' ')-1))
        print *,'NOTICE: because of vertical single force'
      endif
      if (cl_debug) print *,'DEBUG cl_comsel ',cl_comsel

C**********************************************************************C
C 4. Initialisierungen von Feldern und Variablen                       C
C**********************************************************************C
C     Initialisierung des Frequenzfilters ftap(f)
      DO 185 f=1,SL/2+1
         ftap(f)=0.
  185 CONTINUE

C     Initialisierung
C     der normierten Teilspektren bis zur Nyquistfrequenz
      behindap=.false.
      DO 184 f=1,SL
        VFr(f)  =dcmplx(0.,0.)
        VFphi(f)=dcmplx(0.,0.)
        VFz(f)  =dcmplx(0.,0.)
        VNr(f)  =dcmplx(0.,0.)
        VNphi(f)=dcmplx(0.,0.)
        VNz(f)  =dcmplx(0.,0.)
  184 CONTINUE

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

C     Fensterfunktion ftap(f) der Frequenz mit Cos-Taper,
C     zur Verwendung als Frequenzfilter: beidseitig des rechteckigen
C     Frequenzwindows [fwil,fwir] wird das Frequenzfenster mit einem
C     Cos-Taper jeweils bis zu den Grenzfrequenzen fmin und fmax auf 
C     Null heruntergezogen.
      DO 210 f=fmi,fma
         IF(fmin.LE.fr(f) .and. fr(f).LT.fwil) THEN
            ftap(f)=0.5D0*(1.D0-dcos(pi*(fr(f)-fmin)/(fwil-fmin)))
         ELSE IF(fwil.LE.fr(f) .and. fr(f).LE.fwir) THEN
            ftap(f)=1.D0 
         ELSE IF(fwir.LT.fr(f) .and. fr(f).LE.fmax) THEN
            ftap(f)=0.5D0*(1.D0+dcos(pi*(fr(f)-fwir)/(fmax-fwir)))
         ENDIF
  210 CONTINUE




C**********************************************************************C
C 7. Vorberechnungen fuer Empfaenger                                   C
C     -Umrechnung der Winkel phi(E) in Bogenmass phiB(E)               C
C     -Winkel- und Tensorparameter K0(E),K1(E),K2(E),K3(E),L1(E),L2(E) C
C**********************************************************************C

c----------------------------------------------------------------------
c 
c if we use a spherical earth model we have to transform the
c depth of the source
c 
c is NOT done by refmat
      if (radius.gt.0.d0) ZQ=efa_z(radius, radius-ZQ)

c----------------------------------------------------------------------
c 
c check time of first samples
c
      DO E=1,NE
         IF(Vred.EQ.0.)THEN
           help=0.
         ELSE
           help=DSQRT(ZQ*ZQ+r(E)*r(E))/Vred
         ENDIF
         if ((t(f)+help+Tli-Tre).lt.0.d0) stop
     &     'ERROR: traveltime reduction setting will result in negative times'
      enddo

c----------------------------------------------------------------------
c
c Precalculate the amplitude correction factors for each receiver.
c This must be doen for spherical models that were transformed by
c a Flat Earth Approximation (EFA).
c
c The used formula ist given by Mueller in his Tutorial and in:
c
c  Mueller, G., 1977. 
c    Earth-Flattening Approximation for Body Waves Derived from 
c    Geometric Ray Theory - Improvements, Corrections and Range of 
c    Applicability, J. Geophys., 42, 429-436.
c
      if (radius.gt.0.d0) then

c determine exponent
        if (typ.eq.1) then
          correx=2
        elseif (typ.eq.2) then
          correx=1
        elseif (typ.eq.3) then
          correx=1
        else
          correx=0
        endif
c do it
        do e=1,ne
           delta=max(r(E),0.001d0)
           delta=delta/radius
           deltaflat=delta
           if (delta.gt.pi) delta=2.d0*pi-delta
           if (delta.gt.pi) 
     &       stop 'ERROR: receiver coordinate senseless on spherical earth'
           if (delta.lt.0.d0)
     &       stop 'ERROR: negative receiver'
           if (sin(delta).eq.0.d0) then
             print *,'ERROR: at receiver #',e,' dist ',r(e)
             stop 'ERROR: amplitude correction breaks down'
           endif
           ampcorr(e)=((radius/(radius-ZQ))**correx)*sqrt(deltaflat/sin(delta))
           if (deltaflat.gt.pi) then
             behindap(e)=.true.
           endif
           if (cl_debug) then
             print *,'DEBUG: rcv# ',e,' r(E) ',r(E),' deltaflat ',deltaflat,
     &         ' delta ',delta,' ampcorr(E) ',ampcorr(e)
           endif
        enddo
      endif

c----------------------------------------------------------------------
c
c evaluate source coefficients

      DO 220 E=1,NE
      if (typ.eq.1) then
c in case of a moment tensor source
C       Umrechnung der Winkel in Bogenmass
        phiB(E)=2.D0*pi*phi(E)/360.D0

C       Winkel- und Tensorparameter
        K0(E)=Mzz
        K1(E)=Mxz*cos(phiB(E))+Myz*sin(phiB(E))
        K2(E)=0.5D0*(Myy-Mxx)*cos(2.D0*phiB(E))-Mxy*sin(2.D0*phiB(E))
        hilf=Mxx*cos(phiB(E))**2.D0 + Myy*sin(phiB(E))**2.D0
        K3(E)=hilf + Mxy*sin(2.D0*phiB(E))
        L1(E)=Mxz*sin(phiB(E))-Myz*cos(phiB(E))
        L2(E)=0.5D0*(Myy-Mxx)*sin(2.D0*phiB(E))+Mxy*cos(2.D0*phiB(E))
        if ((behindap(e)).and.(cl_coortrans)) then
          k1(e)=-k1(e)
          l1(e)=-l1(e)
        endif
      else
c not a moment tensor source (single force)
        K0(E)=0.d0
        K1(E)=0.d0
        K2(E)=0.d0
        K3(E)=0.d0
        L1(E)=0.d0
        L2(E)=0.d0
      endif
  220 CONTINUE

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
      IF (srcsig.eq.2) THEN
        DO 260 f=1,Ny
           g(f)=DCMPLX(1.)
  260   CONTINUE

C     sonst Berechnung der Herdfunktion und Fouriertransformation
c 
c The factor ap is apllied to make DFORK a correct normalized transform
c from frequency domain to time domain equivalent to a time and frequency
c limited Fourier Transform.
c
      ELSE
C       Herdfunktion im Zeitbereich
        DO 270 f=1,SL
           IF(t(f).LT.The) THEN
             g(f)=DCMPLX(0.)
           ELSE IF(t(f).GE.The .and. t(f).LE.The+Thd) THEN
             g(f)=DCMPLX(hfkt(t(f),The,The+Thd))*ap
           ELSE IF(t(f).GT.The+Thd) THEN
             g(f)=DCMPLX(0.)
           ENDIF
  270   CONTINUE

C       Schnelle Fouriertransformation:Aufruf von DFORK
        CALL DFORK(SL,g,hin)
      ENDIF

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
      
c information on output stage filters
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='Output filters:'
      if (fil_lpstages.gt.0) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)='  lowpass stages:'
        do i=1,fil_lpstages
          sff_nfree=min(sff_maxfree, sff_nfree+1)
          write(sff_free(sff_nfree), 492) i,fil_lpf(i),fil_lpo(i)
        enddo
      else
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)='  lowpass ist not activated'
      endif
      if (fil_hpstages.gt.0) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)='  highpass stages:'
        do i=1,fil_hpstages
          sff_nfree=min(sff_maxfree, sff_nfree+1)
          write(sff_free(sff_nfree), 492) i,fil_hpf(i),fil_hpo(i)
        enddo
      else
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)='  highpass ist not activated'
      endif
  492 format('    stage no.',i2,'   eigenfrequency=',e10.3,'Hz   order=',i2)

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
      if (radius.gt.0.d0) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    '  depth of source was transformed to flat geometry'
        sff_nfree=min(sff_maxfree, sff_nfree+1)
          WRITE(sff_free(sff_nfree),362) (radius-efa_r(radius, ZQ))
  362     FORMAT('  original source depth: ',f11.6,' km')
      endif

c output on receiver configuration
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=' '
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='receiver configuration:'
      if (radius.gt.0.d0) then 
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        write(sff_free(sff_nfree), 380) 'receiver','epicentral distance [km]',
     &    'azimuth angle [degrees]','amp. corr.'
        do e=1,ne
          sff_nfree=min(sff_maxfree, sff_nfree+1)
          write(sff_free(sff_nfree), 390) e, r(e), phi(e), ampcorr(E)
        enddo
  380   format(2x,a10,1x,a25,1x,a25,1x,a12)
  390   format(2x,i10,1x,f25.6,1x,f25.3,1x,f12.4)
      else
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        write(sff_free(sff_nfree), 381) 'receiver','epicentral distance [km]',
     &    'azimuth angle [degrees]'
        do e=1,ne
          sff_nfree=min(sff_maxfree, sff_nfree+1)
          write(sff_free(sff_nfree), 391) e, r(e), phi(e)
        enddo
  381   format(2x,a10,1x,a25,1x,a25)
  391   format(2x,i10,1x,f25.6,1x,f25.3)
      endif

c report amplitude correction scheme
      if (radius.gt.0.d0) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=' '
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'amplitude correction factor according to flat earth approximation:'
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        write(sff_free(sff_nfree), 5050) correx
 5050   format("(R/(R-ZQ))**",i1,
     &    "*sqrt(delta/sin(delta))   delta: epicentral distance")
      else
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=' '
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'there is no amplitude correction applied as this is a flat model'
      endif
      if (cl_phaseshift) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=' '
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'introduced polar phase shift by applying a hilbert transform'
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'in the frequency domain.'
      endif
      if (cl_coortrans) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=' '
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'horizontal components (of source and receiver) are inverted'
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)=
     &    'for receivers behind antipode.'
      endif


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
        FFI=w(f)*w(f)/(4.D0*pi*rhoh)
      elseif (typ.eq.2) then
c single vertical force
        FFI=w(f)/(4.d0*pi*rhoh) 
      endif



C~~~~~Grosse Empfaengerschleife ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C
      DO 2000 E=1,NE


C**********************************************************************C
C 17. Berechnung der einzelnen Integrandenglieder der spektralen Ver-  C
C     schiebungskomponenten fuer eine Langsamkeit,aber fuer alle Fre-  C
C     quenzen und Empfaenger:                                          C
C       -zuerst Aufruf der Besselfunktionen                            C
C       -Fernfeldintegrandenglieder IDFr(f,E),IDFphi(f,E),IDFz(f,E)    C
C       -Nahfeldintegrandenglieder  IDNr(f,E),IDNphi(f,E),IDNz(f,E)    C
C***********************************************************************

C     Besselfunktionen aufrufen   
      jarg=u*w(f)*r(E)
      J0=d_j0(jarg)  
      J1=d_j1(jarg)
      J2=d_jn(2,jarg) 

c calculate integrands

      if (typ.eq.1) then

c for a moment tensor source
C     Berechnung der Fernfeld-Integrandenglieder
        hilf=J1*(K0(E)*In1+K3(E)*In2)
        IDFr= FFI*u*(K1(E)*In3*J0-hilf)
        IDFphi=-FFI*u*(F0(1)*L1(E)*J0+F0(2)*L2(E)*J1)
        hilf=J0*(K0(E)*In4+K3(E)*In5)
        IDFz=FFI*IME*u*(hilf+K1(E)*In6*J1)

C     Berechnung der Nahfeld-Integrandenglieder
        NFI=w(f)/(r(E)*4.D0*pi*rhoh)
        H11=  (F0(1)-In3)*J1
        H22=2.D0*(F0(2)-In2)*J2
        IDNr  =NFI*(K1(E)*H11+K2(E)*H22)
        IDNphi=NFI*(L1(E)*H11+L2(E)*H22)
        IDNz=NFI*IME*2.D0*K2(E)*In5*J1

      elseif (typ.eq.2) then

c for a vertical single force
        IDFr=-FFI*In1*J1
        IDFz=FFI*IME*In4*J0
c   no tangential component, no near field
        IDFphi=dcmplx(0.d0)
        IDNphi=dcmplx(0.d0)
        IDNr=dcmplx(0.d0)
        IDNz=dcmplx(0.d0)

      endif



C**********************************************************************C
C 18. Berechnung der Komponenten des normierten Impulsspektrums:       C
C     (Berechnung der Langsamkeitsintegrale)                           C
C       -Fernfeldkomponenten VFr(f,E),VFphi(f,E),VFz(f,E)              C
C       -Nahfeldkomponenten  VNr(f,E),VNphi(f,E),VNz(f,E)              C
C     Fuer jeden Empfaenger E und jede Frequenz f werden die Komponen- C
C     ten des Impulsspektrums aus ihren Integrandengliedern mittels    C
C     der Trapezregel "aufintegriert" (d.h.aufsummiert):               C
C     Int(x1,xN)[f(x)dx] = dx*[0.5*f(x1)+f(x2)+....+f(xN-1)+0.5*f(xN)] C
C     Die Summation fuer jeden Empfaenger und jede Frequenz erfolgt    C
C     ueber die Langsamkeit.Die vollstaendigen Spektren liegen vor     C
C     wenn die Langsamkeitsschleife abgearbeitet ist.                  C
C       -zusaetzlich wird das Langsamkeitsfenster utap draufmultipli-  C
C        ziert (DDu=Du*gew*utap siehe Nr.11)                           C
C**********************************************************************C

C     Aufsummierung zu Fernfeldspektren
      VFr(f,E)  =VFr(f,E)  +  IDFr*DDu
      VFphi(f,E)=VFphi(f,E)+IDFphi*DDu
      VFz(f,E)  =VFz(f,E)  +  IDFz*DDu

C     Aufsummierung zu Nahfeldspektren
      VNr(f,E)  =VNr(f,E)  +  IDNr*DDu
      VNphi(f,E)=VNphi(f,E)+IDNphi*DDu
      VNz(f,E)  =VNz(f,E)  +  IDNz*DDu


 

 2000 CONTINUE
C~~~~~Ende der grossen Empfaengerschleife~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C


 3000 CONTINUE
C~~~~~Ende der grossen Frequenzschleife~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~C


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

C**********************************************************************C
C 19. vollstaendiges Spektrum fuer die Quelle der Staerke M0:          C
C     -Spektren SFr,SFphi,SFz und SNr,SNphi,SNz durch Multiplikation   C
C      mit der spektralen und gefilterten Anregungsfunktion g(f).      C
C     -Die Dimensionierung wird durch M0dim fuer die Verschiebungs-    C
C      dichte auf [cm] festgelegt.                                     C
C     -Reduktion der Spektren durch Multiplikation mit dem Reduktions- C
C      faktor red (bewirkt spaeter die Zeitverschiebung der Seismo-    C
C      gramme),zusaetzlich wird die Reduktionszeit tred(f,E) pro Em-   C
C      pfaenger ermittelt, um die Zeitskalierung anzupassen.           C
C     -Anschliessend Ergaenzung zu vollstaendigen komplexen Spektren   C
C     -dann Betraege der vollstaendigen komplexen Spektren:            C
C         Fernfeld:BSFr,BSFphi,BSFz                                    C 
C         Nahfeld :BSNr,BSNphi,BSNz                                    C
C**********************************************************************C


      DO 5100 E=1,NE
         DO 5000 f=fmi,fma
C           Fernfeld-Spektren 
            VFr(f,E)  =M0dim*VFr(f,E)*g(f)
            VFphi(f,E)=M0dim*VFphi(f,E)*g(f)
            VFz(f,E)  =M0dim*VFz(f,E)*g(f)
C           Nahfeld-Spektren
            VNr(f,E)  =M0dim*VNr(f,E)*g(f)
            VNphi(f,E)=M0dim*VNphi(f,E)*g(f)
            VNz(f,E)  =M0dim*VNz(f,E)*g(f)
 5000    CONTINUE
 5100 CONTINUE

c----------------------------------------------------------------------
c 
c Apply amplitude correction factors for flat earth transform
c
      if (radius.gt.0.d0) then
        DO E=1,NE
           DO f=fmi,fma
c             Fernfeld-Spektren 
              VFr(f,E)  =VFr(f,E)  *ampcorr(E)
              VFphi(f,E)=VFphi(f,E)*ampcorr(E)
              VFz(f,E)  =VFz(f,E)  *ampcorr(E)
c             Nahfeld-Spektren
              VNr(f,E)  =VNr(f,E)  *ampcorr(E)
              VNphi(f,E)=VNphi(f,E)*ampcorr(E)
              VNz(f,E)  =VNz(f,E)  *ampcorr(E)
           ENDDO
        ENDDO
      endif

c----------------------------------------------------------------------
C     Reduktion der Spektren nach Verschiebungssatz
      DO 5120 E=1,NE
C        Bei Vred=0 keine Empfaengerreduktion
         IF(Vred.EQ.0.)THEN
           help=0.
         ELSE
C          Reduktion bezueglich Hypozentralenfernung
           help=DSQRT(ZQ*ZQ+r(E)*r(E))/Vred
         ENDIF
c do we have to apply hilbert transformation
         if ((behindap(e)).and.(cl_phaseshift)) then
           hilbfact=(0.d0,1.d0)
         else
           hilbfact=(1.d0,0.d0)
         endif
c 
         DO 5110 f=1,SL
C           Berechnung der Reduktionszeit tred(f,E)
            tred(f,E)=t(f)+help+Tli-Tre
 5110    CONTINUE
         do f=fmi,fma
C           und des Reduktionsfaktors red
            red=EXP(IME*w(f)*(help+Tli-Tre))
            if (fr(f).gt.0.d0) red=red*hilbfact
C           Reduktion der Fernfeld-Spektren
            VFr(f,E)  =VFr(f,E)*red
            VFphi(f,E)=VFphi(f,E)*red
            VFz(f,E)  =VFz(f,E)*red
C           Reduktion der Nahfeld-Spektren
            VNr(f,E)  =VNr(f,E)*red
            VNphi(f,E)=VNphi(f,E)*red
            VNz(f,E)  =VNz(f,E)*red
         enddo
c invert sign in case of receivers behind antipode
         if ((behindap(e)).and.(cl_coortrans)) then
           do f=fmi,fma
             VFphi(f,E)=-VFphi(f,E)
             VFr(f,E)  =-VFr(f,E)
             VNphi(f,E)=-VNphi(f,E)
             VNr(f,E)  =-VNr(f,E)
           enddo
         endif
 5120 CONTINUE

c filter output time series
      do f=fmi,fma
c lowpass
        if (fil_lpstages.gt.0) then
          do i=1,fil_lpstages
            fil_coeff=tf_lpbut(sngl(fr(f)),fil_lpf(i),fil_lpo(i))
            do E=1,NE
C  Filterung der Fernfeld-Spektren
              VFr(f,E)  =VFr(f,E)*fil_coeff
              VFphi(f,E)=VFphi(f,E)*fil_coeff
              VFz(f,E)  =VFz(f,E)*fil_coeff
C  Filterung der Nahfeld-Spektren
              VNr(f,E)  =VNr(f,E)*fil_coeff
              VNphi(f,E)=VNphi(f,E)*fil_coeff
              VNz(f,E)  =VNz(f,E)*fil_coeff
            enddo
          enddo
        endif
c highpass
        if (fil_hpstages.gt.0) then
          do i=1,fil_hpstages
            fil_coeff=tf_hpbut(sngl(fr(f)),fil_hpf(i),fil_hpo(i))
            do E=1,NE
C  Filterung der Fernfeld-Spektren
              VFr(f,E)  =VFr(f,E)*fil_coeff
              VFphi(f,E)=VFphi(f,E)*fil_coeff
              VFz(f,E)  =VFz(f,E)*fil_coeff
C  Filterung der Nahfeld-Spektren
              VNr(f,E)  =VNr(f,E)*fil_coeff
              VNphi(f,E)=VNphi(f,E)*fil_coeff
              VNz(f,E)  =VNz(f,E)*fil_coeff
            enddo
          enddo
        endif
      enddo

C     Ergaenzung zu vollstaendigen komplexen Spektren(Symmetrisierung
C     zur Nyquistfrequenz)
      DO 5300 E=1,NE
         DO 5200 f=0,SL/2-2
            VFr(SL-f,E)  =DCONJG(VFr(f+2,E))
            VFphi(SL-f,E)=DCONJG(VFphi(f+2,E))
            VFz(SL-f,E)  =DCONJG(VFz(f+2,E))
            VNr(SL-f,E)  =DCONJG(VNr(f+2,E))
            VNphi(SL-f,E)=DCONJG(VNphi(f+2,E))
            VNz(SL-f,E)  =DCONJG(VNz(f+2,E))
 5200    CONTINUE
 5300 CONTINUE


C**********************************************************************C
C 20. Fouriertransformation des vollstaendigen Spektrums in den        C
C     Zeitbereich (Ruecktransformation) zum Seismogramm; jeweils       C
C     pro Empfaenger.                                                  C
C      -zur Fouriertransformation in den Zeitbereich werden die        C
C       spektralen Komponenten SFr(f,E)....SNz(f,E) auf die            C
C       cxfr(f)....cxnz(f) geschrieben.                                C
C      -nach Aufruf von DFORK stehen auf den cx die Seismogramm-       C
C       komponenten.                                                   C
C      -sie werden auf die Variablen SFr(f,E)....SNz(f,E) rueck-       C
C       geschrieben, die jetzt als Seismogrammkomponenten gelten,      C
C      -dabei Beruecksichtigung der Aperiodizitaet durch den Faktor ap.C
C      -die z-Komponenten werden negiert, damit sich die Seismogramm-  C
C       ausschlaege auf die Oberflaechennormale(nach oben) beziehen    C
C***********************************************************************

      call refmet_ft(VFr,   C, SL, NE, MSL, ME, ap, rueck)
      call refmet_ft(VFphi, C, SL, NE, MSL, ME, ap, rueck)
      call refmet_ft(VFz,   C, SL, NE, MSL, ME, ap, rueck)
      call refmet_ft(VNr,   C, SL, NE, MSL, ME, ap, rueck)
      call refmet_ft(VNphi, C, SL, NE, MSL, ME, ap, rueck)
      call refmet_ft(VNz,   C, SL, NE, MSL, ME, ap, rueck)

      if (cl_debug) print *,'DEBUG: sff_maxfree, sff_nfree ',
     &  sff_maxfree, sff_nfree
c======================================================================
c
c output to file
c
      if (cl_comeach) then
        call refmet_outputc(basename, fdata, idata, MSL, SL, ME, NE, outunits,
     &    cl_comsel,
     &    sff_free, sff_maxfree, sff_nfree, VFz, VFr, VFphi, VNz, VNr, VNphi,
     &    ZQ, The, Thd, typ, r, phi, radius, 
     &    dt, tred, cl_vlevel, cl_debug, lev2)
      else
        call refmet_outputr(basename, fdata, idata, MSL, SL, ME, NE, outunits,
     &    cl_comsel,
     &    sff_free, sff_maxfree, sff_nfree, VFz, VFr, VFphi, VNz, VNr, VNphi,
     &    ZQ, The, Thd, typ, r, phi, radius,
     &    dt, tred, cl_vlevel, cl_debug, lev2)
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



c----------------------------------------------------------------------
c 
c fourier transform to time domain
c
      subroutine refmet_ft(V, C, SL, NE, MSL, ME, ap, rueck)
c 
      integer SL, NE, MSL, ME
      complex*16 V(MSL,ME), C(MSL)
      real*8 ap, rueck
c 
      integer E, f
c 
      DO 7000 E=1,NE
         DO 6500 f=1,SL
C           Umschreiben auf Transformationsvariable
            C(f)  =V(f,E)
 6500    CONTINUE

C        Ruecktransformation in den Zeitbereich:Seismogramme
         CALL DFORK(SL,C,rueck)

C        Umschreiben der cx..(f) auf die Seismogrammkomponenten
C        S..(f,E), dabei Beruecksichtigung der Aperiodizitaet durch ap
C        Bem: die z-Komponenten werden negiert,damit die Ausschlaege
C        sich auf die Oberflaechennormale(nach oben) beziehen
c 
c The factor ap is apllied to make DFORK a correct normalized transform
c from frequency domain to time domain equivalent to a time and frequency
c limited Fourier Transform.
c
         DO 6600 f=1,SL
            V(f,E)  =C(f)*1.D0/ap
 6600    CONTINUE
 7000 CONTINUE
      return
      end
c
c ----- END OF resusnoise.f ----- 
