c this is <refmet.f> originally by J. Ungerer 1990
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
c Fuchs, K. and Mueller, G., 1971.
c  Computation of Synthetic Seismograms with the Reflectivity Method and
c  Comparison with Observations. Geophys. J. R. astr. Soc., 23(4), 417-433.
c
c Mueller, G., 1985.
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
c REVISIONS and CHANGES (only major changes are reported in detail)
c
c 16/01/1997   V1.0   * copied from refseis.f
c                     * adding usual commandline style
c                     * removed $ from variable names, changed CDSQRT,
c                       CDEXP and CDABS to generics, now use bessel functions
c                       from numerical recipes
c                     * removed intermediate output control by Sch(i)
c                     * following a hint from Karl Koch most of the
c                       two dimensional arrays are removed - we use only
c                       VFz,... for the full calculation
c                     * intermediate output is send to stdout instead of
c                       refseis.erg, input is read from four different files
c                     * added new style configuration file reading (four files)
c 21/01/97            * removed old style control output by using
c                       sff FREE-block now
c 23/01/97            * added subroutines copytrace, addtraces, writetrace
c                       refmet_basinf, refmet_rmain, refmet_rsource,
c                       refmet_rmod, refmet_rrcv, refmet_output
c                       they were moved to separate files for debugging
c                       convenience
c                     * FIRST RUNNING VERSION 
c 23/01/1997   V1.1   * includes vertical single force now
c 23/01/1997   V1.2   * check frequency range
c                     * include flat earth amplitude correction
c                     * include report on source function
c 28/01/1997   V1.3   * calculate receiver distance from angles
c                       changed refmet_rrcv
c 29/01/1997   V1.4   * moved amplitude correction calculation to the front
c                       of the program - this allows better control
c                     * included check for sensefull distances (less than 180°)
c 31/01/1997   V1.5   * now delta pulse source will be integrated or
c                       differentiated
c                     * include second output file sorting 
c                       (one component per file)
c 04/02/1997   V1.6   check values for traveltime reduction
c 07/02/1997   V1.7   * when building the source signal the factor ap
c                       must not be applied to the delta-pulse spectrum
c                     * included transformation of source depth to
c                       spherical geometry
c 09/02/1997   V1.8   * set sign of z-component to positive z direction
c                       which is pointing into the halfspace (original
c                       version refseis.f changed sign of vertical component
c                       before output)
c                     * changed to include reference frequency for model
c                       parameters and velocity dispersion (see refmet_basinf
c                       for an explanation of the strategy)
c 10/02/1997   V1.9   * correct declaration of domfac
c                     * extended information on model dispersion
c                     * added output component selection 
c 11/02/1997   V1.10  * fixed velocity dispersion algorithm
c 12/02/1997   V2.0   * fixed bug in RTKC-loop for dispersion (lower
c                       loop boundary must be zero!)
c 13/02/1997   V2.1   * possibility to write coefficients to disk
c 14/02/1997   V2.2   * removed matrix writing routine as there
c                       are now special versions refmat and resus
c 20/02/1997   V2.3   * introduced appropriate amplitude correction for
c                       epicentral distances greater than 180 degrees
c 24/02/1997   V2.4   * selector for delta pulse is srcsig not typ
c 01/04/1997   V2.5   * corrected evaluation of outsig to free block
c 16/02/1999          changed tflib_cmdline call to tf_cmdline
c 25/02/1999   V2.6   allow output coordinates format S
c 29/04/2000   V2.7   introduced Hankel functions
c 11/02/2003   V2.8   introduced new parameter definition (for const-Q case
c                     only): tabluated velocity refers to real part of modulus
c 23/02/2009   V2.9   porting code to gfortran
c 02/03/2010   V2.10  implementing line source
c 03/03/2010   V2.11  correction of line source kernel
c 04/03/2010   V2.12  line source requires scaling with units of wavenumber to
c                     provide seismograms in units of 1m. Wavenumber
c                     is given in units of 1.e-3 rad / m
c              V2.12a use factor 1.e3 rather than 1.e-3
c 14/01/2011   V2.13  implement libfapidxx interface
c 12/10/2011   V2.14  implemented radial component for line source
c 16/01/2012          both components of the line source were tested
c                     successfully against analytical and FD results by 
c                     Lisa Groos (TFSoftware ticket:135)
c           
c======================================================================
      PROGRAM refmet

      character*70 version
      parameter(version='REFMET   V2.14  Reflectivity Method')
      character*79 cvsid
      parameter(cvsid='$Id$')

c array dimension declaration
      integer me, msl, mf, ms
      PARAMETER  (ME=500)
      PARAMETER  (MSL=32800) 
      PARAMETER  (Mf=MSL/2+1)
      PARAMETER  (MS=40)
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
c 14/02/97   * remove matrix variables
c 20/02/97   * introduced deltaflat for amplitude correction on epicentral
c              distances greater than 180 degrees
c 29/04/00   * introduced H0, H1, H2 for Hankel function evaluation
c            * and the switches cl_hankel1 and cl_hankel2
c            * declared Neumann functions
c            * introduced cl_hankelswitch
c 02/03/10   * cl_linesrc
c
c======================================================================
c used libraries and external subroutines
c----------------------------------------------------------------------
c
c libtf.a       containes the commandline parameter routine
c               tflib_cmdline
c lmath.c       delivers the three interfaces d_j0, d_j1, d_jn to the
c               libmath.a (C-style) functions j0, j1, jn (to be used
c               with f2c-Fortran and gcc.
c libemod.a     from here we get the functions efa_z and efa_r to
c               transform source depth
c 
c some files explicitly concerned to refmet:
c
c refmet_basinf.f     gives help information
c refmet_intro.f      some additional text to basinf
c refmet_comments.f   some additional text to basinf
c refmet_rmain.f      reads main configuration file
c refmet_rmod.f       reads earth model
c refmet_rsource.f    reads source configuration
c refmet_rrcv.f       reads receiver configuration
c refmet_output.f     write seismograms to disk
c refmet_wtrace.f     write one seismic trace in SFF-Format
c refmet_preptrace.f  subroutines to prepare SFF-Data array
c
c======================================================================
c parameter declaration

      double precision      hin,rueck,pi
      PARAMETER  (hin=-1.D0,rueck=1.D0,pi=3.14159265358979d0)
      double complex  IME
      PARAMETER  (IME=(0.D0,1.D0))

      CHARACTER text*70,modeltext*72, sourcetext*72, receivertext*72, 
     &          outunits*8,line*80

      INTEGER   E,f,fmi,fma,fwl,fwr,h,i,k,l,n,NE,Nf,Nff,
     &          Nu,Ny,SL,typ,outsig,srcsig
      real      qa(0:MS), qb(0:MS)

      double precision alpha(0:MS),beta(0:MS),d(0:MS),rho(0:MS),z(0:MS),
     &          Du,u,umin,umax,uwil,uwir,utap,uQ,gew,ZQ,
     &          Df,Dt,fmin,fmax,fwil,fwir,FL,Fny,TL,radius,
     &          fr(MSL),ftap(Mf),t(MSL),w(MSL),
     &          phi(ME),phiB(ME),r(ME),FFI,NFI,
     &          ap,hfkt,Thd,The,T1,T2,tvar,
     &          tred(MSL,ME),Tli,Tre,Vred,help,
     &          M0,M0dim,Mxx,Myy,Mzz,Mxy,Mxz,Myz,
     &          K0(ME),K1(ME),K2(ME),K3(ME),L1(ME),L2(ME)

      double complex  a(0:MS),b(0:MS),alphC(0:MS),betaC(0:MS),
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

      double complex  H11,H12,H21,H22,I11,I12,I21,I22,hilf,
     &            DDu,ea,eb,earg,red,g(MSL),
     &            In1,In2,In3,In4,In5,In6,
     &            AQ(0:2),BQ(0:2),CQ(0:2),DQ(0:2),EQ(2),FQ(2),
     &            B0(0:2),D0(0:2),F0(2),E11(0:MS),E22(0:MS)

      double complex  IDFr,IDFphi,IDFz,
     &            IDNr,IDNphi,IDNz,
     &            VFr(MSL,ME),VFphi(MSL,ME),VFz(MSL,ME),
     &            VNr(MSL,ME),VNphi(MSL,ME),VNz(MSL,ME)

      double complex  C1(MSL),C2(MSL),C3(MSL),
     &            C4(MSL),C5(MSL),C6(MSL)

      DOUBLE PRECISION  jarg,J0,J1,J2,d_j0,d_j1,d_jn
      double precision d_y0, d_y1, d_yn

c velocity dispersion
      double precision nuref
      double complex dispfac(Mf)
      logical disperse
c earth flattening approximation
      double precision efa_z, efa_r
c source function
      character*80 hfktstr
c distance correction
      double precision ampcorr(ME), delta, deltaflat
      integer correx
c sff FREE
      integer sff_maxfree, sff_nfree, sff_freebase
      parameter(sff_maxfree=100+MS+2*ME)
      character*90 sff_free(sff_maxfree)
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
c Hankel functions
      double complex H0, H1, H2
      logical cl_hankel1, cl_hankel2
      logical cl_linesrc
      double precision cl_hankelswitch
c commandline
      integer cl_maxopt, cl_lastarg, iargc
      parameter(cl_maxopt=10)
      character*3 cl_optid(cl_maxopt)
      character*40 cl_optarg(cl_maxopt)
      logical cl_optset(cl_maxopt), cl_opthasarg(cl_maxopt)
c command line options and parameter values
      integer cl_vlevel
      character*80 cl_comsel, cl_fileformat
      logical cl_debug, cl_rprogress, cl_comeach
c here are the keys to our commandline options
      data cl_optid/2h-d,2h-v,2h-o,2h-c,2h-s,2h-1,2h-2,2h-p,2h-l,
     &              '-ty'/
      data cl_opthasarg/.FALSE.,2*.TRUE.,.FALSE.,.TRUE.,2*.FALSE.,
     &              .TRUE.,.FALSE.,.true./
      data cl_optarg/1h-,1h1,10hrefmet.out,1h-,2h  ,2*1h-,2h0.,1h-,
     &               'sff'/

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
      print *,cvsid
      call refmet_basinf(version, mainfile, lev1, lev2, lev3, lev4,
     &  ME, MS, MSL, Mf, sff_maxfree, hfktstr)

c      call buggy

c set options
      call tf_cmdline(1, cl_lastarg,
     &     cl_maxopt, cl_optid, cl_optarg, cl_optset, cl_opthasarg)
      cl_debug=cl_optset(1)
      read(cl_optarg(2), '(i10)') cl_vlevel
      basename=cl_optarg(3)
      cl_comeach=cl_optset(4)
      cl_comsel=cl_optarg(5)
      cl_hankel1=cl_optset(6)
      cl_hankel2=cl_optset(7)
      read(cl_optarg(8), *) cl_hankelswitch
      cl_linesrc=cl_optset(9)
      cl_fileformat=cl_optarg(10)
      cl_rprogress=.false.
      if (cl_vlevel.lt.0) cl_rprogress=.true.
      cl_vlevel=abs(cl_vlevel)
      if (cl_debug) print *,'DEBUG: cl_vlevel', cl_vlevel
c get filename
      if (iargc().eq.(cl_lastarg)) stop 'ERROR: filename?'
      if (iargc().gt.(cl_lastarg+1))
     &  print *,'WARNING: additional parameters are ignored'
      call getarg((cl_lastarg+1), mainfile)

c
c select fapid file format
      call sff_select_output_format(cl_fileformat, i)
      if (i.ne.0) stop 'ERROR: selecting output file format'
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

c----------------------------------------------------------------------
c read receiver configuration
      call refmet_rrcv(receiverfile, receivertext,
     &  Vred, Tli, Tre, NE, ME, r, phi, radius,
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
      DO 190 E=1,NE
C        der normierten Teilspektren bis zur Nyquistfrequenz
         DO 184 f=1,SL
            VFr(f,E)  =dcmplx(0.,0.)
            VFphi(f,E)=dcmplx(0.,0.)
            VFz(f,E)  =dcmplx(0.,0.)
            VNr(f,E)  =dcmplx(0.,0.)
            VNphi(f,E)=dcmplx(0.,0.)
            VNz(f,E)  =dcmplx(0.,0.)
  184    CONTINUE
  190 CONTINUE

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

      DO 220 E=1,NE
      if (typ.eq.1) then
c in case of a moment tensor source
C       Umrechnung der Winkel in Bogenmass
        phiB(E)=2.D0*pi*phi(E)/360.D0

C       Winkel- und Tensorparameter
        K0(E)=Mzz
        K1(E)=Mxz*dcos(phiB(E))+Myz*dsin(phiB(E))
        K2(E)=0.5D0*(Myy-Mxx)*dcos(2.D0*phiB(E))-Mxy*dsin(2.D0*phiB(E))
        hilf=Mxx*dcos(phiB(E))**2.D0 + Myy*dsin(phiB(E))**2.D0
        K3(E)=hilf + Mxy*dsin(2.D0*phiB(E))
        L1(E)=Mxz*dsin(phiB(E))-Myz*dcos(phiB(E))
        L2(E)=0.5D0*(Myy-Mxx)*dsin(2.D0*phiB(E))+Mxy*dcos(2.D0*phiB(E))
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
           if (delta.gt.pi) then
             stop 'ERROR: amplitude correction: epicentral distance too large'
           endif
           if (delta.lt.0.d0) then
            stop 'ERROR: amplitude correction: epicentral distance is negative'
           endif
           if (sin(delta).eq.0.d0) then
             print *,'ERROR: at receiver #',e,' dist ',r(e)
             stop 'ERROR: amplitude correction breaks down'
           endif
           ampcorr(e)=((radius/(radius-ZQ))**correx)*sqrt(deltaflat/sin(delta))
        enddo
      endif

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
           alphC(i)=alpha(i)*sqrt(1.D0+ IME/Qa(i))
           betaC(i)= beta(i)*sqrt(1.D0+ IME/Qb(i))
        enddo
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
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)=cvsid

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
      sff_free(sff_nfree)='receiver file: '//
     &  receiverfile(1:min(64,index(receiverfile, ' ')-1))
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      sff_free(sff_nfree)='   '//receivertext

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
  401 format('Hankel functions are used for slowness values greater than ',
     &       f6.3,' s/km')
  410 format(4(1x,f11.6),1x,i11)
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (cl_hankel1) then
        sff_free(sff_nfree)='use the first Hankel functions H^(1)_m'
      elseif (cl_hankel2) then
        sff_free(sff_nfree)='use the second Hankel functions H^(2)_m'
      elseif (cl_linesrc) then
        sff_free(sff_nfree)=
     &    'use line source (vertical force of isotropic MT only)'
      else
        sff_free(sff_nfree)='use the Bessel functions J_m'
      endif
      if ((cl_hankel1).or.(cl_hankel2)) then
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        write(sff_free(sff_nfree), 401) cl_hankelswitch
      endif

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
      if (cl_debug) then
        print *,'DEBUG: (sff_nfree, sff_maxfree) ',
     &    sff_nfree, sff_maxfree
      endif
      write(sff_free(sff_nfree), 490) Vred, Tli, Tre
  460 format('      seismogram length:',i6)
  470 format('  sampling interval [s]:',f11.6,t37,
     &       'frequency interval [Hz]:',f11.6)
  480 format('  seismogram length [s]:',f11.6,t37,
     &       ' nyquist frequency [Hz]:',f11.6)
  490 format('Vred [km/s]:',F8.4,'     Tr [s]:',F6.2,
     &          '     Tl [s]:',F6.2)

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
c   52 format(/a,1x,a)

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
      sff_free(sff_nfree)='source parameters:'
c srcsig
      sff_nfree=min(sff_maxfree, sff_nfree+1)
      if (srcsig.eq.1) then
        sff_free(sff_nfree)='  source signal is compiled function:'
        sff_nfree=min(sff_maxfree, sff_nfree+1)
        sff_free(sff_nfree)='  '//hfktstr(1:78)
      elseif (srcsig.eq.2) then
        sff_free(sff_nfree)='  source signal is delta pulse'
      elseif (srcsig.eq.3) then
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
      elseif (outsig.eq.3) then
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
      elseif (typ.eq.3) then
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
  370   FORMAT(8X, ' duration Td[s]:',F11.5,T45,' Myz=',F7.3)
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


C     Fensterfunktion utap der Langsamkeit mit Cos-Taper
      IF (umin.LE.u .and. u.LT.uwil) THEN
         utap=0.5D0*(1.D0-dcos(pi*(u-umin)/(uwil-umin)))
      ELSE IF(uwil.LE.u .and. u.LE.uwir) THEN
         utap=1.D0
      ELSE IF (uwir.LT.u .and. u.LE.umax) THEN
         utap=0.5D0*(1.D0+dcos(pi*(u-uwir)/(umax-uwir)))
      ENDIF

C     Gewichtungsfaktor fuer Trapezregel.Der erste und letzte Funk-
C     tionswert werden nur halb gewichtet(gew=0.5)
      gew=1.D0
      IF ( l.EQ.1  .OR. l.EQ.Nu ) gew=0.5D0
C     getaperte und gewichtete Integrationsschrittweite fuer Trapezregel
      DDu=DCMPLX(Du*gew*utap)


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

C     Bestimmung der Amplitudenexponenten
      hilf=IME*w(f)*(ZQ-z(h))
      earg=a(h)*hilf
      ea=EXP(earg)
      earg=b(h)*hilf
      eb=EXP(earg)

c      if (cl_debug) then
c        print *,'DEBUG (l,f,IME,a(h),ea,u): ',
c     &      l,f,IME,a(h),ea,u
c      endif

C     source amplitudes
      if (typ.eq.1) then

c moment tensor source
        AQ(0)=IME*a(h)*ea
        AQ(1)=2.D0*u*ea
        AQ(2)=IME*u*u/a(h)*ea 
        CQ(0)=IME*u*eb
        CQ(1)=(u*u/b(h)-b(h))*eb
        CQ(2)=-IME*u*eb
        EQ(1)=eb/(betaC(h)*betaC(h))
        EQ(2)=EQ(1)*IME*u/b(h)

        BQ(0)=IME*a(h)/ea
        BQ(1)=-2.D0*u/ea
        BQ(2)=IME*u*u/(a(h)*ea)
        DQ(0)=-IME*u/eb
        DQ(1)=(u*u/b(h)-b(h))/eb
        DQ(2)=IME*u/eb
        FQ(1)=-1.D0/(betaC(h)*betaC(h)*eb)
        FQ(2)=-FQ(1)*IME*u/b(h)

      elseif (typ.eq.2) then

c vertical single force
        AQ(0)=u*ea
        AQ(1)=dcmplx(0.d0)
        AQ(2)=dcmplx(0.d0)
        CQ(0)=u*u*eb/b(h)
        CQ(1)=dcmplx(0.d0)
        CQ(2)=dcmplx(0.d0)
        EQ(1)=dcmplx(0.d0)
        EQ(2)=dcmplx(0.d0)

        BQ(0)=-u/ea
        BQ(1)=dcmplx(0.d0)
        BQ(2)=dcmplx(0.d0)
        DQ(0)=u*u/(b(h)*eb)
        DQ(1)=dcmplx(0.d0)
        DQ(2)=dcmplx(0.d0)
        FQ(1)=dcmplx(0.d0)
        FQ(2)=dcmplx(0.d0)

      endif
c      if (cl_debug) then
c        print *,'DEBUG (l,f,(AQ(i),BQ(i),CQ(i),DQ(i),EQ(i),FQ(i),i=0,2)): ',
c     &      l,f,(AQ(i),BQ(i),CQ(i),DQ(i),EQ(i),FQ(i),i=0,2)
c      endif


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

c      if (cl_debug) then
c        print *,'DEBUG (l,f,H11,H12,H21,H22,I11,I22,det,TP11,TP12,TP21,TP22): ',
c     &    l,f,H11,H12,H21,H22,I11,I22,
c     &    H11*H22-H12*H21,TP11,TP12,TP21,TP22
c      endif

C     - dann Multiplikation mit TP(lus) nach [H11,H12,H21,H22]
      CALL MATMUL(TP11,TP12,TP21,TP22,I11,I12,I21,I22,
     &            H11,H12,H21,H22)
C     - schliesslich Berechnung von B0(i),D0(i)
      DO 1300 i=0,2
C        Multiplikation von RM(inus) mit Vektor [AQ(i),CQ(i)] und
C        Addition zu Vektor [BQ(i),DQ(i)]; abgelegt auf [I11,I22].
         I11=BQ(i)+RM11*AQ(i)+RM12*CQ(i)
         I22=DQ(i)+RM21*AQ(i)+RM22*CQ(i)

c        if (cl_debug) then
c          print *,'DEBUG (l,f,i,BQ,AQ,CQ,RM11,RM12,RM21,RM22): ',
c     &      l,f,i,BQ(i),AQ(i),CQ(i),RM11,RM12,RM21,RM22
c        endif

C        Multiplikation der Matrix [H11,H12,H21,H22] mit [I11,I22]
         B0(i)=H11*I11+H12*I22
         D0(i)=H21*I11+H22*I22
 1300 CONTINUE

C     Berechnung der Oberflaechenamplitude F0(k)
      DO 1400 k=1,2
         F0(k)=tp/(1.D0-rm*rp)*(FQ(k)+rm*EQ(k))
 1400 CONTINUE
   
 

C**********************************************************************C
C 16. Vorberechnungen fuer die Berechnung der Integrandenglieder       C
C**********************************************************************C
      In1=u*B0(0)+b(0)*D0(0)
      In2=u*B0(2)+b(0)*D0(2)
      In3=u*B0(1)+b(0)*D0(1)
      In4=a(0)*B0(0)-u*D0(0)
      In5=a(0)*B0(2)-u*D0(2)
      In6=a(0)*B0(1)-u*D0(1)
      if (typ.eq.1) then
c moment tensor source
        FFI=w(f)*w(f)/(4.D0*pi*rho(h))
      elseif (typ.eq.2) then
c single vertical force
        FFI=w(f)/(4.d0*pi*rho(h)) 
      endif

c      if (cl_debug) then
c        print *,'DEBUG (l,f,u,b,a,B0,D0): ',
c     &    l,f,u,b(0),a(0),B0(0),B0(1),B0(2),D0(0),D0(1),D0(2)
c        print *,'DEBUG (l, f, FFI, In?): ',
c     &    l,f,FFI,In1,In2,In3,In4,In5,In6
c      endif



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

      jarg=u*w(f)*r(E)
      if (((cl_hankel1).or.(cl_hankel2)).and.
     &  (jarg.gt.1.d-50).and.(u.gt.cl_hankelswitch)) then
C     Hankelfunktionen aufrufen   
        if (cl_hankel1) then
          H0=0.5d0*(d_j0(jarg)+IME*d_y0(jarg))
          H1=0.5d0*(d_j1(jarg)+IME*d_y1(jarg))
          H2=0.5d0*(d_jn(2,jarg)+IME*d_yn(2,jarg))
        else
          H0=0.5d0*(d_j0(jarg)-IME*d_y0(jarg))
          H1=0.5d0*(d_j1(jarg)-IME*d_y1(jarg))
          H2=0.5d0*(d_jn(2,jarg)-IME*d_yn(2,jarg))
        endif

c calculate integrands

        if (typ.eq.1) then

c for a moment tensor source
C     Berechnung der Fernfeld-Integrandenglieder
          hilf=H1*(K0(E)*In1+K3(E)*In2)
          IDFr= FFI*u*(K1(E)*In3*H0-hilf)
          IDFphi=-FFI*u*(F0(1)*L1(E)*H0+F0(2)*L2(E)*H1)
          hilf=H0*(K0(E)*In4+K3(E)*In5)
          IDFz=FFI*IME*u*(hilf+K1(E)*In6*H1)

C     Berechnung der Nahfeld-Integrandenglieder
          NFI=w(f)/(r(E)*4.D0*pi*rho(h))
          H11=  (F0(1)-In3)*H1
          H22=2.D0*(F0(2)-In2)*H2
          IDNr  =NFI*(K1(E)*H11+K2(E)*H22)
          IDNphi=NFI*(L1(E)*H11+L2(E)*H22)
          IDNz=NFI*IME*2.D0*K2(E)*In5*H1

        elseif (typ.eq.2) then

c for a vertical single force
          IDFr=-FFI*In1*H1
          IDFz=FFI*IME*In4*H0
c   no tangential component, no near field
          IDFphi=dcmplx(0.d0)
          IDNphi=dcmplx(0.d0)
          IDNr=dcmplx(0.d0)
          IDNz=dcmplx(0.d0)

        endif
      else
        if (cl_linesrc) then
c     quick and dirty kernel substitution for line source
c     12/10/2011 implemented radial component
c     checked: 
c       in case of vertical single force and of isotropic moment tensor
c         the vertical displacement component is expanded with J0 only
c         the radial displacement component is expanded with J1 only
c       both source types tranform identically
          J0=2.d+3*cos(jarg)/max(1.d-50,w(f)*u)
          J1=2.d+3*sign(1.d0,r(E))*sin(jarg)/max(1.d-50,w(f)*u)
          J2=0.d0
        else
C     Besselfunktionen aufrufen   
          J0=d_j0(jarg)  
          J1=d_j1(jarg)
          J2=d_jn(2,jarg) 
        endif

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
          NFI=w(f)/(r(E)*4.D0*pi*rho(h))
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
         DO 5110 f=1,SL
C           Berechnung der Reduktionszeit tred(f,E)
            tred(f,E)=t(f)+help+Tli-Tre
C           und des Reduktionsfaktors red
            red=EXP(IME*w(f)*(help+Tli-Tre))
C           Reduktion der Fernfeld-Spektren
            VFr(f,E)  =VFr(f,E)*red
            VFphi(f,E)=VFphi(f,E)*red
            VFz(f,E)  =VFz(f,E)*red
C           Reduktion der Nahfeld-Spektren
            VNr(f,E)  =VNr(f,E)*red
            VNphi(f,E)=VNphi(f,E)*red
            VNz(f,E)  =VNz(f,E)*red
 5110    CONTINUE
 5120 CONTINUE

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

      DO 7000 E=1,NE
         DO 6500 f=1,SL
C           Umschreiben auf Transformationsvariable
            C1(f)  =VFr(f,E)
            C2(f)  =VFphi(f,E)
            C3(f)  =VFz(f,E)
            C4(f)  =VNr(f,E)
            C5(f)  =VNphi(f,E)
            C6(f)  =VNz(f,E)
 6500    CONTINUE

C        Ruecktransformation in den Zeitbereich:Seismogramme
         CALL DFORK(SL,C1,rueck)
         CALL DFORK(SL,C2,rueck)
         CALL DFORK(SL,C3,rueck)
         CALL DFORK(SL,C4,rueck)
         CALL DFORK(SL,C5,rueck)
         CALL DFORK(SL,C6,rueck)                

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
            VFr(f,E)  =C1(f)*1.D0/ap
            VFphi(f,E)=C2(f)*1.D0/ap
            VFz(f,E)  =C3(f)*1.D0/ap
            VNr(f,E)  =C4(f)*1.D0/ap
            VNphi(f,E)=C5(f)*1.D0/ap
            VNz(f,E)  =C6(f)*1.D0/ap
 6600    CONTINUE
 7000 CONTINUE

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
c   99 stop 'ERROR: opening file'
c   98 stop 'ERROR: reading file'
c   97 stop 'ERROR: reading file - unexpected end of file'
c   96 stop 'ERROR: closing file'
c   95 stop 'ERROR: writing file'
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
      double precision u,uQ,rho1,rho2,rho12

      double complex  Rppd,Rpsd,Tppd,Tpsd,Rssd,Rspd,Tssd,Tspd,rd,td,
     &            Rppu,Rpsu,Tppu,Tpsu,Rssu,Rspu,Tssu,Tspu,ru,tu,
     &            betaC1,betaC2,a1,a2,b1,b2,a1b1,a2b2,a1b2,a2b1,ab,
     &            abm,rhoabm,C,C0,C1,C2,C3,C4,C5,C6,C7,C8,C9,C10,C11,
     &            mue1,mue2,mue1b1,mue2b2,muebN,muebP

C     nur fuer Einfall von oben
      double complex  D1D,D2D,DD,D1,D2,D3

C     nur fuer Einfall von unten
      double complex  D1U,D2U,DU,U1,U2,U3          



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
      double precision SC,PI,SIGNI
      double complex CX(LX),CARG,CW,CTEMP

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





C23456789012345678901234567890123456789012345678901234567890123456789012
C Subroutine fuer Matrizeninversion
C Bem: DOUBLE COMPLEX muss implementiert sein
 
      SUBROUTINE MATINV(A11,A12,A21,A22,B11,B12,B21,B22)
      double complex A11,A12,A21,A22,B11,B12,B21,B22,D
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
      double complex A11,A12,A21,A22,B11,B12,B21,B22,C11,C12,C21,C22
      C11=A11*B11+A12*B21
      C12=A11*B12+A12*B22
      C21=A21*B11+A22*B21
      C22=A21*B12+A22*B22
      RETURN
      END


