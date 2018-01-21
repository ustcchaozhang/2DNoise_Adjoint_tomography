c this is <gr_refsub.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Some reflectivity subroutines written by J.Ungerer for his program refseis.
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
c NOTICE: the rheology model in gr_fsm is acausal
c
c REVISIONS and CHANGES
c    24/07/97   V1.0   Thomas Forbriger
c    17/11/97   V1.1   introduced subroutine gr_setmod
c    23/01/98   V1.2   introduced subroutine gr_dsetmod
c    14/04/00   V1.3   introduced subroutine gr_greenzr
c    16/10/01   V1.4   units comment
c    16/03/02   V1.5   - now: use full formula to calculate complex velocities
c                      - source amplitude was always applied at
c                        FFI-calculation and still is
c    26/03/02   V1.6   prepare code for complex frequencies
c    27/03/02   V1.7   introduced correct definition of integration parameter
c    02/04/02   V1.8   real velocity is modulus of complex velocity
c    03/04/02   V1.9   added negative sign to radial component (that matches
c                      the calculation of the radial component in refmet)
c    14/05/02   V1.10  the tabulated velocities define a real part of a
c                      modulus per density
c
c==============================================================================
c
c list of subroutines in this file:
c
c   subroutine gr_fsm:     finish a source and a model
c                             this means: calculate the layer to which the
c                             source belongs and calculate the complex
c                             velocities and calculate layer thickness
c   subroutine gr_prep:    prepare interface coefficients for one
c                             horizontal slowness component
c   subroutine gr_prepcomplex: prepare interface coefficients for
c                             complex frequencies
c                             (must be called for each frequency, if complex
c                             frequencies are used - this is done
c                             automatically by gr_green and gr_greenzr)
c   subroutine gr_rtc:     calculate the reflectivities and transmissivities
c                             for PSV waves
c   subroutine gr_phase:   set frequency and calculate phase matrix
c   subroutine gr_green:   calculate the greens coefficient
c                             this function calls gr_phase and gr_rtc
c   subroutine gr_greenzr: calculate the greens coefficients for Uz and Ur
c                             this function calls gr_phase and gr_rtc
c   subroutine gr_refmod:  read a refmet style model and source
c   subroutine gr_setmod:  set model and source parameters
c   subroutine gr_dsetmod: set model and source parameters (double precision)
c 
c----------------------------------------------------------------------
c 
c some remarks on the used units:
c
c   the model is given in units of
c      density:     g/ccm
c      velocity:    km/s
c      slowness:    s/km
c      frequency:   Hz (1/s)
c      layer depth: km
c
c   if you want to receive units appropriate for slowness in s/m
c   you will have to multiply the green coefficients by 1.e6
c
c   the refmet code integrates over a slowness in s/km thus the green
c   coefficients are too small (by a factor of 1.e6) for slowness integration
c   in s/m
c
c   the values returned by the subroutine have the unit 1.e-6 m**3/s if they
c   represent a displacement waveform in units of m
c
c   The velocity value in the model file specifies the real part of the
c   complex velocity.
c 
c======================================================================
c
c finish a model definition
c
c 26/03/2002      initialize for real frequency
c 14/05/2002      new velocity definition
c
      subroutine gr_fsm
c read common blocks
      include 'gr_source.inc'
      include 'gr_rtc.inc'
cE
c rest of variable declaration
      integer i
      double complex ime
      parameter(ime=(0.d0,1.d0))
      double precision pi
      parameter(pi=3.141592653589793115997963d0)
c go
c
c initialize for real frequency
      om_sigma=0.d0
      freq_is_complex=.false.
c
c calculate geometry coefficients
      if (src_type.eq.1) then
        src_dim=1.d-12
      elseif (src_type.eq.2) then
        src_dim=1.d-15
      else
        stop 'ERROR (gr_fsm): unknown source type'
      endif
c 
c now calculate layer thickness
      do i=0,mod_nlay-1
        mod_t(i)=mod_z(i+1)-mod_z(i)
      enddo
c 
c look for source layer index
      i=0
    1 i=i+1
      if ((mod_z(i).le.src_depth).and.(i.lt.mod_nlay)) goto 1
      src_layer=i-1
      if (src_depth.ge.mod_z(mod_nlay)) src_layer=mod_nlay
c      print *,'src_layer ',src_layer
c      print *,'src_depth ',src_depth
c      print *,'src_type ',src_type
c      print *,'src_amp ',src_amp
c
c now calculate the complex velocities
      do i=0,mod_nlay
         mod_aC(i)=mod_a(i)*sqrt(1.+ime/mod_Qa(i))
         mod_bC(i)=mod_b(i)*sqrt(1.+ime/mod_Qb(i))
      enddo
c
c do some precalculation
      FFI=src_amp*src_dim/(4.d0*pi*mod_r(src_layer))
c      print *,FFI
c 
c output
c      print *,'layer stack:'
c      do i=0,mod_nlay
c        print 50,i,mod_z(i),mod_a(i),mod_b(i),mod_r(i),mod_Qa(i),mod_Qb(i)
c      enddo
c      do i=0,mod_nlay
c        print *,i,mod_aC(i), mod_bC(i)
c      enddo
c 
      return
   50 format(i5, 6f10.3)
      end
cS 
c----------------------------------------------------------------------
c 
c prepare interface reflection coefficients for complex frequencies
c
c 26/03/2002      prepare for complex frequencies
c                 rtc_omega, om_sigma, and real_p must be set before
c 
      subroutine gr_prepcomplex
c
c read common block
      include 'gr_rtc.inc'
      include 'gr_results.inc'
cE 
c variables
      double complex h11, h22, ime
      parameter(ime=(0.d0,1.d0))
      integer i
c
c complex frequency
      rtc_omega=real_omega-ime*om_sigma
c
c phase slowness must be complex to keep wavenumber real
c see gr_rtc.inc for the definition of real_p and rtc_u.
c
c first calculate slowness scaling factor
c (will also be used to scale green coefficient - which is an implicit scaling
c of the integration parameter and the Jacoby determinant)
      cplx_scal=1./(1.d0-ime*om_sigma/real_omega)
      cplx_scalq=cplx_scal*cplx_scal
c then apply factor
      rtc_u=real_p*cplx_scal
      rtc_uq=rtc_u*rtc_u
c 
C     Berechnung der vert. Langsamkeiten aller Schichten i:
C     Der Realteil soll fuer w>0 positiv, der Imaginaerteil negativ 
C     sein.Die Wurzelfunktion CSQRT arbeitet so, dass der Realteil der
C     gezogenen Wurzel stets positiv ist und der Imaginaerteil mit dem
C     Vorzeichen versehen wird,das der Imaginaerteil des Arguments hat.
C     Weil der Imaginaerteil des Arguments zur Bildung der vertikalen
C     Langsamkeiten negativ ist, wird obige Forderung erfuellt.
      DO i=0,mod_nlay
         H11= 1.D0/(mod_aC(i)*mod_aC(i))
         H22= 1.D0/(mod_bC(i)*mod_bC(i))
         vsa(i) = SQRT(H11-rtc_uQ)
         vsb(i) = SQRT(H22-rtc_uQ)
      ENDDO

C     Berechnung er Reflektions und Transmissionskoeffizienten fuer
C     die Trennflaeche i+1 (Aufruf von RTKC)
      DO i=0,(mod_nlay-1)
         CALL gr_RTKC(rtc_u,vsa(i),vsb(i),mod_bC(i),mod_r(i),
     &            vsa(i+1),vsb(i+1),mod_bC(i+1),mod_r(i+1),
     &            Rppd(i+1),Rpsd(i+1),Tppd(i+1),Tpsd(i+1),Rssd(i+1),
     &            Rspd(i+1),Tssd(i+1),Tspd(i+1),
     &            Rppu(i+1),Rpsu(i+1),Tppu(i+1),Tpsu(i+1),Rssu(i+1),
     &            Rspu(i+1),Tssu(i+1),Tspu(i+1))
      ENDDO
      return
      end
cS 
c----------------------------------------------------------------------
c 
c prepare interface reflection coefficients for one slowness
c
c 26/03/2002      prepare for complex frequencies
c 
      subroutine gr_prep(u)
c 
c parameters
      double precision u
c
c read common block
      include 'gr_rtc.inc'
cE 
c variables
      double complex h11, h22
      integer i
c
c remember slowness
      real_p=u
c 
c complex and real slowness parameter are equal for sigma=0
c set this to be the default
      rtc_u=u
      rtc_uq=u*u
c 
C     Berechnung der vert. Langsamkeiten aller Schichten i:
C     Der Realteil soll fuer w>0 positiv, der Imaginaerteil negativ 
C     sein.Die Wurzelfunktion CSQRT arbeitet so, dass der Realteil der
C     gezogenen Wurzel stets positiv ist und der Imaginaerteil mit dem
C     Vorzeichen versehen wird,das der Imaginaerteil des Arguments hat.
C     Weil der Imaginaerteil des Arguments zur Bildung der vertikalen
C     Langsamkeiten negativ ist, wird obige Forderung erfuellt.
      DO i=0,mod_nlay
         H11= 1.D0/(mod_aC(i)*mod_aC(i))
         H22= 1.D0/(mod_bC(i)*mod_bC(i))
         vsa(i) = SQRT(H11-rtc_uQ)
         vsb(i) = SQRT(H22-rtc_uQ)
      ENDDO

C     Berechnung er Reflektions und Transmissionskoeffizienten fuer
C     die Trennflaeche i+1 (Aufruf von RTKC)
      DO i=0,(mod_nlay-1)
         CALL gr_RTKC(rtc_u,vsa(i),vsb(i),mod_bC(i),mod_r(i),
     &            vsa(i+1),vsb(i+1),mod_bC(i+1),mod_r(i+1),
     &            Rppd(i+1),Rpsd(i+1),Tppd(i+1),Tpsd(i+1),Rssd(i+1),
     &            Rspd(i+1),Tssd(i+1),Tspd(i+1),
     &            Rppu(i+1),Rpsu(i+1),Tppu(i+1),Tpsu(i+1),Rssu(i+1),
     &            Rspu(i+1),Tssu(i+1),Tspu(i+1))
      ENDDO
      return
      end
cS
c----------------------------------------------------------------------
c 
c calculate phase matrix and set frequency on the fly
c
      subroutine gr_phase(omega)
c
c parameters
      double precision omega
c 
c read in common blocks
      include 'gr_rtc.inc'
      include 'gr_results.inc'
cE 
c variables
      double complex help
      integer i
c 
c remember angular frequency
      real_omega=omega
c
c work on complex frequencies if used
      if (freq_is_complex) then
        call gr_prepcomplex
      else
        rtc_omega=real_omega
      endif
c 
      do i=0,mod_nlay-1
        help=(0.d0,-1.d0)*mod_t(i)*rtc_omega
        E11(i)=exp(vsa(i)*help)
        E22(i)=exp(vsb(i)*help)
      enddo
c
      return
      end
cS 
c----------------------------------------------------------------------
c 
c calculate refelctivities and transmissivites for PSV waves in a layer stack
c
      subroutine gr_rtc
c 
c read in common blocks
      include 'gr_source.inc'
      include 'gr_rtc.inc'
      include 'gr_results.inc'
cE 
c variables
      double complex H11, H12, H21, H22
      double complex I11, I12, I21, I22
      integer i
c 
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
      RTD11(mod_nlay)=(0.,0.)
      RTD12(mod_nlay)=(0.,0.)
      RTD21(mod_nlay)=(0.,0.)
      RTD22(mod_nlay)=(0.,0.)

C     Rekursion von Schicht n-1 bis h  
      DO i=mod_nlay-1,src_layer,-1

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

      enddo
C     Ende Rekursion fuer RTD und rtd 

C     Reflektivitaeten RM(inus) und rm(inus) der Herdschicht
      RM11=RTD11(src_layer)
      RM12=RTD12(src_layer)
      RM21=RTD21(src_layer)
      RM22=RTD22(src_layer)

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

      TTUo11(0)=(1.,0.)
      TTUo12(0)=(0.,0.)
      TTUo21(0)=(0.,0.)
      TTUo22(0)=(1.,0.)

C     Rekursion von Schicht 0 bis Schicht h-1
      DO i=0,src_layer-1

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
 
      enddo
C     Ende der Rekursion fuer RTU,TTUO,rtu,ttuo

C     Reflektivitaeten RP(lus),rp(lus) der Herdschicht
      RP11 = RTU11(src_layer)
      RP12 = RTU12(src_layer)
      RP21 = RTU21(src_layer)
      RP22 = RTU22(src_layer)
   
C     Transmissivitaeten TP(lus),tp(lus) der Herdschicht 
      TP11 = TTUo11(src_layer)
      TP12 = TTUo12(src_layer)
      TP21 = TTUo21(src_layer)
      TP22 = TTUo22(src_layer)
c
      return
      end
cS
c----------------------------------------------------------------------
c
c calculate greens coefficient for vertical component for
c u_z_total(omega,r)=int_0^infty u_z(u,omega) J_0(p*omega*r) p dp
c
c and greens coefficient for radial component:
c u_r_total(omega,r)=int_0^infty u_r(u,omega) J_1(p*omega*r) p dp
c
c this function returns u_z(p,omega) in greenz
c                   and u_r(p,omega) in greenr
c where p and omega where set in the preparation history
c
c p is the phase slowness
c omega is the angular frequency
c
      subroutine gr_greenzr(omega, greenz, greenr)
c 
c parameter
      double precision omega
      double complex greenz, greenr
c 
c For an explosion: K0=K3=1.   K1=K2=L1=L2=0.
c 
      include 'gr_source.inc'
      include 'gr_rtc.inc'
      include 'gr_results.inc'
cE 
c variables
      double complex ea, eb, help
      double complex H11, H12, H21, H22, I11, I12, I21, I22
c 
c prepare phase matrix and go through layer stack
      call gr_phase(omega)
      call gr_rtc
c 
c calculate source amplitudes
c ===========================
c
c do some precalculation
      help=(0.d0,1.d0)*rtc_omega*(src_depth-mod_z(src_layer))
      ea=exp(vsa(src_layer)*help)
      eb=exp(vsb(src_layer)*help)
c      print *,ea,eb
c 
c vertical single force
      if (src_type.eq.1) then
        aq0=ea
        bq0=-1.d0/ea
        cq0=rtc_u/vsb(src_layer)
        dq0=cq0/eb
        cq0=cq0*eb
c 
c explosion
      elseif (src_type.eq.2) then
        aq0=(0.d0,1.d0)*rtc_omega*(vsa(src_layer)+rtc_uq/vsa(src_layer))
        bq0=aq0/ea
        aq0=aq0*ea
        cq0=(0.d0,0.d0)
        dq0=(0.d0,0.d0)
c
c unkown source type
      else
        stop 'ERROR (gr_green): unknown source type'
      endif
c
c calculate surface amplitudes
c ============================
c 
c now apply layers
      CALL MATMUL(RM11,RM12,RM21,RM22,RP11,RP12,RP21,RP22,
     &            H11,H12,H21,H22)
      H11=1.D0-H11
      H12=  -H12
      H21=  -H21
      H22=1.D0-H22
      CALL MATINV(H11,H12,H21,H22,I11,I12,I21,I22)
      CALL MATMUL(TP11,TP12,TP21,TP22,I11,I12,I21,I22,
     &            H11,H12,H21,H22)
      I11=BQ0+RM11*AQ0+RM12*CQ0
      I22=DQ0+RM21*AQ0+RM22*CQ0
      B0=H11*I11+H12*I22
      D0=H21*I11+H22*I22
c 
c calculate displacement
c ======================
c
c 
      GREEN_Z=FFI*rtc_omega*(0.d0,1.d0)*(vsa(0)*B0-rtc_u*D0)
      GREEN_R=-FFI*rtc_omega*(rtc_u*B0+vsb(0)*D0)

c scale result by complex slowness scaling factor
c (which is an implicit scaling of the integration parameter and the Jacoby
c determinant)
      if (freq_is_complex) then
        GREEN_Z=GREEN_Z*cplx_scalq
        GREEN_R=GREEN_R*cplx_scalq
      endif

      greenz=GREEN_Z
      greenr=GREEN_R
c 
      return
      end
cS
c----------------------------------------------------------------------
c
c calculate greens coefficient for vertical component for
c
c u_z_total(omega,r)=int_0^infty u_z(u,omega) J_0(u*omega*r) u du
c
c this function returns u_z(u,omega)
c where u and omega where set in the preparation history
c
      subroutine gr_green(omega, green)
c 
c parameter
      double precision omega
      double complex green
c 
c For an explosion: K0=K3=1.   K1=K2=L1=L2=0.
c 
      include 'gr_source.inc'
      include 'gr_rtc.inc'
      include 'gr_results.inc'
cE 
c variables
      double complex ea, eb, help
      double complex H11, H12, H21, H22, I11, I12, I21, I22
c 
c prepare phase matrix and go through layer stack
      call gr_phase(omega)
      call gr_rtc
c 
c calculate source amplitudes
c ===========================
c
c do some precalculation
      help=(0.d0,1.d0)*rtc_omega*(src_depth-mod_z(src_layer))
      ea=exp(vsa(src_layer)*help)
      eb=exp(vsb(src_layer)*help)
c      print *,ea,eb
c 
c vertical single force
      if (src_type.eq.1) then
        aq0=ea
        bq0=-1.d0/ea
        cq0=rtc_u/vsb(src_layer)
        dq0=cq0/eb
        cq0=cq0*eb
c 
c explosion
      elseif (src_type.eq.2) then
        aq0=(0.d0,1.d0)*rtc_omega*(vsa(src_layer)+rtc_uq/vsa(src_layer))
        bq0=aq0/ea
        aq0=aq0*ea
        cq0=(0.d0,0.d0)
        dq0=(0.d0,0.d0)
c
c unkown source type
      else
        stop 'ERROR (gr_green): unknown source type'
      endif
c
c calculate surface amplitudes
c ============================
c 
c now apply layers
      CALL MATMUL(RM11,RM12,RM21,RM22,RP11,RP12,RP21,RP22,
     &            H11,H12,H21,H22)
      H11=1.D0-H11
      H12=  -H12
      H21=  -H21
      H22=1.D0-H22
      CALL MATINV(H11,H12,H21,H22,I11,I12,I21,I22)
      CALL MATMUL(TP11,TP12,TP21,TP22,I11,I12,I21,I22,
     &            H11,H12,H21,H22)
      I11=BQ0+RM11*AQ0+RM12*CQ0
      I22=DQ0+RM21*AQ0+RM22*CQ0
      B0=H11*I11+H12*I22
      D0=H21*I11+H22*I22
c 
c calculate displacement
c ======================
c
c 
      GREEN_Z=FFI*rtc_omega*(0.d0,1.d0)*(vsa(0)*B0-rtc_u*D0)

c scale result by complex slowness scaling factor
c (which is an implicit scaling of the integration parameter and the Jacoby
c determinant)
      if (freq_is_complex) then
        GREEN_Z=GREEN_Z*cplx_scalq
      endif

      green=GREEN_Z
c 
      return
      end
cS
c----------------------------------------------------------------------
c
      subroutine gr_refmod(modelname, sourcename)
c
c parameters
      character modelname*(*)
      character sourcename*(*)
c
c common blocks
      include 'gr_model.inc'
      include 'gr_source.inc'
cE
c variables
      real inqa(0:mod_mlay), inqb(0:mod_mlay)
      character*80 infotext, junk
      double precision radius, nuref, Thd, The, M1,M2,M3,M4,M5,M6
      integer i, typ, outsig, srcsig
c 
c go model
      call refmet_rmod(modelname, infotext, mod_mlay, 
     &   nuref, radius, mod_nlay,
     &   mod_z, mod_a, mod_b, mod_r, inqa, inqb, 2, .false., 1)
c 
      print *,infotext
c
      if (radius.gt.0.d0) stop 'ERROR (gr_refmod): only flat models!'
      if (nuref.gt.0.d0) stop 'ERROR (gr_refmod): no dispersion!'
c 
      do i=0,mod_nlay
        mod_Qa(i)=inqa(i)
        mod_Qb(i)=inqb(i)
      enddo
c 
c go source
      call refmet_rsource(sourcename, infotext, junk,
     &   typ, outsig, srcsig, The, Thd, src_depth, src_amp,
     &   M1, M2, M3, M4, M5, M6, 2, 1, .false.)
c 
      print *,infotext
c 
      if (typ.eq.1) then
        src_type=2
        print *,'NOTICE: assuming explosion source'
      elseif (typ.eq.2) then
        src_type=1
      else
        stop 'ERROR (gr_refmod): unknown source type'
      endif
c
      call gr_fsm
c 
      return
      end
cS
c----------------------------------------------------------------------
c
      subroutine gr_setmod(depth, alpha, beta, density, Qalpha, Qbeta,
     &  s_type, s_depth, s_amp, nlay)
c
c This subroutine sets model for subsequent calculation of greens
c coeficients. This routines calls gr_fsm before returning.
c
c nlay:         number of layers described by model
c depth(i):     top of layer i in (kilometers)
c alpha(i):     Vp layer velocity (kilometers/second)
c beta(i):      Vs layer velocity (kilometers/second)
c density(i):   layer density (g/ccm)
c Qalpha(i):    Vp quality factor
c Qbeta(i):     Vs quality factor
c
c i=nlay will be the bottom halfspace parameters
c i=1    will be the top halfspace
c
c s_depth:      source depth in kilometer
c s_type:       source type: 1=vertical single force   2=explosion
c s_amp:        source amplitude in N (force) or N*m (explosion)
c
c parameters
      integer nlay
      real depth(nlay), alpha(nlay), beta(nlay), density(nlay)
      real Qalpha(nlay), Qbeta(nlay)
      integer s_type
      real s_depth, s_amp
c
c common blocks
      include 'gr_model.inc'
      include 'gr_source.inc'
cE
c
      integer i
c
c check ranges
      if ((nlay-1).gt.mod_mlay) then
        print *,'ERROR (gr_setmod): you want to use ',nlay,' layers'
        print *,'ERROR (gr_setmod): we provide only ',mod_mlay,' layers'
        stop 'ERROR (gr_setmod): too many layers'
      endif
      if (nlay.lt.3) stop 'ERROR (gr_setmod): too few layers'
c
c go and copy model to common block
c
      mod_nlay=nlay-1
      do i=1,nlay
        mod_a(i-1)=alpha(i)
        mod_b(i-1)=beta(i)
        mod_r(i-1)=density(i)
        mod_Qa(i-1)=Qalpha(i)
        mod_Qb(i-1)=Qbeta(i)
      enddo
c
      do i=1,mod_nlay
        mod_z(i)=depth(i+1)
      enddo
      mod_z(0)=mod_z(1)
c
      src_depth=s_depth
      src_amp=s_amp
      src_type=s_type
c
c set rest of parameters (finish model)
      call gr_fsm
c 
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine gr_dsetmod(depth, alpha, beta, density, Qalpha, Qbeta,
     &  s_type, s_depth, s_amp, nlay)
c
c This subroutine sets model for subsequent calculation of greens
c coeficients. This routines calls gr_fsm before returning.
c 
c double precision version
c
c nlay:         number of layers described by model
c depth(i):     top of layer i in (kilometers)
c alpha(i):     Vp layer velocity (kilometers/second)
c beta(i):      Vs layer velocity (kilometers/second)
c density(i):   layer density (g/ccm)
c Qalpha(i):    Vp quality factor
c Qbeta(i):     Vs quality factor
c
c i=nlay will be the bottom halfspace parameters
c i=1    will be the top halfspace
c
c s_depth:      source depth in kilometer
c s_type:       source type: 1=vertical single force   2=explosion
c s_amp:        source amplitude in N (force) or N*m (explosion)
c
c parameters
      integer nlay
      double precision depth(nlay), alpha(nlay), beta(nlay), density(nlay)
      double precision Qalpha(nlay), Qbeta(nlay)
      integer s_type
      double precision s_depth, s_amp
c
c common blocks
      include 'gr_model.inc'
      include 'gr_source.inc'
cE
c
      integer i
c
c check ranges
      if ((nlay-1).gt.mod_mlay) then
        print *,'ERROR (gr_setmod): you want to use ',nlay,' layers'
        print *,'ERROR (gr_setmod): we provide only ',mod_mlay,' layers'
        stop 'ERROR (gr_setmod): too many layers'
      endif
      if (nlay.lt.3) stop 'ERROR (gr_setmod): too few layers'
c
c go and copy model to common block
c
      mod_nlay=nlay-1
      do i=1,nlay
        mod_a(i-1)=alpha(i)
        mod_b(i-1)=beta(i)
        mod_r(i-1)=density(i)
        mod_Qa(i-1)=Qalpha(i)
        mod_Qb(i-1)=Qbeta(i)
      enddo
c
      do i=1,mod_nlay
        mod_z(i)=depth(i+1)
      enddo
      mod_z(0)=mod_z(1)
c
      src_depth=s_depth
      src_amp=s_amp
      src_type=s_type
c
c set rest of parameters (finish model)
      call gr_fsm
c 
      return
      end
c
c ----- END OF gr_refsub.f -----
