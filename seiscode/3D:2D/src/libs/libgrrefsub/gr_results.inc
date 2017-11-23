c this is <gr_results.inc>
c------------------------------------------------------------------------------
c $Id$
c
c 29/07/97 by Thomas Forbriger (IfG Stuttgart)
c
c common block to hold intermediate results for one frequency
c
c IMPORTANT!
c include gr_model.inc first!
c 
c REVISIONS and CHANGES
c    29/07/97   V1.0   Thomas Forbriger
c    26/03/02   V1.1   changed to use complex frequencies
c    28/03/02   V1.2   hold scaling factor for complex frequencies
c
c==============================================================================
c 
c valid frequency
      double complex rtc_omega
c
c real part of frequency
      double precision real_omega
c
c slowness scaling factor when using complex frequencies
      double complex cplx_scal, cplx_scalq
c
c reflectivities and transmissivities of layer stacks
      double complex RP11, RP12, RP21, RP22
      double complex RM11, RM12, RM21, RM22
      double complex TP11, TP12, TP21, TP22
c
c source amplitudes
      double complex Aq0,Bq0,Cq0,Dq0
c
c surface amplitudes
      double complex B0,D0
c
c phase matrix
      double complex E11(0:mod_mlay), E22(0:mod_mlay)
c
c the greens coefficient for the vertical displacement
      double complex GREEN_Z
c the greens coefficient for the radial displacement
      double complex GREEN_R
c
c here we hold the layer matrix stack
      double complex
     &            RTD11(0:mod_mlay),RTD12(0:mod_mlay),
     &            RTD21(0:mod_mlay),RTD22(0:mod_mlay),
     &            RBU11(0:mod_mlay),RBU12(0:mod_mlay),
     &            RBU21(0:mod_mlay),RBU22(0:mod_mlay),
     &            RTU11(0:mod_mlay),RTU12(0:mod_mlay),
     &            RTU21(0:mod_mlay),RTU22(0:mod_mlay),
     &            TTUo11(0:mod_mlay),TTUo12(0:mod_mlay),
     &            TTUo21(0:mod_mlay),TTUo22(0:mod_mlay),
     &            S11(0:mod_mlay),S12(0:mod_mlay),
     &            S21(0:mod_mlay),S22(0:mod_mlay)
c
c common blocks
      common /gr_intermediate/ RP11, RP12, RP21, RP22,
     &                      RM11, RM12, RM21, RM22,
     &                      TP11, TP12, TP21, TP22,
     &                      B0,D0,
     &                      Aq0,Bq0,Cq0,Dq0,
     &                      GREEN_Z, E11, E22, GREEN_R,
     &                      cplx_scal, cplx_scalq,
     &                      rtc_omega, real_omega
c 
      common /gr_stack/ RTD11, RTD12, RTD21, RTD22,
     &               RBU11, RBU12, RBU21, RBU22,
     &               RTU11, RTU12, RTU21, RTU22,
     &               TTUo11, TTUo12, TTUo21, TTUo22,
     &               S11, S12, S21, S22
c
c ----- END OF gr_results.inc -----
