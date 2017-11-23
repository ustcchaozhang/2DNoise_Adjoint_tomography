c this is <gr_rtc.inc>
c------------------------------------------------------------------------------
c $Id$
c
c 24/07/97 by Thomas Forbriger (IfG Stuttgart)
c
c This file defines a common block to hold precalculated reflection and
c transmission coefficients for PSV waves.
c
c IMPORTANT!
c This file includes the model definition.
c
c REVISIONS and CHANGES
c    24/07/97   V1.0   Thomas Forbriger
c    26/03/02   V1.1   changed to complex version of slowness
c    27/03/02   V1.2   introduced correct definition of integration parameter
c
c==============================================================================
c
c include the model common block to know how large the dimension should be
      include 'gr_model.inc'
c
c these are the precalculated coefficients:
      double complex
     &            Rppd(mod_mlay),Rpsd(mod_mlay),Rssd(mod_mlay),Rspd(mod_mlay),
     &            Tppd(mod_mlay),Tpsd(mod_mlay),Tssd(mod_mlay),Tspd(mod_mlay),
     &            Rppu(mod_mlay),Rpsu(mod_mlay),Rssu(mod_mlay),Rspu(mod_mlay),
     &            Tppu(mod_mlay),Tpsu(mod_mlay),Tssu(mod_mlay),Tspu(mod_mlay)
c
c vertical slowness components
      double complex vsa(0:mod_mlay), vsb(0:mod_mlay)
c
c and here wo hold the phase slowness (seconds/kilometer) related to them:
      double complex rtc_u, rtc_uq
c
c real slowness
c this is defined to be
c
c   real_p = rtc_u * (1 + ime*om_sigma/real_omega)
c
c where ime is the imaginary unit.
c real_p is used as integration parameter in the slowness integral. It is the
c parameter passed to gr_prep.
c
      double precision real_p
c
c imaginary part of frequency and flag which tells that we use it
      double precision om_sigma
      logical freq_is_complex
c
c and now the common block:
      common /gr_rtcoefficients/Rppd,Rpsd,Rssd,Rspd,Tppd,Tpsd,Tssd,Tspd,
     &            Rppu,Rpsu,Rssu,Rspu,Tppu,Tpsu,Tssu,Tspu,
     &            rtc_u, rtc_uq, vsa, vsb, om_sigma, real_p, freq_is_complex
c
c ----- END OF gr_rtc.inc -----
