c this is <glq_inv.inc>
c------------------------------------------------------------------------------
c
c 28/11/97 by Thomas Forbriger (IfG Stuttgart)
c
c all we need to perform the least squares fit
c
c REVISIONS and CHANGES
c    28/11/97   V1.0   Thomas Forbriger
c    29/11/97   V1.1   moved history variables to glq_invres.inc
c    08/04/98   V1.2   changed to use the correct real form
c
c==============================================================================
c
      double complex lq_d(glqd_mano, glqm_mano)
      double complex lq_dss(glqd_mano, glqm_mano)
      double complex lq_dssd(glqm_mano, glqm_mano)
      double complex lq_dssdelta(glqm_mano)
c      double complex lq_mestim(glqm_mano)
c      double complex lq_dssd_nuww(glqm_mano, glqm_mano)
c      double precision lq_dssd(glqm_mano, glqm_mano)
c      double precision lq_dssdelta(glqm_mano)
      double precision lq_mestim(glqm_mano)
      double precision lq_dssd_nuww(glqm_mano, glqm_mano)
c      complex lq_d(glqd_mano, glqm_mano)
c      complex lq_dss(glqd_mano, glqm_mano)
c      complex lq_dssd(glqm_mano, glqm_mano)
c      complex lq_dssdelta(glqm_mano)
c      complex lq_mestim(glqm_mano)
c      complex lq_dssd_nuww(glqm_mano, glqm_mano)
c 
      common/glq_invers/lq_d, lq_dss, lq_dssd, lq_dssdelta,
     &               lq_mestim, lq_dssd_nuww
c
c ----- END OF glq_inv.inc -----
