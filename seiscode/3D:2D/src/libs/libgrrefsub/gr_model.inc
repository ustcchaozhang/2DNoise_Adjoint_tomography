c this is <gr_model.inc>
c------------------------------------------------------------------------------
c
c 24/07/97 by Thomas Forbriger (IfG Stuttgart)
c
c This file contains a common block definition to hold information on the
c layered halfspace model
c
c REVISIONS and CHANGES
c    24/07/97   V1.0   Thomas Forbriger
c
c==============================================================================
c
c get parameter declarations
      include 'gr_dim.inc'
c
c mod_nlay:     number of layers described by model
c mod_z:        top of layer in (kilometers)
c mod_t:        thickness of layer in (kilometers)
c mod_a:        Vp layer velocity (kilometers/second)
c mod_b:        Vs layer velocity (kilometers/second)
c mod_r:        layer density (g/ccm)
c mod_Qa:       Vp quality factor
c mod_Qb:       Vs quality factor
c mod_aC:       complex Vp layer velocity
c mod_bC:       complex Vs layer velocity
c
      integer mod_nlay
      double precision mod_z(0:mod_mlay), mod_t(0:mod_mlay)
      double precision mod_a(0:mod_mlay), mod_b(0:mod_mlay)
      double precision mod_r(0:mod_mlay)
      double complex mod_aC(0:mod_mlay), mod_bC(0:mod_mlay)
      double precision mod_Qa(0:mod_mlay), mod_Qb(0:mod_mlay)
      common /gr_model/mod_aC, mod_bC, mod_z, mod_a, mod_b, mod_r, 
     &              mod_Qa, mod_Qb, mod_t,
     &              mod_nlay
c
c ----- END OF gr_model.inc -----
