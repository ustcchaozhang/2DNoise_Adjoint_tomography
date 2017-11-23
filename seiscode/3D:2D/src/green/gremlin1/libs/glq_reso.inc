c this is <glq_reso.inc>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c common block for shared work in resolution analysis
c
c   parselect:          flags indicating whether a parameter took part in
c                       optimization
c                       (see res_opt.f)
c
c REVISIONS and CHANGES
c    03/05/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      logical parselect(glqm_mano)
      common /glq_reso/parselect
c
c ----- END OF glq_reso.inc ----- 
