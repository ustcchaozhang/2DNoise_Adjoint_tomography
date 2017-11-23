c this is <glq_mseiscorr.inc.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c correction values for mseisfk - removes offset due to acoustic wave
c
c REVISIONS and CHANGES
c    04/07/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      logical msc_apply
      double complex msc_green(glqd_mslo, glqd_mfre)

      common /ms_corr/ msc_green, msc_apply
c
c ----- END OF glq_mseiscorr.inc.f ----- 
