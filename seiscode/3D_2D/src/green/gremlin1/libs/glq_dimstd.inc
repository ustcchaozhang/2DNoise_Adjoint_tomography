c this is <glq_dim.inc>
c------------------------------------------------------------------------------
cS
c $Id$
c
c 28/11/97 by Thomas Forbriger (IfG Stuttgart)
c
c define array dimensions for least squares algorithm
c
c REVISIONS and CHANGES
c    28/11/97   V1.0   Thomas Forbriger
c    25/03/98   V1.1   detach glqm_mano from other parameters
c                      (limit must now be controlled by subroutines)
c
c==============================================================================
c
c define model space
c
c glqm_msec:   maximum number of model sections
c glqm_mpol:   maximum order of model polynomials (fixed to 3!)
c glqm_mpar:   maximum number of model parameters without depth (fixed to 5!)
c glqm_cpar:   maximum number of parameters for chopped model array
c glqm_mlay:   maximum number of model layers (chopped version)
c 
c glqm_mano:   maximum number of anonymous parameters
c
c glqm_mmod:   maximum number of named models to hold
c 
      integer glqm_msec, glqm_mpol, glqm_mpar, glqm_mano, glqm_cpar
      parameter(glqm_msec=10, glqm_mpol=3, glqm_mpar=5, glqm_cpar=glqm_mpar+1)
c      parameter(glqm_mano=glqm_msec*glqm_mpol*glqm_mpar)
      parameter(glqm_mano=glqm_msec*8)
      integer glqm_mmod, glqm_mlay
      parameter(glqm_mmod=2, glqm_mlay=100)
c 
c define model box
      integer mb_ref, mb_work
      parameter(mb_ref=1, mb_work=2)
c 
c define model index
c
      integer mi_alpha, mi_beta, mi_density
      integer mi_Qalpha, mi_Qbeta, mi_depth
      parameter(mi_alpha=1)
      parameter(mi_beta=2)
      parameter(mi_density=3)
      parameter(mi_Qalpha=4)
      parameter(mi_Qbeta=5)
      parameter(mi_depth=6)
c 
c define data space
c
c glqd_mslo:   maximum number of slowness values
c glqd_mfre:   maximum number of frequency values
c glqd_mtts:   maximum number of travel time samples
c
c glqd_mano:   maximum number of anonymous data points
c 
c glqd_mdat:   maximum number of named datasets to hold
c                 1. read original data
c                 2. read modified data
c                 3. modified reference model data
c                 4. modified calculated data
c 
      integer glqd_mdat
      parameter(glqd_mdat=4)
c 
      integer di_read, di_mread, di_mref, di_mcalc
      parameter(di_read=1)
      parameter(di_mread=2)
      parameter(di_mref=3)
      parameter(di_mcalc=4)
c
      integer glqd_mslo, glqd_mfre, glqd_mtts, glqd_mano
      parameter(glqd_mslo=120, glqd_mfre=300, glqd_mtts=400)
      parameter(glqd_mano=glqd_mslo*glqd_mfre+glqd_mtts)
c 
c number of points to remember for history
      integer glqm_mpts
      parameter(glqm_mpts=50)
c
cE
c ----- END OF glq_dim.inc -----
