c this is <glq_para.inc>
c------------------------------------------------------------------------------
cS
c $Id$
c
c 28/11/97 by Thomas Forbriger (IfG Stuttgart)
c
c define parameters to control routine behaviour
c
c REVISIONS and CHANGES
c    28/11/97   V1.0   Thomas Forbriger
c    29/12/97   V1.1   removed all inversion related stuff
c    30/12/97   V1.2   changed logical doprefit to integer prefit_mode
c    27/01/98   V1.3   introduced travel time prefit (allow time offset)
c    07/04/98   V1.4   introduced green amplitude error
c    13/01/99   V1.5   canceled chop_hs (see glq_model.inc)
c    07/04/00   V1.6   introduce data modification parameters:
c                      * conthresh  contrast threshold
c                      * conord     contrast order
c                      * polyord    polynomial trend order
c    09/04/00   V1.7   now use conord1 and conord2
c    11/04/00   V1.8   introduced vptrackfactor
c    02/06/00   V1.9   introduced mweightcondition
c
c==============================================================================
c
c range in the greens to use
      integer rng_smin, rng_smax, rng_fmin, rng_fmax
      double precision val_smin, val_smax, val_fmin, val_fmax
c
c travel time range
c given in km
      integer rng_xmax
      double precision val_xmax
c 
c feasible parameter ranges
      double precision rng_mmin(glqm_mpar), rng_mmax(glqm_mpar)
c 
c mode to evaluate data in (see dat_dcpc)
      integer datamode
c how to stretch log-dynamic-reducer (see dat_dcpc)
      double precision logstretch
c polynomial order for trend removal (see dat_dcpc)
      integer polyord
c contrast increasing order (see dat_dcpc)
      double precision conord1, conord2
c contrast increasing threshold (see dat_dcpc)
      double precision conthresh
c 
c balance between green and travel time data
      double precision balance
c 
c if all travel time errors equal tterror and all errors in the
c green amplitude equal gerror the misfit will be exactly 1.
c
c travel time error to be counted as 1. (in seconds)
      double precision tterror
c
c green amplitude error to be counted as 1.
      double precision gerror
c 
c switch prefitting complex amplitudes
      integer prefit_mode
c 
c travel time prefit
      integer dottprefit
c 
c travel times from discrete model with thin layers (dat_dctt)
c give number of desired layers or set to zero (switched off)
      integer chop_finett
c 
c relative model chopping stepsize
      double precision chop_step
c
c if vptrackfactor is positive it will be used to create a vp model that is
c exactly vp=vp*vptrackfactor each time mod_parcor is called
      double precision vptrackfactor
c
c mweightcondition defines which parameters are to be optimized within a
c resolution analysis. In resolution analysis it might be desirable not to
c optimize defined polynomial orders. If mweightmax is the largest mweight
c value than only a parameter with (mweightcondition*mweight)>mweightmax
c will be optimized in the analysis
      double precision mweightcondition
c
c model chopping depth of halfspace
c given in meters (not km!)
c      double precision chop_hs
c 
c master limit for model chopping
c only parameters up to masterlimit will determine the splitting into
c homogeneous layers
      integer chop_master
c 
c source parameters
      double precision src_depth, src_amp
      integer src_type
c 
c relative parameter variation for partial derivatives approxiamtion
      double precision pvar_pdev
c 
c help space to set anonymous parameter weights
      double precision para_mweights(glqm_mpol, glqm_mpar, glqm_msec)
      double precision para_mdweights(glqm_msec)
c 
      common /glq_para/para_mweights, para_mdweights, mweightcondition
      common /glq_parammi/ rng_mmin, rng_mmax,
     &                 rng_xmax, rng_smin, rng_smax, rng_fmin, rng_fmax
      common /glq_parammv/ val_smin, val_smax, val_fmin, val_fmax,
     &                 val_xmax
      common /glq_paradp / pvar_pdev,
     &                 logstretch, balance,
     &                 tterror, gerror, 
     &                 conthresh,conord1,conord2,
c     &                 chop_hs,
     &                 chop_step, vptrackfactor,
     &                 src_depth, src_amp, chop_master,
     &                 polyord
      common /glq_paraint / src_type,
     &                 datamode, prefit_mode, dottprefit, chop_finett
c
cE
c ----- END OF glq_para.inc -----
