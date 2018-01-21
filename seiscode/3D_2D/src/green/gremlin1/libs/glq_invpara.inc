c this is <glq_invpara.inc>
c------------------------------------------------------------------------------
c
c $Id$
c
c 29/12/97 by Thomas Forbriger (IfG Stuttgart)
c
c parameters controlling the inversion step
c
c REVISIONS and CHANGES
c    29/12/97   V1.0   Thomas Forbriger
c    02/01/98   V1.1   provide now lq_x2release
c    05/01/98   V1.2   removed lq_x2release again
c    10/04/00   V1.3   introduced model_interval
c
c==============================================================================
c
c control improvement
      double precision lq_relimp, lq_x2lim, lq_numax, lq_numin
      integer lq_nmean, lq_msteps, lq_mindown, model_interval
c 
      common /glq_invpara/lq_relimp, lq_x2lim, lq_numin, lq_numax,
     &                 lq_nmean, lq_msteps, lq_mindown, model_interval
c
c ----- END OF glq_invpara.inc -----
