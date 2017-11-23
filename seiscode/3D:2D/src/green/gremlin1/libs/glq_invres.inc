c this is <glq_invres.inc>
c------------------------------------------------------------------------------
c
c 29/12/97 by Thomas Forbriger (IfG Stuttgart)
c
c variables to transfer results from inversion steps
c
c REVISIONS and CHANGES
c    29/12/97   V1.0   Thomas Forbriger
c
c==============================================================================
c
c return values for inversion routine
      real best_nu, small_nu, best_x2
      logical found_best, lq_moderror, lq_inverror, lq_notimprove
c
c control improvement
      real lq_x2ref
c 
c something we like to have for visualization
      real lq_parimp(glqm_mpts), lq_x2imp(glqm_mpts)
      integer lq_npts
c 
      common/glq_history/lq_parimp, lq_x2imp,
     &               lq_npts
c 
      common /glq_invres/lq_moderror, lq_inverror, lq_notimprove,
     &                 best_nu, small_nu, found_best,
     &                 lq_x2ref, best_x2
c
c ----- END OF glq_invres.inc -----
