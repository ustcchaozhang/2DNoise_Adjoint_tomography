c this is <seife_common.inc>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
c
c common block to set debug mode
c
c REVISIONS and CHANGES
c    11/07/2005   V1.0   Thomas Forbriger
c
c ============================================================================
c
c NOTICE: since the linker refuses to link blockdata subroutines that are
c placed in separate object files, we have to attach the initializing
c blockdata subroutine to one of our souce code files.
c
c Decision: the blockdata statement will be placed in seife_rfk.f
c 
c debug mode is active, when .true.
      logical ldebug
c 
      common /seife_global_flags/ ldebug
c
c ----- END OF seife_common.inc ----- 
