c this is <gremlin.inc>
c------------------------------------------------------------------------------
c
c 05/12/97 by Thomas Forbriger (IfG Stuttgart)
c
c this file will be used by gremlin.f to define global data space
c
c REVISIONS and CHANGES
c    05/12/97   V1.0   Thomas Forbriger
c    10/12/97   V1.1   use direct access to common blocks
c
c==============================================================================
c
c plotting
      character*80 device
      logical doask
      common /plots/device,doask
c
c ----- END OF gremlin.inc -----
