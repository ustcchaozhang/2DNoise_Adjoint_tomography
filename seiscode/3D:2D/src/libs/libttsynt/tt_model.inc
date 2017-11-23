c this is <tt_model.inc>
c------------------------------------------------------------------------------
cS
c
c 16/09/99 by Thomas Forbriger (IfG Stuttgart)
c
c common block to hold basic model definition
c
c nlay      number of layers (including halfspace)
c v         velocity of layer in m/s
c d         thickness of layer as defined to the middle of the offset range
c           (common mid point) d gives the distance from the CMP of one
c           interface to the next interface - the plumbline defines the CMP on
c           the next interface (given in meters) - the value d is not defined
c           for the lower halfspace
c phi       defines the dip of the interface which is the angle between this
c           interface and the above one (given in rad) - the dip of the free
c           surface is given in reference to the horizontal
c backoff   is the distance between both shotpoints (given in meters)
c
c Index 1 values:
c v:        velocity of the first topmost layers
c d:        distance between the free surface and the first interface
c phi:      dip of the free surface against the horizontal
c 
c REVISIONS and CHANGES
c    16/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      integer nlay
      real d(maxlay), v(maxlay), phi(maxlay), backoff

      common /tt_model/ d,v,phi,backoff,nlay
c
c ----- END OF tt_model.inc -----
