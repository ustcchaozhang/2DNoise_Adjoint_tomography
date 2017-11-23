c this is <tt_work.inc>
c------------------------------------------------------------------------------
cS
c $Id$
c
c 16/09/99 by Thomas Forbriger (IfG Stuttgart)
c
c all additional (offset-independent) values calculated from model
c
c REVISIONS and CHANGES
c    16/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c 
c cmpx,cmpz:      coordinate of relative CMP on interface
c cmp:            projected CMP length on interface
c d1,d2:          relative orthogonal depth below shotpoints
c absphi:         absolute dip angle
c sapp1,sapp2:    apparent slowness at the surface for both shooting
c                 directions
c ti1,ti2:        intercept times 
c
c traveltimes are: T(x)=ti+x*sapp
c 
c absolute coordinates are always relative to CMP at the free surface
c
      real cmpx(maxlay), cmpz(maxlay),cmp(maxlay)
      real d1(maxlay), d2(maxlay), absphi(maxlay)
      real sapp1(maxlay),sapp2(maxlay)
      real ti1(maxlay),ti2(maxlay)

      common /tt_work/ cmpx,cmpz,cmp,d1,d2,absphi,
     &                 sapp1,sapp2,ti1,ti2
c
c ----- END OF tt_work.inc -----
