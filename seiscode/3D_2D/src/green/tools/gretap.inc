c this is <gretap.inc>
c------------------------------------------------------------------------------
c
c 26/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c some common blocks for gretap
c
c REVISIONS and CHANGES
c    30/06/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
c taper data
      integer maxslo, maxfreq
      parameter (maxslo=10000, maxfreq=5000)
c 
      real weight(maxfreq, maxslo)
      real om(maxfreq), slo(maxslo)
      integer nom, nslo
c 
      integer setmode, mode_or, mode_mul
      parameter(mode_or=1,mode_mul=2)
c
c picks
      integer maxpicktypes, maxpicks
      parameter(maxpicktypes=10, maxpicks=50)

      integer npicks(maxpicktypes)
      real spicks(maxpicks, maxpicktypes)
      real fpicks(maxpicks, maxpicktypes)

      common /gretap_picks/ npicks, spicks, fpicks
      common /gretap_weight/ weight,om,slo,nom,nslo
      common /gretap_modes/ setmode
c
c ----- END OF gretap.inc -----
