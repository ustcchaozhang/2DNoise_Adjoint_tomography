c this is <dig.inc>
c------------------------------------------------------------------------------
c
c 26/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c some common blocks for dig
c
c REVISIONS and CHANGES
c    26/11/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      integer maxpicktypes, maxpicks
      parameter(maxpicktypes=10, maxpicks=50)

      integer npicks(maxpicktypes)
      real spicks(maxpicks, maxpicktypes)
      real fpicks(maxpicks, maxpicktypes)

      common /syg_picks/ npicks, spicks, fpicks
c
c ----- END OF dig.inc -----
