c this is <fidase_taper.inc>
c------------------------------------------------------------------------------
c
c 10/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c hold taper information
c
c REVISIONS and CHANGES
c    10/07/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      integer tap_max
      parameter(tap_max=30)
c 
      real tap_t(tap_max, 4), tap_off(tap_max, 4)
      integer tap_n(4)
c 
      common /fidase_taper/ tap_t, tap_off, tap_n
c
c ----- END OF fidase_taper.inc -----
