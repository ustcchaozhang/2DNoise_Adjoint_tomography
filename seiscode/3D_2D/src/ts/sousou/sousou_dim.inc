c this is <sousou_dim.inc>
c------------------------------------------------------------------------------
c
c 09/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c array dimensions set for fidase
c
c REVISIONS and CHANGES
c    09/07/98   V1.0   Thomas Forbriger
c    17/11/98          taken from fidase
c
c==============================================================================
c
      integer maxtraces, maxsamples, maxfiles, maxspec
      parameter(maxtraces=300,maxsamples=2000000, maxfiles=10, maxspec=4100)
      integer maxslow,maxplot
c maxslow must be less than maxtraces!
      parameter(maxslow=30,maxplot=300)
c
c ----- END OF fidase_dim.inc -----
