c this is <fidase_enfit.inc>
c------------------------------------------------------------------------------
c
c 15/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c some data space to hold enfit stuff
c
c REVISIONS and CHANGES
c    15/07/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
c index to receiver positions (will be an index to enf_offset)
      integer enf_position(maxtraces)
c offsets used by linear inversion
      real enf_offset(maxtraces)
      integer enf_noff
c 
c matrix to calculate derivatives
      double precision enf_Mdif(maxtraces-2,3)
c diagonal matrix to hold number of participating traces
      integer enf_Mn(maxtraces)
c 
c vector to hold reference energies
      double precision enf_Vref(maxtraces)
c
c matrix to hold trace energies
      double precision enf_Men(maxtraces, maxfiles-1)
c 
c vector of amplitude scalings
      double precision enf_Vscal(maxfiles-1)
c 
c common block
      common /fidase_enf/ enf_Mdif, enf_Men, enf_Vref, enf_Vscal,
     &                    enf_Mn, enf_offset, enf_position, enf_noff
c
c ----- END OF fidase_enfit.inc -----
