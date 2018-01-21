c this is <gr_source.inc>
c------------------------------------------------------------------------------
c
c 24/07/97 by Thomas Forbriger (IfG Stuttgart)
c
c This is just a common block definition to hold information on the
c seismic source
c
c REVISIONS and CHANGES
c    24/07/97   V1.0   Thomas Forbriger
c
c==============================================================================
c
c src_depth    source depth in kilometer
c src_type     source type: 1=vertical single force   2=explosion
c src_layer    layer in which source is located
c src_amp      source amplitude in N (force) or N*m (explosion)
c src_dim      dimension factor for source
c
      double precision src_depth, src_amp, src_dim, FFI
      integer src_type, src_layer
      common /gr_source/FFI, src_depth, src_amp, src_type, src_layer, src_dim
c
c ----- END OF gr_source.inc -----
