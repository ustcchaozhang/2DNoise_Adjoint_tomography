c this is <fidase_workspace.inc>
c------------------------------------------------------------------------------
c
c 25/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c some workspace arrays
c
c REVISIONS and CHANGES
c    25/11/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      complex spectra(maxsamples)
      integer nspecsamp(maxtraces), firstinspec(maxtraces)

      common /fidase_workspace/ spectra, nspecsamp, firstinspec
c
c ----- END OF fidase_workspace.inc -----
