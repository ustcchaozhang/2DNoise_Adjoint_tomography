c this is <fidase_para.inc>
c------------------------------------------------------------------------------
c $Id$
c
c 09/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c some parameter settings (common block)
c
c REVISIONS and CHANGES
c    09/07/98   V1.0   Thomas Forbriger
c    09/09/04   V1.1   options for time window
c
c==============================================================================
c
      logical debug, verbose
      logical optwinset
      double precision optwinl,optwinr
c 
      character*40 pgdevice
c 
      common /fidase_parameters/ optwinl,optwinr,debug,verbose,pgdevice,
     &          optwinset
c
c ----- END OF fidase_para.inc -----
