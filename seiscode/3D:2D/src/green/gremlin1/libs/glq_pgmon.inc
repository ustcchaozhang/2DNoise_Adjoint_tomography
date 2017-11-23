c this is <glq_pgmon.inc>
c------------------------------------------------------------------------------
cS
c
c 01/07/99 by Thomas Forbriger (IfG Stuttgart)
c
c define pg-monitor devices which will be served
c
c REVISIONS and CHANGES
c    01/07/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
c tapered original data green
      integer pgmon_tapogreen
c tapered modified data green
      integer pgmon_tapmgreen
c tapered modified reference green
      integer pgmon_taprgreen
c
c update levels
      integer pgmon_l_outer
      integer pgmon_l_inner
      parameter(pgmon_l_outer=0)
      parameter(pgmon_l_inner=1)
c
      common /pgmon_devices/ pgmon_tapogreen, pgmon_tapmgreen, 
     &  pgmon_taprgreen
c
cE
c ----- END OF glq_pgmon.inc -----
