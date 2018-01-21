c this is <glq_pgpara.inc>
c------------------------------------------------------------------------------
cS
c
c 03/12/97 by Thomas Forbriger (IfG Stuttgart)
c
c set some parameter values for the pgplot routines plotting
c
c REVISIONS and CHANGES
c    03/12/97   V1.0   Thomas Forbriger
c    30/01/98   V1.1   make them changable
c    16/04/98   V1.2   memory variables for plot ranges
c    17/08/98   V1.3   introduced line-style variables
c    20/01/99   V1.4   introduced some new parameters
c    02/03/99   V1.5   start rgb with background
c
c==============================================================================
c 
      real pg_border
c      parameter(pg_border=0.1)
c 
      real pg_vbreak, pg_vpsep, pg_vbord, pg_vphsep
c      parameter(pg_vbreak=0.5, pg_vpsep=0.005, pg_vbord=0.05)
c
      integer pg_lw, pg_bestlw, pg_clw
c      parameter(pg_lw=1, pg_bestlw=20)
c 
      real pg_ch, pg_bch, pg_vpch, pg_wch, pg_lch, pg_ach
c      parameter(pg_ch=.8, pg_bch=1.2, pg_vpch=0.100)
c 
      logical pg_plottitle
      logical pg_shorti
c 
      integer pg_maxrgb
      parameter(pg_maxrgb=8)
      real pg_rgbtable(3,0:pg_maxrgb)
c
      integer pg_colind, pg_rcolind
      integer pg_alphacol, pg_alpharcol
      integer pg_betacol, pg_betarcol
      integer pg_linestyle
      logical pg_dohatch
c      parameter (pg_colind=1, pg_alphacol=2, pg_betacol=5)
c 
      integer pg_alphals
      integer pg_betals
c 
c plot range memory
      real pg_velmin, pg_velmax
      real pg_denmin, pg_denmax
      real pg_qmin, pg_qmax
      real pg_depmin, pg_depmax
c 
      common /pgpara/pg_border, pg_vbreak, pg_vpsep, pg_vbord, pg_vphsep,
     &               pg_lw, pg_bestlw, pg_ch, pg_bch, pg_vpch,
     &               pg_velmin, pg_velmax, pg_denmin, pg_denmax,
     &               pg_qmin, pg_qmax, pg_depmin, pg_depmax,
     &               pg_colind, pg_alphacol, pg_betacol,
     &               pg_linestyle, pg_alphals, pg_betals,
     &               pg_rgbtable, pg_rcolind, pg_alpharcol, pg_betarcol,
     &               pg_plottitle, pg_clw, pg_wch, pg_lch, pg_ach,
     &               pg_dohatch, pg_shorti
c
cE
c ----- END OF glq_pgpara.inc -----
