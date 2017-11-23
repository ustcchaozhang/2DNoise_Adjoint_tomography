c this is <par_showpg.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c display pgplot parameters
c
c ----
c This program is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c This program is distributed in the hope that it will be useful,
c but WITHOUT ANY WARRANTY; without even the implied warranty of
c MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
c GNU General Public License for more details.
c 
c You should have received a copy of the GNU General Public License
c along with this program; if not, write to the Free Software
c Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
c ----
c
c REVISIONS and CHANGES
c    20/01/99   V1.0   Thomas Forbriger
c    02/03/99   V1.1   show background rgb
c
c==============================================================================
cS
c 
      subroutine par_showpg
c 
c display all data related parameters
c
      include 'glq_dim.inc'
      include 'glq_pgpara.inc'
c 
cE
      integer i,j
c     
      print 50, pg_border, pg_vbreak, pg_vphsep, pg_vpsep, pg_vbord
      print 51, pg_lw, pg_bestlw, pg_clw
      print 52, pg_ch, pg_bch, pg_wch, pg_lch, pg_ach, pg_vpch
      print 53, pg_colind, pg_rcolind, pg_alphacol, pg_alpharcol,
     &          pg_betacol, pg_betarcol
      print 54, pg_linestyle, pg_alphals, pg_betals
      print 55, pg_plottitle, pg_shorti, pg_dohatch
      do i=0,pg_maxrgb
        print 56,i,(pg_rgbtable(j,i), j=1,3)
      enddo
c 
      return
   50 format(/'pgplot parameters:',/,' viewport:',/
     &  '  border width (unused):',t30,'[bord]: ',f10.3,/
     &  '  vertical viewport sep.:',t30,'[vsep]: ',f10.3,/
     &  '  horizontal viewport sep.:',t30,'[hsep]: ',f10.3,/
     &  '  viewport separator width:',t30,'[vsew]: ',f10.3,/
     &  '  viewport border:',t30,'[vbor]: ',f10.3)
   51 format(' line width:',/,
     &  '  standard:',t30,'[sdlw]: ',i10,/
     &  '  xmax position:',t30,'[xmlw]: ',i10,/
     &  '  curves:',t30,'[culw]: ',i10)
   52 format(' character height:',/,
     &  '  standard:',t30,'[sdch]: ',f10.3,/
     &  '  alpha/beta:',t30,'[abch]: ',f10.3,/
     &  '  wedge:',t30,'[wech]: ',f10.3,/
     &  '  labels:',t30,'[lach]: ',f10.3,/
     &  '  annotations:',t30,'[anch]: ',f10.3,/
     &  '  viewport calculation:',t30,'[vpch]: ',f10.3)
   53 format(' colour index:',/,
     &  '  standard:',t30,'[sdci]: ',i10,/
     &  '  reduced standard:',t30,'[rsci]: ',i10,/
     &  '  alpha:',t30,'[alci]: ',i10,/
     &  '  reduced alpha:',t30,'[raci]: ',i10,/
     &  '  beta:',t30,'[beci]: ',i10,/
     &  '  reduced beta:',t30,'[rbci]: ',i10)
   54 format(' line style:',/,
     &  '  standard:',t30,'[sdls]: ',i10,/
     &  '  alpha:',t30,'[alls]: ',i10,/
     &  '  beta:',t30,'[bels]: ',i10)
   55 format(' look and feel:',/,
     &  '  plot title:',t30,'[plot]: ',l10,/
     &  '  use short titles:',t30,'[shot]: ',l10,/
     &  '  plot with hatching:',t30,'[htch]: ',l10,/
     &  ' rgb-table: [rgb i,r,g,b]',/'  index     red    green     blue')
   56 format(i7,3(2xf6.4))
      end
c
c ----- END OF par_showpg.f -----
