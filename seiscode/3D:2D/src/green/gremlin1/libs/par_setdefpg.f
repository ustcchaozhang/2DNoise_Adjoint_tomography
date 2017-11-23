c this is <par_setdefpg.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c After splitting the source into different object files, the linker
c refuses to link blockdata subroutines.
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
c    06/04/98   V1.0   Thomas Forbriger
c    17/08/98   V1.1   introduced line styles
c    20/01/99   V1.2   added some new options
c    02/03/99   V1.3   set background rgb
c
c==============================================================================
c
c
      subroutine par_setdefpg
c 
      include 'glq_pgpara.inc'
c 
      integer i,j
      real rgbdefault(3, 0:pg_maxrgb)
      data rgbdefault/1.,1.,1.,
     &                0.,0.,0.,   .7,.7,.7,   
     &                1.,0.,0.,   .9,.7,.7,
     &                0.,0.,1.,   .7,.7,.9,
     &                0.,1.,1.,   .7,.9,.9/
c 
c least squares parameters
      pg_border=0.1
      pg_vbreak=0.5
      pg_vphsep=0.5
      pg_vpsep=0.005
      pg_vbord=0.05
      pg_lw=1
      pg_bestlw=15
      pg_ch=0.8
      pg_bch=1.2
      pg_vpch=0.1
      pg_colind=1
      pg_alphacol=3
      pg_betacol=5
      pg_linestyle=1
      pg_alphals=1
      pg_betals=1
c 20/01/99:
      pg_wch=pg_ch
      pg_lch=pg_ch
      pg_ach=pg_ch
      pg_plottitle=.true.
      pg_rcolind=2
      pg_alpharcol=4
      pg_betarcol=6
      pg_clw=pg_lw
      pg_dohatch=.false.
      pg_shorti=.false.
c 
      do i=0,pg_maxrgb
        do j=1,3
          pg_rgbtable(j,i)=rgbdefault(j,i)
        enddo
      enddo
c 
      return
      end
cE
c
c ----- END OF par_setdefpg.f -----
