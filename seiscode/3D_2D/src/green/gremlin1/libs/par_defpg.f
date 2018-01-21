c this is <par_defpg.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c    24/03/98   V1.0   Thomas Forbriger
c    17/08/98   V1.1   introduced line styles
c
c==============================================================================
cS
c
      block data par_defpg
c 
      include 'glq_pgpara.inc'
c 
c least squares parameters
      data pg_border/0.1/
      data pg_vbreak/0.5/
      data pg_vpsep/0.005/
      data pg_vbord/0.05/
      data pg_lw/1/
      data pg_bestlw/15/
      data pg_ch/0.8/
      data pg_bch/1.2/
      data pg_vpch/0.1/
      data pg_colind/1/
      data pg_alphacol/2/
      data pg_betacol/5/
      data pg_alphals/1/
      data pg_linestyle/1/
      data pg_betals/1/
c 
      end
cE
c ----- END OF par_defpg.f -----
