c this is <pg_colis.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c return colour index and index of reduced colour for model index mi
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
c    22/01/99   V1.0   Thomas Forbriger
c
      subroutine pg_colis(mi,coli,rcoli)
c
      integer mi, coli, rcoli
c 
      include 'glq_dim.inc'
      include 'glq_pgpara.inc'
      include 'glq_model.inc'
c
cE
      if ((mi.eq.mi_alpha).or.(mi.eq.mi_Qalpha)) then
        coli=pg_alphacol
        rcoli=pg_alpharcol
      elseif ((mi.eq.mi_beta).or.(mi.eq.mi_Qbeta)) then
        coli=pg_betacol
        rcoli=pg_betarcol
      else
        coli=pg_colind
        rcoli=pg_rcolind
      endif
c 
      return
      end
c
c ----- END OF pg_colis.f -----
