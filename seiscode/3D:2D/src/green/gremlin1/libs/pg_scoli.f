c this is <pg_scoli.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c set correct depth-dependent colour index 
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
c    22/01/99   V1.1   subdivided into pg_colis and pg_zcoli
c
      subroutine pg_scoli(mi,z1,z2,mod)
c
c select colour index for model parameter mi in depth range from z1 to z2
c mod indicates the polynomial model index
c 
c depths are expected to be in kilometer units
c
      integer mi, mod
      double precision z1, z2
c 
      include 'glq_dim.inc'
      include 'glq_pgpara.inc'
      include 'glq_model.inc'
c
cE
      integer coli,rcoli
      logical flag
c 
      call pg_colis(mi,coli,rcoli)
c 
      if (coli.eq.rcoli) then
        flag=.false.
      else
        call pg_zcoli(mi,z1*1.e3,z2*1.e3,mod,flag)
      endif
c
      if (flag) then
        call pgsci(coli)
      else
        call pgsci(rcoli)
      endif
c 
c 
      return
      end
c
c ----- END OF pg_scoli.f -----
