c this is <tt_ingrmod.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c return parameters of any interface of the Gunther Reimann model
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
c    16/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine tt_ingrmod(i,inphi,ind,inv)
c
c declare parameters
      integer i
      real inphi,ind,inv
      include 'tt_dim.inc'
      include 'tt_model.inc'
c
cE
c
c------------------------------------------------------------------------------
c go
      if ((i-1.gt.nlay).or.(i.lt.1))
     &  stop 'ERROR (tt_ingrmod): layer index out of range'
      if (i.eq.1) then
        inphi=0.
        ind=0.
        inv=v(1)
      else
        inphi=phi(i)
        ind=d(i)
        inv=v(i)
      endif
c
      return
      end
c
c ----- END OF tt_ingrmod.f -----
