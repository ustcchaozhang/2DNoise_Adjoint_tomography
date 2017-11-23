c this is <mod_eval.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c evaluate polynomial model description at any depth
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
c 11.2.2010: changed definition of a(3) return value
c I understand that a(2) should be the first derivative and a(3) should be the
c second derivative. This was not the case. I assume that this was a
c programming error. Currently the subroutine is only used in function
c mod_writeold and this uses only the return value a(1) and withx in the
c program moval.f which is not part of the inversion process. For this reason
c I modified the definition without hesitation.
c
c REVISIONS and CHANGES
c    03/03/99   V1.0   Thomas Forbriger
c    11/02/10   V1.1   modified definition of a(3)
c
c==============================================================================
c
      subroutine mod_eval(mindex, pindex, depth, a)
c 
c mindex:   index to model space
c pindex:   index to model parameter
c depth:    depth to calculate parameter value at
c a:        value return-array
c           a(1): value at depth
c           a(2): first derivative at depth
c           a(3): second derivative at depth
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
c declare parameters
      integer mindex,pindex
      real depth
      real a(glqm_mpol)
c
cE
c declare local variables
      integer mod_isec, isec, i
      real x, secbot, sectop
      real b(glqm_mpol)
c
c------------------------------------------------------------------------------
c go
      if (mindex.gt.glqm_mmod) stop 'ERROR (mod_eval): model indexi to large'
      if (mindex.lt.1) stop 'ERROR (mod_eval): model index less than 1'
      isec=mod_isec(depth, mindex)
      sectop=0.
      if (isec.gt.1) sectop=mdepth(isec-1,mindex)
      secbot=mdepth(isec,mindex)
      x=depth-0.5*(secbot+sectop)
      do i=1,glqm_mpol
        if (i.le.glqm_npol(isec,pindex)) then
          b(i)=model(i, isec, pindex, mindex)
        else
          b(i)=0.
        endif
      enddo
      a(1)=b(1)+b(2)*x+b(3)*x*x
      a(2)=b(2)+2.*b(3)*x
c
c ATTENTION: should'nt this be
c a(3)=2.*b(3)
c
c      a(3)=b(3)
c
c 11.2.2010 changed this to:
      a(3)=2.*b(3)
c
      return
      end
c
c ----- END OF mod_eval.f -----
