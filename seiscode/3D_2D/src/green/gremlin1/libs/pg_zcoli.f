c this is <pg_zvari.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c return flag indicating an anonymous parameter contained in depth range
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
c
      subroutine pg_zcoli(mi,z1,z2,mod,flag)
c
c flag is true if named model parameter of model index mod in the depth
c range from z1 to z2 has a representation in the set of anonymous
c parameters
c 
c depths are expected to be in meter units
c
      integer mi, mod
      double precision z1, z2
      logical flag
c 
      include 'glq_dim.inc'
      include 'glq_pgpara.inc'
      include 'glq_model.inc'
c
cE
      integer mod_isec,sec
      real mz
c 
      mz=(z1+z2)*0.5
c 
      sec=mod_isec(mz,mod)
      if (glqm_follow(sec,mi)) then
        if (mestim(sec,mi)) then
          flag=.true.
        else
          do while ((sec.gt.1).and.
     &        (glqm_follow(sec,mi)).and.(.not.(mestim(sec,mi))))
            sec=sec-1
          enddo
          if (mestim(sec,mi)) then
            flag=.true.
          else
            flag=.false.
          endif
        endif
      else
        if (mestim(sec,mi)) then
          flag=.true.
        else
          flag=.false.
        endif
      endif
c 
      return
      end
c
c ----- END OF pg_zcoli.f -----
