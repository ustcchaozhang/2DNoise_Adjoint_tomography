c this is <res_parselect.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c create parselect array in glq_reso.inc
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
c    03/05/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine res_parselect
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_reso.inc'
c
      double precision mweightmax
      integer im
c find maximum search range value
      mweightmax=0.d0
      do im=1,mod_n
        mweightmax=max(mweightmax,mweight(im))
      enddo
c find selected parameters
      do im=1,mod_n
        if ((mweight(im)*mweightcondition).gt.mweightmax) then
          parselect(im)=.true.
        else
          parselect(im)=.false.
        endif
      enddo
c
      return
      end
c
c ----- END OF res_parselect.f ----- 
