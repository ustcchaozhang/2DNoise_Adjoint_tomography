c this is <grepg_phase.f>
c------------------------------------------------------------------------------
cS
c
c 14/10/99 by Thomas Forbriger (IfG Stuttgart)
c
c plot colorfull with phase
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
c    14/10/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine grepg_phase(pd, pp, ma, mb,
     &              a1, a2, b1, b2,
     &              maxvalue, minvalue, tr, bgw)
c
c declare parameters
      real pd(ma,mb), pp(ma,mb)
      integer ma,mb,a1,a2,b1,b2
      real minvalue, maxvalue, tr(6)
      logical bgw
c
cE
c declare local variables
      integer i,j,ci
      real amp,pha,xco(4),yco(4),vh,vl,vs,vi,vj
      parameter(ci=5)
c
c------------------------------------------------------------------------------
c go
      call pgsave
      do i=a1,a2
        do j=b1,b2
          amp=(min(maxvalue,max(minvalue,pd(i,j)))-minvalue)/
     &      (maxvalue-minvalue)
          pha=pp(i,j)
c 
          vi=float(i)
          vj=float(j)
          xco(1)=tr(1)+tr(2)*(vi-0.5)+tr(3)*(vj-0.5)
          xco(2)=tr(1)+tr(2)*(vi+0.5)+tr(3)*(vj-0.5)
          xco(3)=tr(1)+tr(2)*(vi+0.5)+tr(3)*(vj+0.5)
          xco(4)=tr(1)+tr(2)*(vi-0.5)+tr(3)*(vj+0.5)
          yco(1)=tr(4)+tr(5)*(vi-0.5)+tr(6)*(vj-0.5)
          yco(2)=tr(4)+tr(5)*(vi+0.5)+tr(6)*(vj-0.5)
          yco(3)=tr(4)+tr(5)*(vi+0.5)+tr(6)*(vj+0.5)
          yco(4)=tr(4)+tr(5)*(vi-0.5)+tr(6)*(vj+0.5)
c 
          vh=pp(i,j)
          if (bgw) then
            vl=1.-0.5*amp
          else
            vl=0.5*amp
          endif
          vs=amp
c 
          call pgsci(ci)
          call pgshls(ci, vh, vl, vs)
          call pgpoly(4, xco, yco)
c 
        enddo
        if (i.eq.(30*int(i/30))) then
          call pgupdt
        endif
      enddo
      call pgunsa
c
      return
      end
c
c ----- END OF grepg_phase.f -----
