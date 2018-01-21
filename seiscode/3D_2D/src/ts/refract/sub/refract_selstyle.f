c this is <refract_selstyle.f>
c------------------------------------------------------------------------------
c
c 03/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c select plotting style for trace
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    03/07/98   V1.0   Thomas Forbriger
c
c==============================================================================
cS
      subroutine refract_selstyle(i)
c
c select plotting style for trace i
c
      integer i
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
c
cE
      integer newci, newls
c select color and line style
      if ((plflag_linestyle).and.(plflag_color).and.
     &  (plpar_colcyc.gt.1).and.(plpar_lscyc.gt.1)) then
        newci=mod((i-1),plpar_colcyc)+1
        newls=mod(int((i-1)/plpar_colcyc),plpar_lscyc)+1
        call pgsci(newci)
        call pgsls(newls)
      elseif ((plflag_color).and.(plpar_colcyc.gt.1)) then
        newci=mod((i-1),plpar_colcyc)+1
        call pgsci(newci)
      elseif ((plflag_linestyle).and.(plpar_lscyc.gt.1)) then
        newls=mod((i-1),plpar_lscyc)+1
        call pgsls(newls)
      endif
c
      return
      end
c
c ----- END OF refract_selstyle.f -----
