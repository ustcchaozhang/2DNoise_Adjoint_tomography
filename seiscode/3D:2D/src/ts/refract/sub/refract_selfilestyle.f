c this is <refract_selfilestyle.f>
c------------------------------------------------------------------------------
cS
c
c 24/05/2000 by Thomas Forbriger (IfG Stuttgart)
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
c select file plotting style
c
c REVISIONS and CHANGES
c    24/05/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine refract_selfilestyle(i)
c
c declare parameters
c i: file no.
      integer i
c 
      include 'refract_dim.inc'
      include 'refract_pgpara.inc'
c
cE
c
c------------------------------------------------------------------------------
c go
      if (pg_file_ci(i).ge.0) then
        call pgsci(pg_file_ci(i))
        call pgsls(pg_file_ls(i))
        call pgslw(pg_file_lw(i))
        if (pg_file_rgb(1,i).ge.0.) call pgscr(pg_file_ci(i),
     &    pg_file_rgb(1,i), pg_file_rgb(2,i), pg_file_rgb(3,i))
      else
        call refract_selstyle(i)
      endif
c
      return
      end
c
c ----- END OF refract_selfilestyle.f -----
