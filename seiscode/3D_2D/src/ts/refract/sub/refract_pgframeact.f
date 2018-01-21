c this is <refract_pgframeact.f>
c------------------------------------------------------------------------------
c
c 01/05/98 by Thomas Forbriger (IfG Stuttgart)
c
c activate main frame
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
c    01/05/98   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      subroutine pgframeact
c
c activate main frame
c
      include 'refract_dim.inc'
      include 'refract_seipar.inc'
c
      call pgsvp(tov_vpleft, tov_vpright, tov_vpbot, tov_vptop)
      call pgswin(tov_tmin, tov_tmax, tov_rmin, tov_rmax)
c
      return
      end
c
c ----- END OF refract_pgframeact.f -----
