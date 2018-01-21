c this is <refract_seipar.inc>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c parameters for seismogram viewports
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
c    30/04/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
c parameters for the total seismogram viewport
c
c world coordinates
      real tov_rmin, tov_rmax, tov_tmin, tov_tmax
c viewport
      real tov_vpbot, tov_vptop, tov_vpleft, tov_vpright
c
c parameters for a single trace viewport
c
c world coordinates 
      real trv_vmax, trv_vmin, trv_tmin, trv_tmax
c viewport in world coordinates of the total viewport
      real trv_rbot, trv_rtop, trv_tleft, trv_tright
c viewport
      real trv_vpbot, trv_vptop, trv_vpleft, trv_vpright
c scaling factor for each trace (meters per count)
      real trv_mpc(maxtraces)
c
c
c common blocks
      common /refract_tov/tov_rmin, tov_rmax, tov_tmin, tov_tmax,
     &                 tov_vpbot, tov_vptop, tov_vpleft, tov_vpright 
      common /refract_trv/ trv_mpc, trv_rbot, trv_rtop, trv_tleft, trv_tright,
     &                 trv_vpbot, trv_vptop, trv_vpleft, trv_vpright,
     &                 trv_vmax, trv_vmin, trv_tmin, trv_tmax 
c
c ----- END OF refract_seipar.inc -----
