c this is <refract_picks.inc>
c------------------------------------------------------------------------------
c
c 05/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c data space to hold picks
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
c    05/07/98   V1.0   Thomas Forbriger
c    18/08/98   V1.1   did set picktypes to 6 (includes now picks for one
c                      traveltime per trace)
c
c==============================================================================
c
      integer pick_max, pick_ntypes
      parameter(pick_max=400, pick_ntypes=6)
c 
      real pick_x(pick_max, pick_ntypes)
      real pick_y(pick_max, pick_ntypes)
      integer pick_n(pick_ntypes)
c
      common /refract_pickarray/ pick_x, pick_y, pick_n
c
c ----- END OF refract_picks.inc -----
