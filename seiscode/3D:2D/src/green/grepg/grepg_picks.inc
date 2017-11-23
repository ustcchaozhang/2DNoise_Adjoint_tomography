c this is <grepg_picks.inc>
c------------------------------------------------------------------------------
c
c 25/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c hold pick values
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
c    25/11/98   V1.0   Thomas Forbriger
c    21/01/02   V1.1   use maxpickdim
c
c==============================================================================
c
      integer npicks(maxpickdim), active_pick
      real spicks(maxpicks, maxpickdim)
      real fpicks(maxpicks, maxpickdim)

      common /grepg_picks/ npicks, spicks, fpicks, active_pick
c
c ----- END OF grepg_picks.inc -----
