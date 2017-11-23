c this is <refract_model.inc>
c------------------------------------------------------------------------------
c
c 05/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c some earth model data 
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
c
c==============================================================================
c
c model data is stored in terms of layer thickness and phase slowness
c
      integer mod_maxlay
      parameter(mod_maxlay=10)
c
      integer mod_nlay
      real mod_slo(mod_maxlay)
      real mod_thick(mod_maxlay)
c 
c box position
      real mod_boxx, mod_boxy
c
      logical mod_valid
c
      common /refract_model/ mod_slo, mod_thick, mod_nlay, mod_valid
      common /refract_modbox/ mod_boxx, mod_boxy
c
c ----- END OF refract_model.inc -----
