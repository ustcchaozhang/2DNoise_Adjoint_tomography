c this is <refract_preread.f>
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
c read additional files on boot (taper, traveltimes, etc.)
c
c REVISIONS and CHANGES
c    24/05/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine refract_preread
c
c declare parameters
      include 'refract_dim.inc' 
      include 'refract_opt.inc' 
      include 'refract_para.inc' 
      include 'refract_picks.inc' 
      include 'refract_model.inc' 
c
cE
c declare local variables
c 
      character*80 comment 
c
c------------------------------------------------------------------------------
c go
c model file
      if (opt_Fmodel(1:7).ne.'-NONE- ') then
        call refract_doreadmodel(opt_Fmodel)
      endif
c
c travel time polygon file
      if (opt_Fpicks(1:7).ne.'-NONE- ') then
          call refract_doreadttpicks(opt_Fpicks,1)
      endif
c
c arrival times file
      if (opt_Farrival(1:7).ne.'-NONE- ') then
          call refract_doreadttpicks(opt_Farrival,6)
      endif
c
c taper file
      if (opt_Ftaper(1:7).ne.'-NONE- ') then
        call tf_ttapread(opt_Ftaper,pick_x(1,2), pick_y(1,2), pick_n(2),
     &    pick_max, comment)
        flag_replot=.true.
      endif
c
      return
      end
c
c ----- END OF refract_preread.f -----
