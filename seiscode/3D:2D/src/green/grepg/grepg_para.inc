c this is <grepg_para.inc>
c------------------------------------------------------------------------------
c
c 25/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c some parameters
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
c    24/06/99   V1.1   new parameters defining normalizing taper
c    05/03/01   V1.2   color curve parameter
c
c==============================================================================
c
      character*79 plstring_lastwarn, plstring_version
      integer plpar_pickmode
c 
      common /grepg_strings/ plstring_lastwarn, plstring_version
      common /grepg_plpara/ plpar_pickmode
c 
      integer plpar_lscyc, plpar_colcyc, plpar_bgcurvewidth
      integer plpar_colcurw
      logical plflag_linestyle, plflag_color, plflag_bgcurve
      logical plflag_colcur
      real plpar_colcurc(3)
      common /grepg_para/ plpar_lscyc, plpar_colcyc,  plpar_bgcurvewidth,
     &                    plpar_colcurw, plpar_colcurc
      common /grepg_flags/ plflag_linestyle, plflag_color, plflag_bgcurve,
     &                     plflag_colcur
c 
c original parameters
      real mins, minf, maxs, maxf
c 
      common /grepg_mainpara/ mins, maxs, minf, maxf
c 
c define normalizing taper
      real norm_center, norm_width
      logical norm_on
      common /grepg_norm/ norm_center, norm_width, norm_on
c
c ----- END OF grepg_para.inc -----
