c this is <refract_plotoffset.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
c
c return plot offset (ordinate)
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
c    24/10/2013   V1.0   Thomas Forbriger
c
c ============================================================================
c
      real function plotoffset(itrace)
c
      integer itrace
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
c
      real theoffset
c
      theoffset=roffset(itrace)
      if (plflag_osnoreduce) theoffset=fieldoffset(itrace)
c
      plotoffset=theoffset
      return
      end
c
c ----- END OF refract_plotoffset.f ----- 
