c this is <refract_vpframe.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
c
c plot a frame at viewport bounds
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
c
c REVISIONS and CHANGES
c    13/11/2012   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine refract_vpframe
      call pgsave
      call pgsci(1)
      call pgslw(1)
      call pgsls(1)
      call pgsvp(0.,1.,0.,1.)
      call pgswin(0.,1.,0.,1.)
      call pgmove(0.,0.)
      call pgdraw(1.,0.)
      call pgdraw(1.,1.)
      call pgdraw(0.,1.)
      call pgdraw(0.,0.)
      call pgunsa
      return
      end
c
c ----- END OF refract_vpframe.f ----- 
