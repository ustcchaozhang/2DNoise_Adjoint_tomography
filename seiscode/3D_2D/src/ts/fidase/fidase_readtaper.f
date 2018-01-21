c this is <fidase_readtaper.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
cS
c
c read taper information from quad taper file
c
c REVISIONS and CHANGES
c    10/07/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine readtaper(filename)
c
      character filename*(*)
c
      include 'fidase_taper.inc'
      include 'fidase_para.inc'
c 
      character*80 comment
c
cE
      if (verbose) 
     &  print *,'read taper from file ',filename(1:index(filename, ' '))
      call tf_ttapread(filename, tap_t, tap_off, tap_n, tap_max, comment)
      if (verbose) print *,'taper is ',comment(1:index(comment, ' '))
c
      return
      end
c 
c ----- END OF fidase_readtaper.f -----
