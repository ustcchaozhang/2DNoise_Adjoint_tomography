c this is <f77common.inc>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c common block for Fortran test
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
c    22/12/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
c dimensions for arrays
      integer amax, bmax
      parameter(amax=10,bmax=20)
c
c used elements in each dimension
      integer na, nb 
c
c complex array
      double complex array(amax,bmax)
c
c common block
      common /f77common/ array,na,nb
c
c ----- END OF f77common.inc ----- 
