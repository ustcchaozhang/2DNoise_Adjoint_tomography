c this is <greda_dim.inc>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c dimension of greda arrays
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
c    17/09/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
c maximum number of linear equations (=number of stations)
      integer maxtr
c      parameter(maxtr=300)
      parameter(maxtr=500)
c other dimension limits
      integer maxsamp, maxslo, maxom
c      parameter(maxom=2100,maxslo=5*maxtr,maxsamp=maxom*2+1)
c      parameter(maxom=2100,maxslo=4*maxtr,maxsamp=maxom*2+1)
c      parameter(maxom=8300,maxslo=3*maxtr,maxsamp=maxom*2+1)
      parameter(maxom=8300,maxslo=10*maxtr,maxsamp=maxom*2+1)
c
c dimension of phasor selection table
      integer pwo_maxsel
      parameter(pwo_maxsel=30)
c dimension of pahsor lookup table
      integer pwo_maxlu
      parameter(pwo_maxlu=pwo_maxsel)
c
c ----- END OF greda_dim.inc ----- 
