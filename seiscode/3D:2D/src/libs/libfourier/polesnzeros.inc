c this is <polesnzeros.inc>
cS
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c ----
c libfourier is free software; you can redistribute it and/or modify
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
c common containing pole and zero collection
c
c REVISIONS and CHANGES
c    11/11/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
c dimension
      integer fourier_nmax
      parameter(fourier_nmax=20)
c
c numerator
      double complex fourier_numer
c
c denominator
      double complex fourier_denom
c
c complex poles
      double complex fourier_poles(fourier_nmax)
c
c complex zeros
      double complex fourier_zeros(fourier_nmax)
c
c fill mode (normal or inverse)
      logical fourier_normal
c
c counters
      integer fourier_npoles, fourier_nzeros
c
c common block
      common /fourier_pnz/ fourier_numer, fourier_denom,
     &  fourier_poles, fourier_zeros, 
     &  fourier_npoles, fourier_nzeros, fourier_normal
c
cE
c ----- END OF polesnzeros.inc ----- 
