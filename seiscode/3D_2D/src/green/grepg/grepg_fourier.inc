c this is <grepg_fourier.inc>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c data space for Fourier coefficients
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
c    13/09/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
c magic number
      character*4 cmagic
      parameter(cmagic='SP34')
c 
c flag indicating available fourier data
      logical fourieravail
c 
c dimensions
      integer fnom, fntr, fmaxom, fmaxtr
      parameter(fmaxom=2000,fmaxtr=400)
c
c data
      double complex fourier(fmaxtr, fmaxom)
c
c frequencies
      real fom(fmaxom)
c
c offsets
      real foffs(fmaxtr)
c
c common
      common /fourierdata/ fourier,fom,foffs,fnom,fntr,fourieravail      
c
c ----- END OF grepg_fourier.inc ----- 
