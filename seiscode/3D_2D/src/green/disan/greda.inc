c this is <greda.inc>
c------------------------------------------------------------------------------
c
c 23/10/97 by Thomas Forbriger (IfG Stuttgart)
c
c This file contains the matrix inversion common block for greda.f
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
c    23/10/97   V1.0   Thomas Forbriger
c    12/06/02   V1.1   supply wavelength specific taper factors
c    17/09/02   V1.2   move dimensions to greda_dim.inc
c    10/01/12   V1.3   provide component selection
c
c==============================================================================
c
c number of linear equations (=number of stations)
      integer ntr
c some other dimensions
      integer nslo, nsamp, nom
c
c gram matrix
      double precision gram(maxtr, maxtr)
c
c expansion coefficients to hilbert space representers
      double complex alpha(maxsamp)
c
c complex seismogram spectra
      double complex spectra(maxtr, maxsamp)
c
c wavelength specific taper factors
      real wltaperfac(maxtr)
c 
c wavelength taper length in units of wavelength
      real wltaperlen
c 
c wavelength taper falling edge start in units of the taper length
      real wltaperfrac
c 
c true, if radial compoment has to be transformed
      logical radial
c
c----------------------------------------------------------------------
c
c common blocks
      common /hilbert/alpha, gram
      common /seismo/spectra, nsamp, nom, ntr, nslo
      common /wltaper/wltaperfac, wltaperlen, wltaperfrac
      common /component/ radial
c
c ----- END OF greda.inc -----
