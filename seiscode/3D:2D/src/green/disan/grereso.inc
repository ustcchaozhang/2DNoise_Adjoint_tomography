c this is <grereso.inc>
c------------------------------------------------------------------------------
c
c Copyright 1999,2010 by Thomas Forbriger (IfG Stuttgart)
c
c all global stuff for grereso
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
c    06/07/99   V1.0   Thomas Forbriger
c    29/04/00   V1.1   introduced 'lowpass' exponent
c    09/07/00   V1.2   introduced 'omscale' switch
c    13/11/10   V1.3   removed non-conforming tab
c
c==============================================================================
c
      integer maxslo, maxpf, maxr
      parameter(maxslo=300,maxpf=300, maxr=300)
      integer nr, nslo,np,nom

c output spectra (spec(p,f) or spec(p,p0))
      complex spec(maxslo, maxpf)

c resolution matrix (remat(p,p0))
      complex remat(maxslo, maxpf)

c input spectrum to be convolved (inspec(p, f))
      complex inspec(maxslo, maxpf)

c frequency values
      real om(maxpf)

c test slowness value
      real tslo(maxpf)

c slowness values
      real slo(maxslo)

c slowness stepping
      double precision dp

c input seismogram spectra
      double complex u(maxr)

c inverse gram
      double precision igr(maxr,2)
c      double precision igr(maxr,maxr)

c geophone positions
      double precision r(maxr)

c damping constant for scalar product
      double precision rho 

c seismogram exponential damping coefficient
      double precision kappa

c seismogram taper fraction
      double precision tfrac

c seismogram 'lowpass' exponent
      double precision lowexp

      logical seismoh1,seismoh2,analyh1,analyh2,omscale

      common /greresos/ spec,om,tslo,slo,u,igr,r,rho,kappa,tfrac,lowexp,dp,
     &                  nr,nslo,nom,np,seismoh1,seismoh2,analyh1,analyh2,
     &                  omscale
      common /greresom/ remat, inspec
c
c ----- END OF grereso.inc -----
