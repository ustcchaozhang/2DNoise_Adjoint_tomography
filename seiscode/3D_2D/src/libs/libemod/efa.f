c this is <efa.f> by Thomas Forbriger 13/1/1997
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c This file contains some subroutines for an earth flattening
c approximation.
c 
c The transformation functions follow the suggestment of
c G. Mueller in his tutorial to the reflectivity method.
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
c    17/03/1999   V1.1   introduced variable density transformation using
c                        exponent n as defined by
c                        G. Mueller, 1977, J. Geophys., 42, 429-436
c                        preferable values are
c                        n=-1 for body waves (after Mueller)
c                        n=1  for the acoustic case (after Helmberger)
c                        n=-2 for Rayleigh waves (after Biswas)
c                        n=-5 for Love waves (after Biswas and Knopoff)
c
cS
c----------------------------------------------------------------------
      double precision function efa_velback(R, z, vel)
c 
c return velocity of corresponding spherical model from given flat model
c (R is the earth radius, z given depth, vel velocity at depth z)
c
      double precision R, z, vel
cE
      double precision svel
c
      svel=exp(-z/R)*vel
      efa_velback=svel
      return
      end
cS
c----------------------------------------------------------------------
      double precision function efa_denback(R, z, den)
c 
c return velocity of corresponding spherical model from given flat model
c (R is the earth radius, z given depth, den density at depth z)
c
      double precision R, z, den
cE
      double precision sden
c
      sden=den*exp(z/R)
      efa_denback=sden
      return
      end
cS
c----------------------------------------------------------------------
      double precision function efa_z(R, x)
c 
c return depth of flat model corresponding to radius x
c (R is the earth radius)
c
      double precision R, x
cE
      double precision z
c
      z=R*(log(R)-log(x))
      efa_z=z
      return
      end
cS
c----------------------------------------------------------------------
      double precision function efa_r(R, z)
c 
c return radius in spherical model corresponding to depth z in flat model
c (R is the earth radius)
c
      double precision R, z
cE
      double precision ra
c
      ra=R*exp(-z/R)
      efa_r=ra
      return
      end
cS
c----------------------------------------------------------------------
      subroutine efa_vel(R, z, v, a, nl, maxlayers)
c 
c calculate velocity from polynomal coefficients
c
c input:  R     earth radius
c         z     depth for desired value of v
c         a     polynomial coefficients
c         nl    layer corresponding to radius x
c         maxlayers array dimension
c output: v     velocity in flat model
c
      integer maxlayers, nl
      double precision R, z, v, a(maxlayers, 4)
cE
      double precision q
      integer i
c
      q=exp(-z/R)
      v=a(nl, 4)
      do i=3,1,-1
        v=a(nl, i)+q*v
      enddo
      v=v/q
      return
      end
cS
c----------------------------------------------------------------------
      subroutine efa_den(R, z, v, a, nl, maxlayers)
c 
c calculate density from polynomal coefficients
c
c input:  R     earth radius
c         z     depth for desired value of v
c         a     polynomial coefficients
c         nl    layer corresponding to radius x
c         maxlayers array dimension
c output: v     density in flat model
c
      integer maxlayers, nl
      double precision R, z, v, a(maxlayers, 4)
cE
      double precision q
      integer i
c
      q=exp(-z/R)
      v=a(nl, 4)
      do i=3,1,-1
        v=a(nl, i)+q*v
      enddo
      v=v*q
      return
      end
c 
cS
c----------------------------------------------------------------------
      subroutine efa_denn(R, z, n, v, a, nl, maxlayers)
c 
c calculate density from polynomal coefficients
c 
c this version uses exponent n as defined by Mueller (the previous one is
c the version for n=-1)
c
c input:  R     earth radius
c         z     depth for desired value of v
c         a     polynomial coefficients
c         nl    layer corresponding to radius x
c         maxlayers array dimension
c output: v     density in flat model
c
      integer maxlayers, nl, n
      double precision R, z, v, a(maxlayers, 4)
cE
      double precision q
      integer i
c 
c q=r/R
c
      q=exp(-z/R)
      v=a(nl, 4)
      do i=3,1,-1
        v=a(nl, i)+q*v
      enddo
c here we got the spherical earth's density in v
c the actual transformation follows
c q=(R/r)**n
      q=exp(n*z/R)
      v=v*q
      return
      end
c 
cS
c----------------------------------------------------------------------
      double precision function efa_denbackn(R, z, n, den)
c 
c return velocity of corresponding spherical model from given flat model
c (R is the earth radius, z given depth, den density at depth z)
c
c this version uses exponent n as defined by Mueller (the previous one is
c the version for n=-1)
c
      double precision R, z, den
      integer n
cE
      double precision sden
c
c exp(z/R)=R/r
c
      sden=den*exp(-n*z/R)
      efa_denbackn=sden
      return
      end
cS
c----------------------------------------------------------------------
      subroutine efa_ampfac(R, zs, n, fexp, fdc)
c 
c The seismogram amplitudes calculated in the flat domain have to be
c corrected to be interpreted as resulting from wave propagation in a
c spherical earth. The correction factor for receivers on the earth's
c surface given by Mueller consist of two factors. One depends on
c epicentral distance, the other on the source depth. The factor 
c depending on source depth will be calculated here.
c 
c input:
c   R   earth radius
c   zs  source depth
c   n   transformation exponent as defined by Mueller
c output:
c   fexp   factor for explosion type source
c   fdc    factor for double-couple type source
c
      double precision R, zs, fexp, fdc
      integer n
cE
      double precision q
c
c q=R/rs
c
      q=exp(zs/R)
      fexp=q
      fdc=q**(n+3)
      return
      end
c 
