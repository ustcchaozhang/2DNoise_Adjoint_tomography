c this is <fsexphyd.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c pressure from explosion in full space
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
c    04/07/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
c calculate greens coefficient for pressure component for
c
c P_total(omega,r)=int_0^infty P(p,omega) J_0(p*omega*r) p dp
c
c this function returns P(p,omega)
c where p and omega where set in the preparation history
c
c p: phase slowness
c omega: angular frequency
c
      subroutine gr_green(omega, green)
c 
c parameter
      double precision omega
      double complex green
c
cE
c
c get leading array dimension nnd
      include 'common.inc'
c
      double precision pi
      parameter(pi=3.14159265358979311599796d0)
      double complex IME
      parameter(IME=(0.d0,1.d0))
c 
      double precision A0,z,zq,p
      double complex a
c 
      stop 'STOP: pressure formula not yet implemented'
c 
      p=slowness
c receiver is at 10cm depth
      z=0.1d0
      zq=srcdep
c 
      A0=-srcamp/(4.d0*pi*mdensity(1)*malpha(1)**2)
      a=zsqrt(((1.d0,0.d0)/malpha(1)**2)-p**2)
c
      green=-A0*omega**2*sign(1.d0,(z-zq))*exp(IME*omega*a*abs(z-zq))
c 
      return
      end
cS
c----------------------------------------------------------------------
c not implemented - radial component of pressure makes no sense
c
c calculate greens coefficient for vertical component for
c u_z_total(omega,r)=int_0^infty u_z(u,omega) J_0(p*omega*r) p dp
c
c and greens coefficient for radial component:
c u_r_total(omega,r)=int_0^infty u_r(u,omega) J_1(p*omega*r) p dp
c
c this function returns u_z(p,omega) in greenz
c                   and u_r(p,omega) in greenr
c where p and omega where set in the preparation history
c
c p is the phase slowness
c omega is the angular frequency
c
      subroutine gr_greenzr(omega, greenz, greenr)
c 
c parameter
      double precision omega
      double complex greenz, greenr
c 
cE
      call gr_green(omega, greenz)
      greenr=(0.d0,0.d0)
c 
      return
      end
c
c ----- END OF fsexphyd.f ----- 
