c this is <seife_mra.f>
c------------------------------------------------------------------------------
cS
c   ($Id$)
c
c 22/11/2001 by Thomas Forbriger (IfG Stuttgart)
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
c multiply with ramp
c
c REVISIONS and CHANGES
c    22/11/2001   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine seife_mra(nil,par,x,n,dt,msg)
c
c declare parameters
      real*8 x(n)
      character par*(*), msg*(*)
      logical nil
c
cE
      real rampfac
c
c------------------------------------------------------------------------------
c go
      read(par, *) rampfac
      do 1 j=1,n
    1   x(j)=x(j)*dt*(j-1)*rampfac
      write(msg,'("multiplied with ramp: ",f10.3)') rampfac
      nil=.false.
      return
      end
c
c ----- END OF seife_mra.f -----
