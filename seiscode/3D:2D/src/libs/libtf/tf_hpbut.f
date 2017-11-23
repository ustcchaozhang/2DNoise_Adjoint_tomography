c this is <tf_hpbut.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
c
c Butterworth highpass coefficients
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
c    30/04/99   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      complex function tf_hpbut(omega, omega0, n)
c
c butterworth high pass impulse response coefficient
c
c omega:    angular frequency to evaluate at
c omega0:   eigenfrequency of filter
c n:        oder of filter
c
cE
c declare local variables
      real omega, omega0
      integer n
c
      complex pole, ime, coeff
      real pi, omegan
      integer i
      parameter (ime=(0.,1.),pi=3.14159265358979311599796)
c 
      coeff=1.
      omegan=omega/omega0
      do i=1,n
        pole=cexp(ime*pi*(float(i)-0.5)/float(n))
        coeff=coeff*omegan/(omegan-pole)
      enddo
c      print *,omega,omega0,ime,coeff
      tf_hpbut=coeff
c 
      return
      end
c
c ----- END OF tf_hpbut.f -----
