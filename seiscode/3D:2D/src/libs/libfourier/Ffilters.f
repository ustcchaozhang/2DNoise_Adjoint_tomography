c this is <Ffilters.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c ready made filters
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
c
c REVISIONS and CHANGES
c    11/11/2002   V1.0   Thomas Forbriger
c    28/06/2016   V1.1   discard function foufil_revision
c
cS
c ============================================================================
c
      subroutine foufil_clear
c
c initialize fourier filters
c set to period mode to be compatible with seife
c 
      include 'filters.inc'
c
cE
c
      call fou_clear
      call foufil_period
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      double precision function foufil_omega(in)
c
c calculate angular frequency
c in: will be treated as a frequency given in Hz, if fourier_frequency is true
c     otherwise it will be treated as a period given in sec
c 
      include 'filters.inc'
c
      double precision in
c
cE
c
      double precision pi2, value
      parameter(pi2=2.d0*3.14159265358979d0)
c
      if (fourier_frequency) then
        value=pi2*in
      else
        value=pi2/in
      endif
c
      foufil_omega=value
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_frequency
c
c set frequency mode
c 
      include 'filters.inc'
c
cE
      fourier_frequency=.true.
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_period
c
c set period mode
c 
      include 'filters.inc'
c
cE
      fourier_frequency=.false.
c
      return
      end
c
c======================================================================
c define filters
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_int
c
c integrate once
c
cE
      double complex value
      value=(0.d0,0.d0)
      call fou_pole(value)
      value=(0.d0,1.d0)
      call fou_cdenom(value)
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_dif
c
c differentiate once
c
cE
      double complex value
      value=(0.d0,0.d0)
      call fou_zero(value)
      value=(0.d0,1.d0)
      call fou_cnumer(value)
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_hpb(in, ord)
c
c Butterworth high pass filter
c in:       frequency or period (cf. foufil_omega)
c ord:      order of filter
c
      double precision in
      integer ord
c
cE
      double precision foufil_omega, omega, pi
      integer i
      double complex ime, pole, zero
      parameter (ime=(0.d0,1.d0), pi=3.1415926535897931159d0)
c
      if (ord.lt.1) stop 'ERROR (foufil_hpb): illegal order'
      omega=foufil_omega(in)
c 
      zero=(0.d0,0.d0)
      do i=1,ord
        call fou_zero(zero)
        pole=omega*exp(ime*pi*(2.d0*i-1.d0)/(2.d0*ord))
        call fou_pole(pole)
      enddo
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_lpb(in, ord)
c
c Butterworth low pass filter
c in:       frequency or period (cf. foufil_omega)
c ord:      order of filter
c
      double precision in
      integer ord
c
cE
      double precision foufil_omega, omega, pi
      integer i
      double complex ime, pole, cfac
      parameter (ime=(0.d0,1.d0), pi=3.1415926535897931159d0)
c
      if (ord.lt.1) stop 'ERROR (foufil_hpb): illegal order'
      omega=foufil_omega(in)
c 
      cfac=-ime*omega
      do i=1,ord
        call fou_cnumer(cfac)
        pole=omega*exp(ime*pi*(2.d0*i-1.d0)/(2.d0*ord))
        call fou_pole(pole)
      enddo
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_lp2(in, h)
c
c second order low pass filter
c in:       frequency or period (cf. foufil_omega)
c h:        damping as a fraction of critical
c
      double precision in, h
c
cE
      double precision foufil_omega, omega
      double complex ime, pole, cfac
      parameter (ime=(0.d0,1.d0))
c
      omega=foufil_omega(in)
c 
      cfac=-ime*omega
      call fou_cnumer(cfac)
      call fou_cnumer(cfac)
      pole=omega*(ime*h+sqrt(1.d0-h*h))
      call fou_pole(pole)
      pole=omega*(ime*h-sqrt(1.d0-h*h))
      call fou_pole(pole)
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine foufil_hp2(in, h)
c
c second order high pass filter
c in:       frequency or period (cf. foufil_omega)
c h:        damping as a fraction of critical
c
      double precision in, h
c
cE
      double precision foufil_omega, omega
      double complex ime, pole, zero
      parameter (ime=(0.d0,1.d0))
c
      omega=foufil_omega(in)
c 
      zero=(0.d0,0.d0)
      call fou_zero(zero)
      call fou_zero(zero)
      pole=omega*(ime*h+sqrt(1.d0-h*h))
      call fou_pole(pole)
      pole=omega*(ime*h-sqrt(1.d0-h*h))
      call fou_pole(pole)
c
      return
      end
c

c
c ----- END OF Ffilters.f ----- 
