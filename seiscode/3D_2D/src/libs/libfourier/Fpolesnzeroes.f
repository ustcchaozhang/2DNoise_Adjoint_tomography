c this is <Fpolesnzeroes.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c pole and zero access subroutines
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
c    13/11/2002   V1.1   introduce waterlevel to omega
c    28/06/2016   V1.2   discard function fou_revision
c
cS
c ============================================================================
c
      subroutine fou_clear
c
c initialize poles and zeros database
c
      include 'polesnzeros.inc'
c
cE
      call fou_normal
      fourier_numer=(1.d0,0.d0)
      fourier_denom=(1.d0,0.d0)
      fourier_npoles=0
      fourier_nzeros=0
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_inverse
c
c set inverse flag
c all following coefficients will be treated as inverse filters
c
      include 'polesnzeros.inc'
c
cE
c
      fourier_normal=.false.
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_normal
c
c reset inverse flag
c all following coefficients will be treated as normal filters
c
      include 'polesnzeros.inc'
c
cE
c
      fourier_normal=.true.
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_pole(pole)
c
c set one pole
c
      double complex pole
c
      include 'polesnzeros.inc'
c
cE
c
      if (fourier_normal) then
        if (fourier_npoles.lt.fourier_nmax) then
          fourier_npoles=fourier_npoles+1
          fourier_poles(fourier_npoles)=pole
        else
          stop 'ERROR (fourier_pole): too many poles'
        endif
      else
        if (fourier_nzeros.lt.fourier_nmax) then
          fourier_nzeros=fourier_nzeros+1
          fourier_zeros(fourier_nzeros)=pole
        else
          stop 'ERROR (fourier_pole): too many zeros'
        endif
      endif
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_zero(zero)
c
c set one zero
c
      double complex zero
c
      include 'polesnzeros.inc'
c
cE
c
      if (fourier_normal) then
        if (fourier_nzeros.lt.fourier_nmax) then
          fourier_nzeros=fourier_nzeros+1
          fourier_zeros(fourier_nzeros)=zero
        else
          stop 'ERROR (fourier_zero): too many zeros'
        endif
      else
        if (fourier_npoles.lt.fourier_nmax) then
          fourier_npoles=fourier_npoles+1
          fourier_poles(fourier_npoles)=zero
        else
          stop 'ERROR (fourier_zero): too many poles'
        endif
      endif
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_numer(factor)
c
c set one numerator factor
c
      double precision factor
c
      include 'polesnzeros.inc'
c
cE
c
      if (fourier_normal) then
        fourier_numer=factor*fourier_numer
      else
        fourier_denom=factor*fourier_denom
      endif
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_denom(factor)
c
c set one denominator factor
c
      double precision factor
c
      include 'polesnzeros.inc'
c
cE
c
      if (fourier_normal) then
        fourier_denom=factor*fourier_denom
      else
        fourier_numer=factor*fourier_numer
      endif
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_cnumer(factor)
c
c set one complex numerator factor
c
      double complex factor
c
      include 'polesnzeros.inc'
c
cE
c
      if (fourier_normal) then
        fourier_numer=factor*fourier_numer
      else
        fourier_denom=factor*fourier_denom
      endif
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      subroutine fou_cdenom(factor)
c
c set one complex denominator factor
c
      double complex factor
c
      include 'polesnzeros.inc'
c
cE
c
      if (fourier_normal) then
        fourier_denom=factor*fourier_denom
      else
        fourier_numer=factor*fourier_numer
      endif
c
      return
      end
c
cS
c----------------------------------------------------------------------
c
      double complex function fou_eval(om)
c
c evaluate filter response coefficient at real frequency om
c the function applies a waterlevel of 1.d-30 to om
c
      double precision om
c
      include 'polesnzeros.inc'
c
cE
      double complex value, thenumer, thedenom, omega
      integer i
c 
      omega=max(1.d-30,om)
c 
      thenumer=(1.d0,0.d0)
      do i=1,fourier_nzeros
        thenumer=thenumer*(omega-fourier_zeros(i))
      enddo
c 
      thedenom=(1.d0,0.d0)
      do i=1,fourier_npoles
        thedenom=thedenom*(omega-fourier_poles(i))
      enddo
c 
      value=thenumer*fourier_numer/(fourier_denom*thedenom)
c
      fou_eval=value
c
      return
      end
c
c ----- END OF Fpolesnzeroes.f ----- 
