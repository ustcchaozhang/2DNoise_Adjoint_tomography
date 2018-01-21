c this is <grepg_poly.f>
c------------------------------------------------------------------------------
cS
c
c 07/04/2000 by Thomas Forbriger (IfG Stuttgart)
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
c remove polynomial trend
c
c REVISIONS and CHANGES
c    07/04/2000   V1.0   Thomas Forbriger
c    10/04/2000   V1.1   select fitting of real and imag part
c
c==============================================================================
c
      subroutine grepg_poly(no,ns,nf,d,v, ms, mf,sel)
c
c declare parameters
      integer no, ns, nf, ms, mf, sel
      complex d(ms,mf)
      logical v
c
cE
c declare local variables
      real a,b
      double precision nh, sum, msum, legendre
      integer i,j,n, msel
c
c------------------------------------------------------------------------------
c go
      if (v) print *, 'remove polynomial trend'
      msel=sel
c
c here we remove a polynomial trend for marine data
      n=max(0,min(6,no))
      nh=dble(ns)/2.d0
      do n=0,n
        if (mod(msel,2).ne.0) then
          do j=1,nf
            sum=0.d0
            do i=1,ns
              sum=sum+legendre(n,(dble(i)/nh-1.d0))*real(d(i,j))
c              print *,n,i,j,sum,lp(n,(dble(i)/nh-1.d0)),d(i,j),abs(d(i,j))
            enddo
            sum=sum/nh
            do i=1,ns
              a=real(d(i,j))
              b=imag(d(i,j))
              a=a-sngl(sum*legendre(n,(dble(i)/nh-1.d0)))
              d(i,j)=cmplx(a,b)
            enddo
            msum=msum+sum
          enddo
        endif
        msel=int(msel/2)
        if (mod(msel,2).ne.0) then
          msum=0.d0
          do j=1,nf
            sum=0.d0
            do i=1,ns
              sum=sum+legendre(n,(dble(i)/nh-1.d0))*imag(d(i,j))
c              print *,n,i,j,sum,lp(n,(dble(i)/nh-1.d0)),d(i,j),abs(d(i,j))
            enddo
            sum=sum/nh
            do i=1,ns
              a=real(d(i,j))
              b=imag(d(i,j))
              b=b-sngl(sum*legendre(n,(dble(i)/nh-1.d0)))
              d(i,j)=cmplx(a,b)
            enddo
            msum=msum+sum
          enddo
          msum=msum/dble(nf)
        endif
        if (v) print *,'  polynomial order: ',n,' mean coefficient: ',msum
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c legendre polynomials

      double precision function legendre(n,x)
      integer n, nord
      double precision x
      double precision lnorm
      lnorm(nord)=sqrt((2.d0*dble(nord)+1)/2.d0)
      if (n.eq.0) then
        legendre=lnorm(0)
      elseif (n.eq.1) then
        legendre=lnorm(1)*x
      elseif (n.eq.2) then
        legendre=lnorm(2)*0.5d0*(3.d0*(x**2)-1.d0)
      elseif (n.eq.3) then
        legendre=lnorm(3)*0.5d0*(5.d0*(x**3)-3.d0*x)
      elseif (n.eq.4) then
        legendre=lnorm(4)*0.125d0*(35.d0*(x**4)-30.d0*(x**2)+3.d0)
      elseif (n.eq.5) then
        legendre=lnorm(5)*0.125d0*(63.d0*(x**5)-70.d0*(x**3)+15.d0*x)
      elseif (n.eq.6) then
        legendre=lnorm(6)*
     &           0.0625d0*(231.d0*(x**6)-315.d0*(x**4)+105.d0*(x**2)-5.d0)
      else
        stop 'illegal polynomial order!'
      endif
      return
      end
c
c ----- END OF grepg_poly.f -----
