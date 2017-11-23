c this is <tf_gauss.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1984 by Erhard Wielandt
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c solve a set of linear equations
c
c This routine was originally written by Erhard Wielandt
c It is contained in the program seife published at
c http://www.software-for-seismometry.de
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
c    04/08/97   V1.0   Thomas Forbriger
c    17/12/07   V1.1   index is the name of an intrinsic function and is
c                      therefore replaced by iindex
c
c==============================================================================
cS
c
c unchanged original subroutine
c
      subroutine tf_gauss(aik,m,rs,f)
c 
c I believe that this routine should solve
c
c   RS_i = A_ik F_k
c
c   with Aik = aik(i,k)
c
      dimension aik(12,12),rs(12),f(12),h(13),imax(12)
      real aik, rs, f, h
      integer imax, m
c 
cE 
      integer j, k, iindex, l
      real aikmax, q
c 
      iindex=0
      do 1401 j=1,m
      aikmax=0.
      do 1402 k=1,m
      h(k)=aik(j,k)
      if(abs(h(k)).le.aikmax) go to 1402
      aikmax=abs(h(k))
      iindex=k
 1402 continue
      h(m+1)=rs(j)
      do 1403 k=1,m
      q=aik(k,iindex)/h(iindex)
      do 1404 l=1,m
 1404 aik(k,l)=aik(k,l)-q*h(l)
 1403 rs(k)=rs(k)-q*h(m+1)
      do 1405 k=1,m
 1405 aik(j,k)=h(k)
      rs(j)=h(m+1)
 1401 imax(j)=iindex
      do 1406 j=1,m
      iindex=imax(j)
 1406 f(iindex)=rs(j)/aik(j,iindex)
      return
      end
c----------------------------------------------------------------------
cS
c
c real
c
      subroutine tf_dgauss(aik, m, rs, f, mmax)
c 
      integer m, mmax
      double precision aik(mmax,mmax),rs(mmax),f(mmax)
c 
cE
      integer immax
      parameter (immax=100)
      double precision h(immax) 
      integer imax(immax)
c 
      integer j, k, iindex, l
      double precision aikmax, q
c 
      if (m.ge.immax) stop 'ERROR (tf_dcgauss): increase array dimensions!'
c 
      iindex=0
      do 1401 j=1,m
      aikmax=0.
      do 1402 k=1,m
      h(k)=aik(j,k)
      if(abs(h(k)).le.abs(aikmax)) go to 1402
      aikmax=abs(h(k))
      iindex=k
 1402 continue
      h(m+1)=rs(j)
      do 1403 k=1,m
      q=aik(k,iindex)/h(iindex)
      do 1404 l=1,m
 1404 aik(k,l)=aik(k,l)-q*h(l)
 1403 rs(k)=rs(k)-q*h(m+1)
      do 1405 k=1,m
 1405 aik(j,k)=h(k)
      rs(j)=h(m+1)
 1401 imax(j)=iindex
      do 1406 j=1,m
      iindex=imax(j)
 1406 f(iindex)=rs(j)/aik(j,iindex)
      return
      end
c----------------------------------------------------------------------
cS
c
c real
c
      subroutine tf_rgauss(aik, m, rs, f, mmax)
c 
      integer m, mmax
      real aik(mmax,mmax),rs(mmax),f(mmax)
c 
cE
      integer immax
      parameter (immax=100)
      real h(immax) 
      integer imax(immax)
c 
      integer j, k, iindex, l
      real aikmax, q
c 
      if (m.ge.immax) stop 'ERROR (tf_dcgauss): increase array dimensions!'
c 
      iindex=0
      do 1401 j=1,m
      aikmax=0.
      do 1402 k=1,m
      h(k)=aik(j,k)
      if(abs(h(k)).le.abs(aikmax)) go to 1402
      aikmax=abs(h(k))
      iindex=k
 1402 continue
      h(m+1)=rs(j)
      do 1403 k=1,m
      q=aik(k,iindex)/h(iindex)
      do 1404 l=1,m
 1404 aik(k,l)=aik(k,l)-q*h(l)
 1403 rs(k)=rs(k)-q*h(m+1)
      do 1405 k=1,m
 1405 aik(j,k)=h(k)
      rs(j)=h(m+1)
 1401 imax(j)=iindex
      do 1406 j=1,m
      iindex=imax(j)
 1406 f(iindex)=rs(j)/aik(j,iindex)
      return
      end
c----------------------------------------------------------------------
cS
c
c double complex 
c
      subroutine tf_dcgauss(aik, m, rs, f, mmax)
c 
      integer m, mmax
      double complex aik(mmax,mmax),rs(mmax),f(mmax)
c 
cE
      integer immax
      parameter (immax=100)
      double complex h(immax) 
      integer imax(immax)
c 
      integer j, k, iindex, l
      double complex aikmax, q
c 
      if (m.ge.immax) stop 'ERROR (tf_dcgauss): increase array dimensions!'
c 
      iindex=0
      do 1401 j=1,m
      aikmax=0.
      do 1402 k=1,m
      h(k)=aik(j,k)
      if(abs(h(k)).le.abs(aikmax)) go to 1402
      aikmax=abs(h(k))
      iindex=k
 1402 continue
      h(m+1)=rs(j)
      do 1403 k=1,m
      q=aik(k,iindex)/h(iindex)
      do 1404 l=1,m
 1404 aik(k,l)=aik(k,l)-q*h(l)
 1403 rs(k)=rs(k)-q*h(m+1)
      do 1405 k=1,m
 1405 aik(j,k)=h(k)
      rs(j)=h(m+1)
 1401 imax(j)=iindex
      do 1406 j=1,m
      iindex=imax(j)
 1406 f(iindex)=rs(j)/aik(j,iindex)
      return
      end
c
c ----- END OF tf_gauss.f -----
