c this is <seife_rfk.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 1984 by Erhard Wielandt
c This code was part of seife.f. A current version of seife.f can be obtained
c from http://www.software-for-seismometry.de/
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
c extracted from libseife.f
c
c REVISIONS and CHANGES
c    25/10/2000   V1.0   Thomas Forbriger
c    11/07/2005   V1.1   added block data subroutine
c
c==============================================================================
c
      subroutine seife_rfk(it,t0,h,t0s,hs,f0,f1,f2,g1,g2)
c  determine coefficients for recursive filter
      implicit real*8 (a-h,o-z)
      data zero,one,two,four,eight/0.d0,1.d0,2.d0,4.d0,8.d0/
      if(it.ne.1) goto 10
      f0=one/two/t0
      f1=f0
      f2=zero
      g1=one
      g2=zero
      return
   10 zpi=eight*datan(one)
      eps=zpi/t0
      f2=zero
      g2=zero
      if(it.gt.20) goto 20
      g1=(two-eps)/(two+eps)
      if(it.gt.11) goto 12
      f0=eps/(two+eps)
      f1=f0
      goto 14
   12 if(it.gt.12) goto 13
      f0=two/(two+eps)
      f1=-f0
      goto 14
   13 if(it.gt.13) return
      epss=zpi/t0s
      f0=(epss+two)/(eps+two)
      f1=(epss-two)/(eps+two)
   14 return
   20 epsq=eps*eps
      a=one-eps*h+epsq/four
      b=-two+epsq/two
      c=one+eps*h+epsq/four
      g1=-b/c
      g2=-a/c
      if(it.gt.21) goto 22
      f0=epsq/four/c
      f1=f0+f0
      f2=f0
      goto 25
   22 if(it.gt.22) goto 23
      f0=one/c
      f1=-f0-f0
      f2=f0
      goto 25
   23 if(it.gt.23) goto 24
      epss=zpi/t0s
      epssq=epss*epss
      as=one-epss*hs+epssq/four
      bs=-two+epssq/two
      cs=one+epss*hs+epssq/four
      f0=cs/c
      f1=bs/c
      f2=as/c
      goto 25
   24 if(it.gt.24) return
      f0=eps/two/c
      f1=zero
      f2=-f0
   25 return
      end
c 
c----------------------------------------------------------------------
c 
c here we place the blockdata statement to force the linker 
c (see seife_common.inc)
c
      block data seife_init_global
      include 'seife_common.inc'
      data ldebug / .false. /
      end
c     
c ----- END OF seife_rfk.f -----
