c this is <tt_prepmod.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c calculate additional lengths describing model
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
c anything that is independent of the ray geometry
c
c REVISIONS and CHANGES
c    16/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine tt_prepmod(verbose)
c
c declare parameters
      include 'tt_dim.inc'
      include 'tt_model.inc'
      include 'tt_work.inc'
      logical verbose
c
cE
c declare local variables
      real pi
      parameter(pi=3.14159265358979311599796)
      integer i
c
c------------------------------------------------------------------------------
c 
c there will several job be done here
c 
c calculate cmp positions and absolute dipping angles
      absphi(1)=phi(1)
      cmpx(1)=0.5*backoff*cos(absphi(1))
      cmpz(1)=-0.5*backoff*sin(absphi(1))
      do i=2,nlay
        absphi(i)=absphi(i-1)+phi(i)
        cmpz(i)=cmpz(i-1)+d(i-1)*cos(absphi(i))
        cmpx(i)=cmpx(i-1)+d(i-1)*sin(absphi(i))
      enddo
c 
c calculate cmplength on interface
      cmp(1)=0.5*backoff
      do i=2,nlay
        cmp(i)=cmp(i-1)*cos(phi(i))
      enddo
c
c calculate plumbline length below shotpoints
      do i=1,nlay-1
        d1(i)=d(i)+cmp(i)*sin(phi(i+1))
        d2(i)=d(i)-cmp(i)*sin(phi(i+1))
c        print *,d(i),d1(i),d2(i)
      enddo
c 
c be verbose
      if (verbose) then
        print 50,'i','CMPx [m]','CMPz [m]','CMP [m]','absphi [°]'
        do i=1,nlay
          print 51,i,cmpx(i),cmpz(i),cmp(i),absphi(i)*180./pi
        enddo
        print 52,backoff,'i','d1 [m]','d2 [m]'
        do i=1,nlay
          print 53,i,d1(i),d2(i)
        enddo
      endif
c 
      return
   50 format(/'prepared values:'/a5,4(a10,2x))
   51 format(i5,4(f10.4,2x))
   52 format(/'back offset: ',f10.4,'m',/a5,2(a10,2x))
   53 format(i5,2(f10.4,2x))
      end
c
c ----- END OF tt_prepmod.f -----
