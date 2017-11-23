c this is <tt_propagate.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c propagate refracted waves to apparent slowness and intercept times
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
c    17/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine tt_propagate(verbose)
c
c declare parameters
      include 'tt_dim.inc'
      include 'tt_model.inc'
      include 'tt_work.inc'
      logical verbose
c
cE
c declare local variables
      real seta1,seta2,gamma1,gamma2,pi
      parameter(pi=3.1415926535897931159979634685)
      integer i,j
c
c------------------------------------------------------------------------------
c go
      sapp1(1)=1/v(1)
      sapp2(1)=1/v(1)
      ti1(1)=0.
      ti2(1)=0.
      do i=2,nlay
        seta1=1.
        seta2=1.
        ti1(i)=0.
        ti2(i)=0.
        do j=i,2,-1
          gamma1=asin(seta1*v(j-1)/v(j))
          gamma2=asin(seta2*v(j-1)/v(j))
          seta1=sin(gamma1+phi(j))
          seta2=sin(gamma2-phi(j))
          ti1(i)=ti1(i)+d1(j-1)*cos(gamma1)/v(j-1)
          ti2(i)=ti2(i)+d2(j-1)*cos(gamma2)/v(j-1)
        enddo
        sapp1(i)=seta2/v(1)
        sapp2(i)=seta1/v(1)
        ti1(i)=2.*ti1(i)
        ti2(i)=2.*ti2(i)
      enddo
c 
c be verbose
      if (verbose) then
        print 50,'i','p1 [s/km]','Ti1 [ms]','p2 [s/km]','Ti2 [ms]'
        do i=1,nlay
          print 51,i,1.e3*sapp1(i),1.e3*ti1(i),1.e3*sapp2(i),1.e3*ti2(i)
        enddo
      endif
c
      return
   50 format(/'traveltime base values:'/,a5,2(2(a10,2x),2x))
   51 format(i5,2(2(f10.4,2x),2x))
      end
c
c ----- END OF tt_propagate.f -----
