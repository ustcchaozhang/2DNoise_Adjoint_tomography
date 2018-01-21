c this is <tt_convgrmod.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c convert model format defined by Gunther Reimann to our format
c
c REVISIONS and CHANGES
c    16/09/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine tt_convgrmod(verbose)
c
c declare parameters
      include 'tt_dim.inc'
      include 'tt_model.inc'
      logical verbose
c
cE
c declare local variables
      real pi
      parameter(pi=3.141592653589793115997963)
      real dgr, cmpoffset, phitop, phibot, phirel
      integer i
c
c------------------------------------------------------------------------------
c 
c we'll do the job in four steps
c 1. convert angles from degree to rad
c    and convert depth to thickness
c 2. we calculate all the CMP d-values
c 3. then we correct for relative dip angles rather than absolute ones
c 4. convert velocities from km/s to m/s
c 
c and remember: the GR-model has a non-dipping free surface
c 
c first of all: we also count the halfspace
      nlay=nlay+1
c
c STEP 1
c ======
      do i=1,nlay
        phi(i)=phi(i)*pi/180.
      enddo
      do i=1,nlay-1
        d(i)=d(i+1)-d(i)
      enddo
c
c STEP 2
c ======
c
c where to find the place along the interface to find the distance to the next
c interface (this will change while walking through the layer stack)
      cmpoffset=0.5*backoff
c use phitop as there is already phi(0) set to the dip angle of the free
c surface
      phitop=0.
      do i=1,nlay-1
        phibot=phi(i+1)
        phirel=phibot-phitop
        dgr=d(i)
        d(i)=cmpoffset*sin(phirel)+dgr*cos(phibot)
c move on values
        phitop=phi(i+1)
        cmpoffset=cmpoffset*cos(phirel)-dgr*sin(phibot)
      enddo
c 
c STEP 3
c ======
      if (nlay.ge.3) then
        do i=nlay,3,-1
          phitop=phi(i-1)
          phibot=phi(i)
          phirel=phibot-phitop
          phi(i)=-phirel
        enddo
      endif
      phi(2)=-phi(2)
c      do i=1,nlay
c        print *,'i, relphi ',i,phi(i)*180./pi
c      enddo
c 
c STEP 4
c ======
      do i=1,nlay
        v(i)=v(i)*1.e3
      enddo
c 
c be verbose
      if (verbose) then
        print 50,'i','d [m]','phi [°]','v [m/s]'
        do i=1,nlay-1
          print 51,i,d(i),phi(i)*180./pi,v(i)
        enddo
        print 52,nlay,phi(nlay)*180./pi,v(nlay)
      endif
c
      return
   50 format(/'converted model:'/a5,3(a10,2x))
   51 format(i5,2(f10.5,2x),f10.2)
   52 format(i5,12x,f10.5,2x,f10.2)
      end
c
c ----- END OF tt_convgrmod.f -----
