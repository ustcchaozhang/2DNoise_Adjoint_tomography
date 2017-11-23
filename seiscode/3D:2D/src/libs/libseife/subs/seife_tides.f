c this is <seife_tides.f>
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
c
c==============================================================================
c
      subroutine seife_tides(nil,par,x,n,dts,msg)
c  remove tides. number of frequencies is automatically chosen according
c  to the total length of the record.
      implicit real*8 (a-h,o-z)
      real*4 dts
      logical nil
      character par*35,msg*(*)
      dimension x(n),a(13,13),rs(13),c(13),d(13),e(13),f(13),
     & omega(6),omeg(6),cor(6)
      data omega/1.93227d0,0.92954d0,2.00000d0,1.00274d0,1.89567d0,
     & 0.89293d0/
      data zero,one,two/0.d0,1.d0,2.d0/
      ndim=13
      dt=dts
      dth=dt/two
      read(par,*) nstep
      if(nstep.eq.0) nstep=300./dts
      nstep=max(nstep,1)
      step=nstep
      tstep2=(step-one)*dth
      tint=tstep2+dth
c  determine the number of frequencies required for a good fit
      dur=n*dt/3600.d0
      nfreq=6
      if(dur.lt.35.d0) nfreq=5
      if(dur.lt.18.d0) nfreq=4
      if(dur.lt.14.d0) nfreq=3
      if(dur.lt. 5.d0) nfreq=2
      nco=2*nfreq+1
      omeg0=8.d0*datan(one)/86400.d0
      do 101 i=1,nfreq
  101 omeg(i)=omeg0*omega(i)
c  determine partial amplitudes by least-squares fit
      do 1 i=1,nco
      rs(i)=0.d0
      do 1 k=1,nco
    1 a(i,k)=0.d0
c  correction for averaging over nstep samples
      do 102 j=1,nfreq
  102 cor(j)=step*dsin(omeg(j)*dth)/dsin(step*omeg(j)*dth)
c  set up system of linear equations
      c(1)=one
      do 2 j=1,n-nstep+1,nstep
      sx=zero
      do 12 jj=j,j+nstep-1
   12 sx=sx+x(jj)
      sx=sx/step
      t=(j-1)*dt+tstep2
      do 103 k=1,nfreq
      c(2*k)  =dcos(omeg(k)*t)
  103 c(2*k+1)=dsin(omeg(k)*t)
      do 2 i=1,nco
      rs(i)=rs(i)+sx*c(i)
      do 2 k=1,nco
    2 a(i,k)=a(i,k)+c(i)*c(k)
c  solve for partial amplitudes
      call seife_gauss(a,nco,ndim,rs,f)
      do 3 j=1,n-nstep+1,nstep
      t=(j-1)*dt+tstep2
c  remove average and tides
      do 104 k=1,nfreq
      k2=2*k
      k3=k2+1
      c(k2)=dcos(omeg(k)*t)
      c(k3)=dsin(omeg(k)*t)
      d(k2)=-omeg(k)*c(k3)
      d(k3)= omeg(k)*c(k2)
      e(k2)=-omeg(k)*d(k3)
  104 e(k3)= omeg(k)*d(k2)
      xgez1=f(1)
      xgez2=zero
      xgez3=zero
      do 105 k=1,nfreq
      k2=2*k
      k3=k2+1
      xgez1=xgez1+cor(k)*(f(k2)*c(k2)+f(k3)*c(k3))
      xgez2=xgez2+cor(k)*(f(k2)*d(k2)+f(k3)*d(k3))
  105 xgez3=xgez3+cor(k)*(f(k2)*e(k2)+f(k3)*e(k3))/two
      if(j.gt.n-2*nstep+1) nstep=n+1-j
      do 3 jj=j,j+nstep-1
      tdif=(jj-j)*dt-tstep2
    3 x(jj)=x(jj)-xgez1-xgez2*tdif-xgez3*tdif*tdif
 	write(6, *)	f(1),(omega(j),f(2*j),f(2*j+1), j=1,nfreq)
      write(msg, '("remove tides with nfreq: ",i10)')nfreq
      nil=.false.
      return
      end
c
c ----- END OF seife_tides.f -----
