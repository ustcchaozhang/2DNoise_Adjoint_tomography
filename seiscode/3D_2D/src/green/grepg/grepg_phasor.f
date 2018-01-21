c this is <grepg_phasor.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c plot Fourier coefficient phasor walkout
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
c    13/09/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine grepg_phasor(freq,slow)
c
      real freq,slow
c
      include 'grepg_fourier.inc'
c 
      complex phasor(0:fmaxtr)
      real x(0:fmaxtr),y(0:fmaxtr)
      real maxx,minx,maxy,miny
      integer i, ifreq
      real freqdist
      complex ime
      parameter (ime=(0.,1.))
c 
      real pi2
      parameter (pi2=2.*3.141592653589793)
c 
      print *,'phasor walkout test at ',freq,' Hz and ',slow,' s/km'
c 
      freqdist=fom(fnom)
      ifreq=fnom
      do i=1,fnom
        if (freqdist.gt.abs(freq-fom(i)/pi2)) then
          ifreq=i
          freqdist=abs(freq-fom(i)/pi2)
        endif
      enddo
      print *,'  true frequency: ',fom(ifreq)/pi2,' Hz'
c 
      phasor(0)=(0.,0.)
      x(0)=0.
      y(0)=0.
      minx=0.
      maxx=0.
      miny=0.
      maxy=0.
      do i=1,fntr
        phasor(i)=cmplx(phasor(i-1)+
     &    fourier(i,ifreq)*exp(ime*fom(ifreq)*slow*1.e-3*foffs(i)))
        x(i)=real(phasor(i))
        y(i)=aimag(phasor(i))
        minx=min(minx,x(i))
        maxx=max(maxx,x(i))
        miny=min(miny,y(i))
        maxy=max(maxy,y(i))
      enddo
      call pgenv(minx,maxx,miny,maxy,0,2)
      call pgsave
      call pgline(fntr+1,x,y)
      call pgsci(2)
      call pgline(fntr/2,x,y)
      call pgunsa
c 
      return
      end
c
c ----- END OF grepg_phasor.f ----- 
