c this is <sousou_prepare.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c some routines to prepare the input dataset
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
c    17/11/98   V1.0   Thomas Forbriger
c    23/02/99   V1.1   a factor of two was missing
c
c==============================================================================
c
      subroutine ampscale
c
c scale spectra amplitudes to maximum of one
c
      include 'sousou_dim.inc'
      include 'sousou_data.inc'
      include 'sousou_workspace.inc'
      include 'sousou_options.inc'
c 
      real factor
      integer limit,j,i,idx
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'scale to amplitude of 1.'
      endif
c 
      limit=nspecsamp/2+1
      do i=1,ntraces
        factor=0.
        do j=1,limit
          idx=(i-1)*nspecsamp+j
          factor=max(factor,abs(spectra(idx)))
        enddo
        do j=1,nspecsamp
          idx=(i-1)*nspecsamp+j
          spectra(idx)=spectra(idx)/factor
        enddo
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine inpspectra
c
c call spectra from input data
c
      include 'sousou_dim.inc'
      include 'sousou_data.inc'
      include 'sousou_workspace.inc'
      include 'sousou_options.inc'
c 
      integer i,power2,maxinpsamp,j
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'calculate input dataset spectra'
      endif
c 
      maxinpsamp=0
      do i=1,ntraces
        maxinpsamp=max(maxinpsamp,nsamples(i))
      enddo
c 
      if (opt_verbose.gt.0)
     &  print *,'  longest trace has ',maxinpsamp,' samples'
c
      power2=0
      nspecsamp=2**power2
      do while (nspecsamp.lt.maxinpsamp)
        power2=power2+1
        nspecsamp=2**power2
      enddo
c
      if (opt_verbose.gt.0)
     &  print *,'  need ',nspecsamp ,' samples per spectrum'
c 
      if ((ntraces*nspecsamp).gt.maxsamples) then
        print *,'ERROR: this exceeds a total number of ',maxsamples,' samples'
        stop
      endif
c 
      if (nspecsamp.gt.maxspec) 
     &  stop 'ERROR: we are not prepared to perform a back transform'
c
      if (opt_verbose.gt.0)
     &  print *,'  calculating spectra...'
c 
      do i=1,ntraces
        do j=1,nspecsamp
          spectra((i-1)*nspecsamp+j)=(0.,0.)
        enddo
        do j=1,nsamples(i)
          spectra((i-1)*nspecsamp+j)=data(firstsample(i)+j-1)
        enddo
        call tf_fork(nspecsamp,spectra((i-1)*nspecsamp+1), -1.)
      enddo
c 
      df=1./(nspecsamp*dt(1))
c
      if (opt_verbose.gt.0)
     &  print *,'  done'
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine hpfilter
c
c apply high pass filter to get sound wave
c
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_data.inc'
      include 'sousou_workspace.inc'
c 
      real freq
      complex coeff, hpbutcoeff
      integer i,j,idx,limit
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'apply zero phase high pass butterworth filter'
        print *,'  of order ',opt_hpord,' at ',opt_hpfreq,'Hz'
      endif
c
      limit=nspecsamp/2+1
      do j=1,limit
        freq=df*(j-1)
        coeff=hpbutcoeff(freq,opt_hpfreq,opt_hpord)
c phase free
        coeff=abs(coeff)
        do i=1,ntraces
          idx=(i-1)*nspecsamp+j
          spectra(idx)=spectra(idx)*coeff
        enddo
        if ((j.gt.1).and.(j.lt.limit)) then
          coeff=conjg(coeff)
          do i=1,ntraces
            idx=i*nspecsamp-j+2
            spectra(idx)=spectra(idx)*coeff
          enddo
        endif
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine lpfilter
c
c apply inverse phase low pass filter to make anti alias filter phase-free
c
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
      include 'sousou_data.inc'
c 
      real freq
      complex coeff, lpbutcoeff
      integer i,j,idx,limit
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'apply inverse phase low pass butterworth filter'
        print *,'  of order ',opt_lpord,' at ',opt_lpfreq,'Hz'
      endif
c
      limit=nspecsamp/2+1
      do j=1,limit
        freq=df*(j-1)
        coeff=lpbutcoeff(freq,opt_lpfreq,opt_lpord)
c inverse phase
        coeff=conjg(coeff)
        do i=1,ntraces
          idx=(i-1)*nspecsamp+j
          spectra(idx)=spectra(idx)*coeff
        enddo
        if ((j.gt.1).and.(j.lt.limit)) then
          coeff=conjg(coeff)
          do i=1,ntraces
            idx=i*nspecsamp-j+2
            spectra(idx)=spectra(idx)*coeff
          enddo
        endif
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c 
      complex function hpbutcoeff(omega, omega0, n)
c
c butterworth high pass impulse response coefficient
c
c omega:    angular frequency to evaluate at
c omega0:   eigenfrequency of filter
c n:        oder of filter
c 
      real omega, omega0
      integer n
c
      complex pole, ime, coeff
      real pi, omegan
      integer i
      parameter (ime=(0.,1.),pi=3.1415927)
c 
      coeff=1.
      omegan=omega/omega0
      do i=1,n
        pole=cexp(ime*pi*(float(i)-0.5)/float(n))
        coeff=coeff*omegan/(omegan-pole)
      enddo
c      print *,omega,omega0,ime,coeff
      hpbutcoeff=coeff
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
      complex function lpbutcoeff(omega, omega0, n)
c
c butterworth low pass impulse response coefficient
c
c omega:    angular frequency to evaluate at
c omega0:   eigenfrequency of filter
c n:        oder of filter
c 
      real omega, omega0
      integer n
c
      complex pole, ime, coeff
      real pi, omegan
      integer i
      parameter (ime=(0.,1.),pi=3.1415927)
c 
      coeff=1.
      omegan=omega/omega0
      do i=1,n
        pole=cexp(ime*pi*(float(i)-0.5)/float(n))
        coeff=coeff*ime/(omegan-pole)
      enddo
      lpbutcoeff=coeff
c 
      return
      end
c 
c----------------------------------------------------------------------
c 
      subroutine envelope
c
c calculate envelope
c 23/02/99 a factor of two was missing
c
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
      include 'sousou_data.inc'
c 
      integer limit,j,i,idx
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'calculate envelope'
      endif
c 
      limit=nspecsamp/2+1
      do i=1,ntraces
        do j=limit+1,nspecsamp
          idx=(i-1)*nspecsamp+j
          spectra(idx)=(0.,0.)
        enddo
        idx=(i-1)*nspecsamp+1
        call tf_fork(nspecsamp,spectra(idx),1.)
        do j=1,nspecsamp
          idx=(i-1)*nspecsamp+j
          spectra(idx)=cmplx(2.d0*abs(spectra(idx)), 0.)
        enddo
        idx=(i-1)*nspecsamp+1
        call tf_fork(nspecsamp,spectra(idx),-1.)
      enddo
c 
      return
      end
c
c ----- END OF sousou_prepare.f -----
