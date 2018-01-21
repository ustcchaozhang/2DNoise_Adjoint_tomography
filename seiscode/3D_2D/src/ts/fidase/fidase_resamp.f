c this is <fidase_resamp.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
cS
c
c do some resampling
c
c REVISIONS and CHANGES
c    25/11/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine resamp(newpow,newdt)
c 
      integer newpow
      real newdt
c 
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_para.inc'
      include 'fidase_workspace.inc'
c 
cE
      real pi, t, sum
      integer newnsamp, newtotalsamps, isamp, itrace, ispec
      integer specbase, limit
      complex ime, expfac, expfacbase
      parameter(pi=3.1415729,ime=(0.,1.))
c 
      if (verbose) then
        print *,' '
        print *,'do some resampling...'
      endif
c 
      newnsamp=2**newpow
      if (verbose) then
        print *,'  new sampling interval: ',newdt,'sec'
        print *,'  new number of samples: ',newnsamp
      endif
c 
      newtotalsamps=newnsamp*ntraces
      if (newtotalsamps.gt.maxsamples)
     &  stop 'ERROR: too many samples in new set!'
c 
      do itrace=1,ntraces
c 
        if ((timeofsample(firstsample(itrace))/dt(itrace)).gt.0.001) then
          print *,'  time of first sample in trace ',itrace,' is not zero!'
          print *,'  we do no time-shifting when resampling!'
        endif
c 
        nspecsamp(itrace)=2
        do while (nspecsamp(itrace).lt.nsamples(itrace))
          nspecsamp(itrace)=nspecsamp(itrace)*2
        enddo
c 
        if (itrace.eq.1) then
          firstinspec(itrace)=1
        else
          firstinspec(itrace)=firstinspec(itrace-1)+nspecsamp(itrace-1)
        endif
c 
        do isamp=1,nspecsamp(itrace)
          if (isamp.gt.nsamples(itrace)) then
            spectra(firstinspec(itrace)+(isamp-1))=(0.,0.)
          else
            spectra(firstinspec(itrace)+(isamp-1))=
     &        cmplx(data(firstsample(itrace)+(isamp-1)),0.)
          endif
        enddo
c 
        call tf_fork(nspecsamp(itrace),spectra(firstinspec(itrace)),-1.)
      enddo
c 
      if (verbose) then
        print *,'  we hold all spectra now'
        print *,'  going to perform resampling DFT (which will be slow)...'
      endif
c 
      do itrace=1,ntraces
        if (verbose) print *,'    transforming trace ',itrace
c
c dt*sqrt(nsamples) is the factor we have to multiply in order to get
c   spectral coefficients corresponding to a foruier integral transform
c
c dt*(nsamples-1) is the time length of the original trace, which is the 
c   recurrence time of the time limited fourier signal
c 
c 1./(dt*nsamples) is the frequency step size
c
        if (newdt.gt.dt(itrace)) then
          print *,'      old sampling interval (',
     &      dt(itrace),'sec) is less than new one!'
          print *,
     &    '      it''s up to you to perform a correct low pass filtering'
        endif
c 
        firstsample(itrace)=newnsamp*(itrace-1)+1
        expfacbase=2.*pi*ime*newdt/(dt(itrace)*float(nspecsamp(itrace)))
        limit=nspecsamp(itrace)/2
        specbase=firstinspec(itrace)
c 
        do isamp=1,newnsamp
          t=(isamp-1)*newdt
          timeofsample(firstsample(itrace)+isamp-1)=t
          expfac=expfacbase*(isamp-1)
          if (t.gt.(dt(itrace)*(nsamples(itrace)-1))) then
            data(firstsample(itrace)+isamp-1)=0.
          else
             sum=0.5*real(spectra(specbase))
             do ispec=1,limit-1
               sum=sum+real(spectra(specbase+ispec)*
     &           cexp(expfac*float(ispec)))
             enddo
             sum=sum+0.5*real(spectra(specbase+limit)*
     &         cexp(expfac*(limit)))
             data(firstsample(itrace)+isamp-1)=
     &         2.*sum/sqrt(float(nspecsamp(itrace)))
          endif
        enddo
c 
        nsamples(itrace)=newnsamp
        dt(itrace)=newdt
      enddo
c 
      return
      end
c
c ----- END OF fidase_resamp.f -----
