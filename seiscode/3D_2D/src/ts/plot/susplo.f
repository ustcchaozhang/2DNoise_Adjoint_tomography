c this is <susplo.f>
c------------------------------------------------------------------------------
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot sum of spectra
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
c    08/04/99   V1.0   Thomas Forbriger
c    04/12/09   V1.1   use correct DIN notation for units
c
c==============================================================================
      program susplo
      character*70 version
      parameter(version='SUSPLOT   V1.1   plot stacked spectra')
c declare data variables
      integer maxtraces, maxsamples
      parameter(maxtraces=50)
      parameter(maxsamples=100000)
      character*80 infile, device
      integer ntraces
      integer trace, sample
      real data(maxsamples, maxtraces)
      integer idata(maxsamples)
      complex yc(maxsamples)
      real y(maxsamples),x(maxsamples)
      real dt(maxtraces)
      integer nsamp(maxtraces)
c 
      real refdt
      integer refnsamp
c sff
      integer lu, ierr
      parameter(lu=10)
      character timestamp*20, code*10
      character*132 wid2line(maxtraces), line
      real sffversion, tanf
      logical last
c general
      double precision pi
      real frac, tapfrac, tapedge
      parameter (pi=3.14159265358979d0)
      character*10 sfrac
      character header*80
      integer iargc, twoexp
c trace sorting

c----------------------------------------------------------------------
c

c give basic information
      print *,version
      if (iargc().ne.4) then
        print *,'Usage: susplo datafile tapfrac device frac'
        print *,' '
        print *,'  datafile  name of data file (sff format)'
        print *,'  tapfrac   tapering fraction for cosine taper'
        print *,'  device    pgplot device specification'
        print *,'  frac      gives the left fraction of the frequency'
        print *,'            axis that is to be plotted'
        print *,' '
        print *,'   maxtraces: ',maxtraces
        print *,'  maxsamples: ',maxsamples
        stop
      endif
      call getarg(1, infile)
      call getarg(2, sfrac)
      read(sfrac, *) tapfrac
      call getarg(3, device)
      call getarg(4, sfrac)
      read(sfrac, *) frac
c      print *,'infile ',infile
c      print *,'tapfrac ',tapfrac
c      print *,'device ',device
c      print *,'frac ',frac
c----------------------------------------------------------------------
c
c read data file
c
      call sff_ROpen(lu, infile, sffversion, timestamp, code, ierr)
      if (ierr.ne.0) stop 'ERROR: opening data file'
      ntraces=0
    1 continue
        ntraces=ntraces+1
        nsamp(ntraces)=maxsamples
        call sff_RTrace(lu, tanf, dt(ntraces), wid2line(ntraces), 
     &    nsamp(ntraces), y,
     &    idata, code, last, ierr)
        if (ntraces.eq.1) then
          refdt=dt(ntraces)
          refnsamp=nsamp(ntraces)
        else
          if (refdt.ne.dt(ntraces)) stop 'ERROR: sampling interval differs'
          if (refnsamp.ne.nsamp(ntraces)) stop 'ERROR: number of samples differs'
        endif
        do sample=1,nsamp(ntraces)
          data(sample, ntraces)=y(sample)
        enddo
        if (ierr.ne.0) stop 'ERROR: reading data file'
        if (ntraces.eq.maxtraces) then
          print *,'NOTICE: reached limit of traces'
          if (.not.last) then
            close(lu)
            last=.true.
          endif
        endif
        if (.not.last) goto 1

c----------------------------------------------------------------------
c do trace-sorting
      call pgp_setdevice(device,1,1)
      call pgslw(1)
c
c clear array
      do sample=1,maxsamples
        y(sample)=0.
      enddo
c
c go through all traces
      do trace=ntraces,1,-1
        print *,'work on trace ',trace
c clear workspace
        do sample=1,maxsamples
          yc(sample)=(0.,0.)
        enddo
c evaluate next two's exponent to nsamples
          twoexp=1
    2     twoexp=twoexp*2
        if (twoexp.lt.nsamp(trace)) goto 2
        if (twoexp.gt.maxsamples) stop
     &       'ERROR: array sample-dimension too small'
        print *,'use ',twoexp,' samples'
c 
c create x-axis
c
c highest frequency is 1/(2*dt)
c lowest frequency is 0
c but we use only half of the samples in x as the second half is the conj.comp.
        do sample=1,twoexp
          x(sample)=float(sample-1)/dt(trace)/float(twoexp)
        enddo
c
c we need complex data
c perform tapering here
        if (tapfrac.gt.0.01) then
          tapedge=float(nsamp(trace))/tapfrac
          do sample=1,nsamp(trace)
            yc(sample)=cmplx(data(sample,trace), 0.)
            if (float(sample).lt.tapedge) yc(sample)=
     &         yc(sample)*sin(pi*float(sample)/2/tapedge)**2
            if (float(sample).gt.(float(nsamp(trace))-tapedge)) yc(sample)=
     &         yc(sample)*cos(pi*float(nsamp(trace)-sample)/2/tapedge)**2
          enddo
        else
          print *,'tapfrac is below limit - skipped taper'
          do sample=1,nsamp(trace)
            yc(sample)=cmplx(data(sample,trace), 0.)
          enddo
        endif
c
c fill rest to twoexp with zero
        do sample=nsamp(trace)+1,twoexp
          yc(sample)=cmplx(0., 0.)
        enddo
c
c do the fft
        call tf_fork(twoexp,yc,-1.)
c
c now retrieve a real array and the maximum amplitude
        do sample=1,twoexp
          y(sample)=y(sample)+cabs(yc(sample))
        enddo
      enddo
c
c plot it
      do sample=1,twoexp
        y(sample)=y(sample)/ntraces
      enddo
c      line=wid2line(trace)
c      header=infile(1:index(infile,' '))//line(6:39)//line(89:94)
      header=infile(1:index(infile,' '))
      call pgslw(1)
c      call pgsch(sqrt(float(ntraces)))
      call spekplot(maxsamples, 1, int(frac*twoexp/2),
     &    x, y, header)
c
      call pgend
      stop 
      end

c----------------------------------------------------------------------
c
c here we do a plot
c
      subroutine spekplot(maxpts, pmin, pmax, freq, spek, text)
c
c declare variables
      integer maxpts, pmin, pmax
      real spek(maxpts), freq(maxpts)
      character*(*) text
c
c declare variables
      integer sample
      real ymax
c
c norm amplitude to 1
      ymax=0.
      do sample=pmin,pmax
        ymax=max(ymax,spek(sample))
      enddo
      if (ymax.eq.0.) ymax=1.
c
c make a frame for nsamples/2
      call pgenv(freq(pmin),freq(pmax), 0., ymax, 0, 0)
c
c plot the data
      call pgslw(5)
      do sample=pmin,pmax
        call pgmove(freq(sample),0.)
        call pgdraw(freq(sample),spek(sample))
      enddo
      call pgslw(1)
      call pgline(pmax,freq,spek)
c      do sample=1,pmax
c        print *,sample,freq(sample),spek(sample)
c      enddo
      call pgupdt
c
c label data
      call pglab('frequency / Hz',' ',text)
      return
      end


c
c ----- END OF susplo.f -----
