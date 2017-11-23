c this is <phasedsignals.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
c
c compute synthetic signals with given phase
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
c    09/11/2004   V1.0   Thomas Forbriger
c    17/04/2009   V1.1   use GSL rng
c
c ============================================================================
c
      program phasedsignals
c
      character*(*) version
      parameter(version=
     &  'PHASEDSIGNALS   V1.1   '
     &  //'compute synthetic signals with given phase')
      character*(*) PHASEDSIGNALS_CVS_ID
      parameter(PHASEDSIGNALS_CVS_ID=
     &  '$Id$')
c
c 
      logical overwrite, arg_usedamp
      character arg_model
      double precision arg_duration, arg_delay, arg_bandwidth
      double precision arg_damp
      integer arg_npower, arg_nord, arg_npo
c 
      integer nsamples, maxsamples, maxfreq, nfreq, nord, npo
      parameter(maxsamples=100000,maxfreq=((maxsamples/2)+1))
      double precision dt, t0, df, pi, pi2, f, bw, fac
      parameter(pi=3.141592653589793d0,pi2=2.d0*pi)
      double precision amp(maxfreq), phase(maxfreq)
      double precision sigtospec, sigtotime, calfac, damp
      parameter (sigtospec=-1.d0, sigtotime=1.d0)
      real fdata(maxsamples), tf_rand
      integer idata(maxsamples)
      equivalence (fdata,idata)
      double complex spectrum(maxsamples), ime, pfac
      double complex calspec(maxsamples), lp
      complex tf_lpbut
      parameter(ime=(0.d0,1.d0))
      integer lu, i
      parameter(lu=12)
      character*80 filename
c debugging
      logical debug, verbose
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=11)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-o, 2h-p, 2h-t, 2h-n, 2h-s, 2h-b, 2h-O,
     &           2h-N, 2h-h/
      data opthasarg/3*.FALSE.,8*.TRUE./
      data optarg/3*1h-,1h0,2h1.,2h10,2h0.,2h1.,1h4,1h1,2h.7/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: phasedsignals outfile [-o] [-p 0|m|r] [-O o]'
        print *,'                     [-t d] [-n p] [-s t] [-b bw]'
        print *,'                     [-N o] [-h h]'
        print *,'   or: phasedsignals -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'outfile      output sff data file'
        print *,' '
        print *,'-o           overwrite'
        print *,'-p 0|m|r     select phase model'
        print *,'             0: zero phase'
        print *,'             m: minimum delay'
        print *,'             r: random phase'
        print *,'-t d         duration'
        print *,'-n p         number so samples = 2**p'
        print *,'-s t         shift data by t'
        print *,'-b bw        set bandwidth'
        print *,'-O o         use low pass repsonse of order ''o'' '
        print *,'-N o         take filter coeff. to the power of ''o'' '
        print *,'-h h         take second order filter with damping'
        print *,'             ''h'' of critical'
        print *,' '
        print *,PHASEDSIGNALS_CVS_ID
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call getarg(1, filename)
      call tf_cmdline(2, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      overwrite=optset(3)
      arg_model=optarg(4)
      read(optarg(5), *) arg_duration
      read(optarg(6), *) arg_npower
      read(optarg(7), *) arg_delay
      read(optarg(8), *) arg_bandwidth
      read(optarg(9), *) arg_nord
      read(optarg(10), *) arg_npo
      read(optarg(11), *) arg_damp
      arg_usedamp=optset(11)
c
c------------------------------------------------------------------------------
c go
      nsamples=2**arg_npower
      if (nsamples.gt.maxsamples)
     &  stop 'ERROR: you request too many samples!'
      t0=arg_delay
      dt=arg_duration/nsamples
      df=1./arg_duration
      nfreq=(nsamples/2)+1
      bw=arg_bandwidth
      nord=arg_nord
      npo=arg_npo
c
      if (verbose) then
        print *,'prepare spectrum of Butterworth Lowpass'
        print *,'at ',bw,' Hz with order ',nord
      endif
c 
      do i=1,nfreq
        f=(i-1)*df
        if (arg_usedamp) then
          spectrum(i)=lp(f,bw,arg_damp)**npo
        else
          spectrum(i)=tf_lpbut(sngl(f),sngl(bw),nord)**npo
        endif
        amp(i)=abs(spectrum(i))
        calspec(i)=spectrum(i)
      enddo
c 
      if (verbose) print *,'prepare calibration factor'
      call ccspectrum(calspec,nsamples)
      call tf_dfork(nsamples,calspec,sigtotime)
      calfac=0.d0
      do i=1,nsamples
        calfac=max(calfac,abs(calspec(i)))
      enddo
c
      if (arg_model.eq.'0') then
        if (verbose) print *,'prepare zero phase'
        do i=1,nfreq
          phase(i)=0.d0
        enddo
      elseif (arg_model.eq.'r') then
        if (verbose) print *,'prepare random phase'
c        call tf_tsrand()
        call tf_gsl_rng_uniform(phase, nfreq)
        do i=1,nfreq
          phase(i)=pi2*phase(i)
c          phase(i)=pi2*tf_rand()
c          print *,i,phase(i)
        enddo
      endif
c
      if ((arg_model.eq.'0').or.(arg_model.eq.'r')) then
        if (verbose) print *,'apply new phase'
        do i=1,nfreq
          pfac=exp(ime*phase(i))
          spectrum(i)=amp(i)*pfac
        enddo
      endif
c 
      if (verbose) print *,'apply delay'
      do i=1,nfreq
        f=(i-1)*df
        phase(i)=-f*pi2*t0
        pfac=exp(ime*phase(i))
        spectrum(i)=spectrum(i)*pfac
      enddo
c 
      if (verbose) print *,'complement spectrum'
      call ccspectrum(spectrum,nsamples)
      if (verbose) print *,'transform spectrum to time domain'
      call tf_dfork(nsamples,spectrum,sigtotime)
      fac=sqrt(float(nsamples))*df*pi2/calfac
      fac=1.d0/calfac
      if (verbose) print *,'normalize signal'
      do i=1,nsamples
        fdata(i)=real(spectrum(i))*fac
      enddo
      if (overwrite) call sff_New(lu,filename,i)
      if (verbose) print *,'open file ',
     &   filename(1:index(filename,' ')-1)
      call sffu_simpleopen(lu,filename)
      if (verbose) print *,'write file'
      call sffu_simplewrite(lu, .true., fdata, nsamples, 
     &                      sngl(dt), sngl(t0))
      if (verbose) print *,'finished'
c
      stop
      end
c======================================================================
c
      subroutine ccspectrum(spectrum, n)
c 
c complement the spectrum
c
      double complex spectrum(n)
      integer n,i
      integer n2
      n2=n/2
      do i=2,n2
        spectrum(n-i+2)=conjg(spectrum(i))
      enddo
      return
      end

c----------------------------------------------------------------------

      double complex function lp(omega,omegan,h)
      double precision omega,omegan,h
      double complex res, ime
      parameter (ime=(0.d0,1.d0))
      double precision q,rt
      q=omega/omegan
      rt=sqrt(1-h**2)
      res=-1.d0/((q-ime*h-rt)*(q-ime*h+rt))
      lp=res
      return
      end

c
c ----- END OF phasedsignals.f ----- 
