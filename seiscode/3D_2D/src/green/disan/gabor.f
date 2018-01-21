c this is <gabor.f>
c------------------------------------------------------------------------------
c
c Copyright 2001,2006 by Thomas Forbriger (IMGF Frankfurt)
c
c calculate gabor matrix
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
c    22/02/2001   V1.0   Thomas Forbriger
c    02/08/2001   V1.1   allow trace order rather than offset order
c    09/09/2004   V1.2   set lower limit of time window
c    13/06/2006   V1.3   provide Hanning taper
c    30.12.2010   V1.4   implemented file format selection
c
c==============================================================================
c
      program gabor
c
      character*(*) version
      parameter(version='GABOR   V1.4   calculate gabor matrix')
c
c input dataset
      character*80 filename, informat
      integer maxtraces, totmaxsamples
      parameter(maxtraces=40, totmaxsamples=4000000)
      integer lu, ierr
      parameter(lu=12)
      real fdata(totmaxsamples)
      integer idata(totmaxsamples)
      equivalence(fdata,idata)
      real toffset(maxtraces), tracedt(maxtraces), roffset(maxtraces)
      integer innsamples(maxtraces), firstsample(maxtraces)
      integer ntraces
c processing dataset
      integer maxsamples, maxnsamples, nsamples, nfny
      integer maxvalues
      parameter(maxsamples=100000,maxvalues=100000)
      integer chain(maxtraces), first
      real dt
      real df
c here follows what we need to hold and write the output data
c
c magic number for binary file identification
      integer magic
      character*4 cmagic
      parameter(cmagic='123G')
c greens function
      integer maxtime,maxom
      parameter(maxtime=300,maxom=maxsamples)
      complex gabormat(maxtime, maxom),process(maxom)
      real tref(maxtime), om(maxom)
c 
c processing
      real fmax,tau,tmax,winfac,stdfac,tmin, factor, now, wlen
      integer ntime, nptraces, nom
      character*80 outbase, outfile
      logical overwrite, traceorder, hanning
      parameter(stdfac=8.)
c
      integer i,j,k,itrace,itime
c constants
      complex ime
      real pi2,hin,pi
      parameter(hin=1.,pi2=2.*3.14159265358979,ime=(0.,1.))
      parameter(pi=3.14159265358979)
c debugging
      logical debug, verbose
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=12)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/'-d', '-v', '-f', '-n', '-o', '-t', '-T', '-w', '-S',
     &           '-m', '-h', '-ty'/
      data opthasarg/2*.FALSE.,2*.TRUE.,.FALSE.,5*.TRUE.,.false.,
     &           .true./
      data optarg/2*'-','100.','100','-','1.','1','1.','1','0.','-',
     &           'sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
c 
      if (argument(1:6).eq.'-xhelp') then
        call sff_help_details
        stop
      endif
c        print *,iargc()
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: gabor datafile outfile [-v] [-d]'
        print *,'             [-f freq] [-n N] [-t tmax] [-o]'
        print *,'             [-T N | -S N] [-h]'
        print *,'             [-w fac] [-m tmin] [-ty f]'
        print *,'   or: gabor -help'
        print *,'   or: gabor -xhelp'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'calculate gabor matrix'
        print *,' '
        print *,'datafile     SFF seismogram data'
        print *,'outfile      basename for gabormatrix output'
        print *,' '
        print *,'-v           be somehow verbose'
        print *,'-d           debug mode'
        print *,' '
        print *,'-f freq      analyse frequencies up to ''freq'' '
        print *,'-n N         use ''N'' time steps'
        print *,'-t tmax      use windows up to time ''tmax'' '
        print *,'-o           '
        print *,     'overwrite mode when writing results to file'
        print *,'-T N         use first ''N'' traces in offset order'
        print *,'-S N         use first ''N'' traces in trace order'
        print *,'-w fac       broaden time windoe by factor ''fac'' '
        print *,'-m tmin      use windows starting at tmin'
        print *,'-h           use Hanning taper rather than Gaussian'
        print *,'-ty f        select input file format'
        print *,' '
        call sff_help_formats
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      read(optarg(3), *) fmax
      read(optarg(6), *) tmax
      read(optarg(4), *) ntime
      nptraces=1
      if (optset(7)) read(optarg(7), *) nptraces
      read(optarg(8), *) winfac
      traceorder=optset(9)
      if (optset(9)) read(optarg(9), *) nptraces
      read(optarg(10), *) tmin
      hanning=optset(11)
      informat=optarg(12)

      overwrite=optset(5)

      if (ntime.gt.maxtime) stop 'ERROR: too many time steps selected'
c
      call getarg(1, filename)
      call getarg(2, outbase)
c------------------------------------------------------------------------------
c go
      call sff_select_input_format(informat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting input file format'
      call sffu_simpleread(lu, filename, maxtraces, totmaxsamples, 
     &    fdata, idata, toffset, tracedt, roffset, innsamples, 
     &    firstsample, ntraces, verbose)
c 
      call tf_rchain(roffset, chain, ntraces, first, 1)
c
c consistency check and copy
      maxnsamples=innsamples(first)
      dt=tracedt(first)
      nptraces=min(ntraces,nptraces) 
      if (nptraces.gt.1) then
        k=first
        do i=1,nptraces-1
          j=k
          k=chain(k)
          if (abs(1.-tracedt(j)/tracedt(k)).gt.1.e-4) then
            print *,'ERROR: trace ',j,' and ',k,' have inconsistent'
            print *,'       sampling intervals'
            stop 'can''t handle that!'
          endif
          maxnsamples=max(maxnsamples,innsamples(k))
          if (innsamples(j).ne.innsamples(k)) then
            print *,'NOTICE: trace ',i,' and ',i+1,' have inconsistent'
            print *,'        numbers of samples'
          endif
        enddo
      endif
c
      nsamples=1
      do while (nsamples.lt.maxnsamples)
        nsamples=nsamples*2
      enddo
      if (nsamples.gt.maxsamples) stop 'ERROR: too many samples'
      df=1./(float(nsamples)*dt)
      nfny=nsamples/2
c
c
c----------------------------------------------------------------------
      tmax=min(tmax,float(nsamples)*dt)
      k=first
c effective window length
      tau=stdfac*winfac*tmax/float(ntime)
      wlen=2.*tau*sqrt(-log(0.5))
      if (hanning) tau=wlen
      if (debug) print *,'tau: ',tau, ' tmax: ',tmax, ' ntime: ',ntime
      nom=fmax/df
      do i=1,nom
        om(i)=df*float(i-1)*pi2
      enddo
      do itime=1,ntime
        tref(itime)=tmin+float(itime-1)*(tmax-tmin)/float(ntime-1)
      enddo
      if (verbose) then
        print *,'analysis parameters:'
        print *,'  number of samples used per trace: ',nsamples
        print *,'     number of timesteps to output: ',ntime
        print *,'   number of frequencies to output: ',nom
        print *,'                 Nyquist frequency: ',nfny*df,' Hz'
        print *,'      smallest frequency to output: ',df,' Hz'
        print *,'       maximum frequency to output: ',fmax,' Hz'
        print *,'             first sample taken at: ',tmin,' s'
        print *,'              last sample taken at: ',tmax,' s'
        print *,'                  window halfwidth: ',wlen,' s'
        if (hanning) then
          print *,'  use Hanning taper'
        else
          print *,'  use Gaussian taper'
          print *,'          Gaussian time constant: ',tau,' s'
        endif
      endif
      do itrace=1,nptraces
        if (traceorder) k=itrace
c----------------------------------------------------------------------
c inner loop, calculates Gabor matrix
        do itime=1,ntime
          do i=1,nsamples
            process(i)=(0.,0.)
          enddo
c copy tapered time series
          do i=1,innsamples(k)
            now=dt*(i-1)
            if (hanning) then
              if (now.lt.(tref(itime)-tau)) then
                factor=0.
c              elseif (now.lt.tref(itime)) then
c                factor=0.5*(1.+cos(pi*(tref(itime)-now)/tau))
              elseif (now.lt.(tref(itime)+tau)) then
                factor=0.5*(1.+cos(pi*(tref(itime)-now)/tau))
              else
                factor=0.
              endif
              if (debug.and.(itime.eq.20).and.(itrace.eq.1)) then
                print *,'DEBUG ',i,' factor= ',factor
              endif
            else
              factor=exp(-1.*((now-tref(itime))/tau)**2)
            endif
            process(i)=cmplx(fdata(i+firstsample(k)-1))*factor
          enddo
c calculate Fourier coefficients
          call tf_fork(nsamples,process,hin)
c store them
          do i=1,nom
            gabormat(itime,i)=process(i)
          enddo
c end of inner loop, this trace's Gabor matrix is ready
c----------------------------------------------------------------------
        enddo
        k=chain(k)
c 
c write green code (easy to use)
c 
        if (nptraces.eq.1) then
          outfile=outbase
        else
          write(outfile, '(a,1h.,i3.3)') outbase(1:index(outbase,' ')-1),itrace
        endif
        print *,' '
        if (overwrite) then
          print *,'opening gabor file ',outfile(1:index(outfile,' ')),
     &      ' - overwrite mode'
          open(lu, file=outfile, form='unformatted', err=98)
        else
          print *,'opening gabor file ',outfile(1:index(outfile,' '))
          open(lu, file=outfile, status='new', form='unformatted', err=98)
        endif
        call tf_magic(cmagic, magic)
        write(lu, err=97) magic
        write(lu, err=97) nom, ntime
        write(lu, err=97) (om(i), i=1,nom), (tref(i), i=1,ntime)
        write(lu, err=97) ((gabormat(j,i), i=1,nom), j=1,ntime)
        close(lu, err=96)
      enddo
c 
      stop
   99 stop 'ERROR: reading command line argument'
   98 stop 'ERROR: opening gabor file'
   97 stop 'ERROR: writing gabor file'
   96 stop 'ERROR: closing gabor file'
      end
c
c ----- END OF gabor.f -----
