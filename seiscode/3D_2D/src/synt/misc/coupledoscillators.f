c this is <coupledoscillators.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
c
c calculate response of two coupled mass-spring oscillators
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
c    03/11/2008   V1.0   Thomas Forbriger
c    17/12/2008   V1.1   there was a factor sqrt(2*pi) missing in all sysmtem
c                        frequencies
c
c ============================================================================
c
      program coupledoscillators
c
      character*(*) version
      parameter(version=
     & 'COUPLEDOSCILLATORS   V1.0  two coupled mass-spring oscillators')
      character*(*) COUPLEDOSCILLATORS_CVS_ID
      parameter(COUPLEDOSCILLATORS_CVS_ID=
     &'$Id$')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=20)
      character*4 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      character*80 outfile
      integer lu
      parameter(lu=10)
      logical last
c 
      double precision pi
      double complex ime
      parameter(pi= 3.14159265358979)
      parameter(ime=(0.d0,1.d0))
c 
      double precision tlen,dt,du1,df,fny,omny,dom,fre,om,om2,du2
      double precision f1,f2,Q1,Q2,fK1,fK2,QK,a1,a2,del1,del2
      double precision f02,fd2,fr1,fr2,k4,tvar,avg1,avg3,eps
      integer der1,der2
c 
      double complex in1,in2,det1,det2,det
      double complex m11,m12,m21,m22
      double complex im11,im12,im21,im22
      double complex om12,om22,omK12,omK22
      integer i,k,ierr
c
      integer nsamp,msamp
      parameter(msamp=4100*16)
      double complex spec(msamp,8)
      double complex strans(msamp)
      double complex src1(msamp), src2(msamp)
      real data(msamp)
c debugging
      logical verbose, overwrite
c here are the keys to our commandline options
      data optid/'-o', '-v', '-f1', '-f2', '-Q1', '-Q2', 
     &  '-fK1', '-fK2', '-QK',
     &  '-a1', '-a2', '-T', '-t', '-du1', '-d1', '-d2', '-du2', 
     &  '-D1', '-D2', '-e'/
      data opthasarg/2*.FALSE.,18*.TRUE./
      data optarg/2*'-','1.','1.','20.','20.','0.5','0.5','20.',
     &  '1.','0.','100.','0.01','0.2','0.','0.', '0.2', '0', '0','0.01'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: coupledoscillators'
        print *,'       [-v] [-o] [-T T] [-t t] [-du1 t] [-du2 t]'
        print *,'       [-f1 f] [-f2 f] [-Q1 Q] [-Q2 Q]'
        print *,'       [-fK1 f] [-fK2 f] [-QK Q]'
        print *,'       [-a1 a] [-a2 a] [-d1 d] [-d2 d]'
        print *,'       [-D1 n] [-D2 n] [-e eps]'
        print *,'       outfile'
        print *,'   or: coupledoscillators -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: missing file name'
        print *,' '
        print *,'outfile      SFF output file'
        print *,' '
        call usage
        print *,' '
        print *,COUPLEDOSCILLATORS_CVS_ID
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      overwrite=optset(1)
      verbose=optset(2)
      read(optarg(3), *, err=99) f1
      read(optarg(4), *, err=99) f2
      read(optarg(5), *, err=99) Q1
      read(optarg(6), *, err=99) Q2
      read(optarg(7), *, err=99) fK1
      read(optarg(8), *, err=99) fK2
      read(optarg(9), *, err=99) QK
      read(optarg(10), *, err=99) a1
      read(optarg(11), *, err=99) a2
      read(optarg(12), *, err=99) tlen
      read(optarg(13), *, err=99) dt
      read(optarg(14), *, err=99) du1
      read(optarg(15), *, err=99) del1
      read(optarg(16), *, err=99) del2
      read(optarg(17), *, err=99) du2
      read(optarg(18), *, err=99) der1
      read(optarg(19), *, err=99) der2
      read(optarg(20), *, err=99) eps
      if (lastarg.ge.iargc()) stop 'ERROR: missing file name'
      call getarg(iargc(), outfile)
c
c------------------------------------------------------------------------------
c go
      if (verbose) then
        print *,version
        print *,COUPLEDOSCILLATORS_CVS_ID
        print *,'system parameters:'
        print 50,'f1: ',f1,'Hz'
        print 50,'f2: ',f1,'Hz'
        print 50,'Q1: ',Q1,'  '
        print 50,'Q2: ',Q2,'  '
        print 50,'fK1: ',fK1,'Hz'
        print 50,'fK2: ',fK2,'Hz'
        print 50,'QK: ',qK,' '
        print *,'input parameters:'
        print 50,'a1: ',a1,'m/s**2'
        print 50,'a2: ',a2,'m/s**2'
        print 50,'d1: ',del1,'s'
        print 50,'d2: ',del2,'s'
        print 50,'du1: ',du1,'s'
        print 50,'du2: ',du2,'s'
        print 51,'D1: ',der1,' '
        print 51,'D2: ',der2,' '
        print 50,'e: ',eps,' '
        print *,'time series parameters:'
        print 50,'T: ',tlen,'s'
        print 50,'t: ',dt,'s'
        print *,'output file: ',outfile(1:index(outfile,' ')-1)
      endif
c 
c calculate derived system properties
      f02=0.5*((f1**2)+(fK1**2)+(f2**2)+(fK2**2))
      fd2=0.5*((f1**2)+(fK1**2)-(f2**2)-(fK2**2))
      k4=(fK1*fK2)**2
c resonance frequencies
      fr1=sqrt(f02+sqrt(fd2**2+k4))
      fr2=sqrt(f02-sqrt(fd2**2+k4))
      if (verbose) then
        print *,'resonance frequencies:'
        print 50,'fr1: ',fr1,'Hz'
        print 50,'fr2: ',fr2,'Hz'
      endif
c 
c determine number of samples
      nsamp=1
      do while ((nsamp*dt).lt.tlen)
        nsamp=nsamp*2
      enddo
      if (nsamp.gt.msamp) then
        print *,'requires ',nsamp,' samples'
        print *,'compiled for ',msamp,' samples'
        stop 'ERROR: too many samples'
      endif
c 
c determine time series parameters
      tlen=nsamp*dt
      df=1./tlen
      fny=df*(nsamp/2)
      omny=fny*2.*pi
      dom=df*2.*pi
c
c determine complex system parameters
      om12=((2*pi*f1)**2)*(1+ime/Q1)
      om22=((2*pi*f2)**2)*(1+ime/Q2)
      omK12=((2*pi*fK1)**2)*(1+ime/QK)
      omK22=((2*pi*fK2)**2)*(1+ime/QK)
c 
c prepare source signal
      do i=1,nsamp
        tvar=i*dt
        if (tvar.le.du1) then
          src1(i)=dcmplx(dsin(pi*(tvar)/(du1))**3)*
     &       sqrt(float(nsamp))*dt
        else
          src1(i)=(0.d0,0.d0)
        endif
        if (tvar.le.du2) then
          src2(i)=dcmplx(dsin(pi*(tvar)/(du2))**3)*
     &       sqrt(float(nsamp))*dt
        else
          src2(i)=(0.d0,0.d0)
        endif
      enddo
      call tf_dfork(nsamp,src1,-1.d0)
      call tf_dfork(nsamp,src2,-1.d0)
c
      do i=1,nsamp
        do k=1,8
          spec(i,k)=(0.d0,0.d0)
        enddo
      enddo
c 
      avg1=0.d0
      avg3=0.d0
      do i=2,nsamp/2
        fre=df*(i-1)
        om=2.*pi*fre
        om2=om**2
c 
c butterworth lowpass
        in1=a1*exp(-ime*om*del1)*src1(i)*(ime*om)**der1
        in2=a2*exp(-ime*om*del2)*src2(i)*(ime*om)**der2
c store input to mass 1
        spec(i,1)=in1
c store input to mass 2
        spec(i,2)=in2
c 
c build system matrix
        m11=om12+omK12-om2
        m12=-omK12
        m21=-omK22
        m22=om22+omK22-om2
c derive inverse matrix
        det1=m11*m22
        det2=m21*m12
        if (abs(1.-abs(det1/det2)).lt.1.e-15) then
          print *,'matrix is near to singular'
          stop 'ERROR: increase Q'
        endif
        det=det1-det2
        im11=m22/det
        im12=-m12/det
        im21=-m21/det
        im22=m11/det
c 
c add to spectra
c   motion of mass 1
        spec(i,3)=in1*im11+in2*im12
c   motion of mass 2
        spec(i,4)=in1*im21+in2*im22
c   energy in signals 1 and 3
        avg1=avg1+abs(spec(i,1))**2
        avg3=avg3+abs(spec(i,3))**2
c   symmetric motion of mass 1 and 2
        spec(i,7)=0.5*(spec(i,3)+spec(i,4))
c   antisymmetric motion of mass 1 and 2
        spec(i,8)=spec(i,3)-spec(i,4)
      enddo
c stabilization values for deconvolution
      avg1=eps*avg1/float(nsamp/2-1)
      avg3=eps*avg3/float(nsamp/2-1)
c deconvolve motion of mass 1 with input to mass 1
      do i=2,nsamp/2
        spec(i,5)=spec(i,3)*dconjg(spec(i,1))/(abs(spec(i,1))**2+avg1)
      enddo
c deconvolve motion of mass 2 with motion of mass 1
      do i=2,nsamp/2
        spec(i,6)=spec(i,4)*dconjg(spec(i,3))/(abs(spec(i,3))**2+avg3)
      enddo
c
      if (overwrite) call sff_New(lu, outfile, ierr)
      if (ierr.ne.0) stop 'ERROR: cleaning file'
      call sffu_simpleopen(lu, outfile)
c calculate time-domain signal and write to output file
      if (verbose) print *,'calculate time series'
      do i=1,8
        last=.false.
        if (i.eq.8) last=.true.
        do k=1,nsamp
          strans(k)=(0.d0,0.d0)
        enddo
        do k=1,nsamp/2
          strans(k)=spec(k,i)*sqrt(float(nsamp))*df
        enddo
        do k=2,nsamp/2-1
          strans(nsamp+2-k)=dconjg(strans(k))
        enddo
        call tf_dfork(nsamp,strans,1.d0)
        do k=1,nsamp
          data(k)=sngl(dreal(strans(k)))
        enddo
        call sffu_simplewrite(lu, last, data, nsamp, sngl(dt), float(i))
      enddo
c
      stop
   50 format(3x,a5,f10.4,1x,a)
   51 format(3x,a5,i10,1x,a)
   99 stop 'ERROR: reading command line parameter'
      end
c
c======================================================================
c
      subroutine usage
c
      print *,'This program calculates the response of two '
     &       ,'coupled spring-mass oscilliators.'
      print *,' '
      print *,'  |                                       |'
      print *,'  |---/////---­O----/////----O----/////---|'
      print *,'  |                                       |'
      print *,'  |    D1      m1    DK      m2    D2     |'
      print *,'  |                                       |'
      print *,'  |                                       |'
      print *,' '
      print *,'Masses are m1 and m2. Oscillator springs are '
     &       ,'D1 and D2 and the coupling'
      print *,'spring is DK. The system is determined in '
     &       ,'terms of frequencies and and'
      print *,'the excitation is determined in terms of '
     &       ,'acceleration a1 and a2 on mass'
      print *,'m1 and m2 respectively.'
      print *,' '
      print *,'Frequencies are:'
      print *,' '
      print *,'-f1   f1=sqrt(D1/m1)/(2*pi)'
      print *,'-f2   f2=sqrt(D2/m2)/(2*pi)'
      print *,'-fK1  fK1=sqrt(DK/m1)/(2*pi)'
      print *,'-fK2  fK2=sqrt(DK/m2)/(2*pi)'
      print *,' '
      print *,'Other system parameters are:'
      print *,' '
      print *,'-Q1   quality factor for D1'
      print *,'-Q2   quality factor for D2'
      print *,'-QK   quality factor for DK'
      print *,' '
      print *,'-a1   amplitude of external acceleration '
     &       ,'pulse applied to m1'
      print *,'-a2   amplitude of external acceleration '
     &       ,'pulse applied to m2'
      print *,'-d1   delay of pulse to m1'
      print *,'-d2   delay of pulse to m2'
      print *,'-du1  duration of pulse 1'
      print *,'-du2  duration of pulse 2'
      print *,'-D1   order of time derivative for wavelet 1'
      print *,'-D2   order of time derivative for wavelet 2'
      print *,' '
      print *,'-e    stabilisation parameter'
      print *,' '
      print *,'Time series parameters are:'
      print *,' '
      print *,'-T    duration of time series'
      print *,'-t    sampling interval'
     &       ,'Nyquist'
      print *,' '
      print *,'-o    overwrite output file'
      print *,'-v    verbose mode'
      print *,' '
      print *,'output time series:'
      print *,'1: input signal to mass 1'
      print *,'2: input signal to mass 2'
      print *,'3: displacement of mass 1'
      print *,'4: displacement of mass 2'
      print *,'5: response of mass 1 with respect to input 1'
      print *,'6: response of mass 2 with respect to mass 1'
      print *,'7: symmetric component of oscillation'
      print *,'8: antisymmetric component of oscillation'
c
      return
      end
c
c ----- END OF coupledoscillators.f ----- 
