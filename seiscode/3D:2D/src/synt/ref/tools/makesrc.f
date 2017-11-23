c this is <makesrc.f>
c
c======================================================================
c
c this is a quick hack to generate a source time-signal
c
c Copyright (c) 1996 by Thomas Forbriger
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
c this needs libtf
c
c Revisions
c   V1.0   17/04/96   first running version
c   V1.1   10/07/96   multiple source-fctn
c   V1.2   09/02/97   changed parameter reading
c
c======================================================================
      program makesrc
c declare variables
      integer maxtraces, maxsamples, maxprolog, iargc
      parameter(maxtraces=1)
      parameter(maxsamples=5000)
      parameter(maxprolog=50)
      character*80 outfile, dtstr, Tstr, typestr
      integer trace, sample, type, i, maxtype
      integer ntraces, nsamples, nprolog
      real data(maxtraces, maxsamples)
      real distance(maxtraces)
      character*80 prolog(maxprolog)
      real dt, stmin, stsec, T, pi, TL
      real srcfcta, srcfctb
      parameter(pi=3.1415926535898)
      character*80 version
      character*80 info1(10),info2(10)

c======================================================================
c
c DEFINE SOURCE HERE!
c
c srcfct is the source-function with the following arguments:
c        t:   is the time to which we desire a value
c        TL:  is the total time-length of the function (0 <= t <= TL)
c info1  should be a string containing the function definition
c info2  should be an explanatory string
c      srcfct(t,TL)=0.75*pi*TL*sin(pi*t/TL)**3
c      info1='srcfct(t,TL)=0.75*pi*TL*sin(pi*t/TL)**3'
      srcfcta(t,TL)=sin(pi*t/TL)**3
      srcfctb(t,TL)=exp(-pi*2*t*0.5/TL)*sin((pi*2*t*0.866/TL))
      info1(1)='srcfct(t,TL)=sin(pi*t/TL)**3'
      info2(1)='Ungerer''s Mueller-Bruestle function'
      info1(2)=
     &'srcfct(t,TL)=exp(-pi*2*t*0.5/TL)*sin(pi*2*t*0.866/TL)'
      info2(2)='damped harmonic - signal length is T*4'
      maxtype=2
c======================================================================

c give basic information
      version='MAKESRC V1.2   create source time-signal'
      print *,version
      if (iargc().ne.4) then
        print *,'Usage: makesrc outfile dt T type'
        print *,'   or: makesrc -help'
        if (iargc().eq.1) then
          call getarg(1,outfile)
          if (outfile(1:5).eq.'-help') then
            print *,'outfile  is the file where the trace will be'
            print *,'         written to'
            print *,'dt       is the desired sampling rate'
            print *,'T        is the desired length of the signal'
            print *,' '
            print *,'available types are:'
            do i=1,maxtype
              print *,i
              print *,'       ',info2(i)
              print *,'       ',info1(i)
            enddo
          endif
        endif
        stop
      endif
c get arguments
      call getarg(1, outfile)
      call getarg(2, dtstr)
      call getarg(3, Tstr)
      call getarg(4, typestr)
      read(dtstr, *) dt
      read(Tstr, *) T
      read(typestr, *) type
      print *,' dt: ',dt,'    T: ',T,'   type: ',type
c----------------------------------------------------------------------
c initialize values
      ntraces=1
      trace=1
      nsamples=int(T/dt)+1
      T=float(nsamples-1)*dt
      if (type.eq.2) nsamples=nsamples*4
      distance(trace)=1.
c----------------------------------------------------------------------
c create prolog
      nprolog=5
      if (nprolog.gt.maxprolog) stop 'ERROR: too many prolog lines'
      prolog(1)=version
      write(prolog(2), 1) dt
    1 format('desired sampling interval [s]: ',f10.6)
      write(prolog(3), 2) T
    2 format(' desired length of signal [s]: 'f10.4)
      prolog(4)=info1(type)
      prolog(5)=info2(type)
c----------------------------------------------------------------------
c create signal
      do sample=1,nsamples
        if(type.eq.1) then
          data(trace,sample)=srcfcta(float(sample-1)*dt,T)
        elseif (type.eq.2) then
          data(trace,sample)=srcfctb(float(sample-1)*dt,T)
        endif
      enddo
c----------------------------------------------------------------------
c write output-data
      stmin=0.
      stsec=0.
      call seiswrite(outfile,
     &         maxtraces, maxsamples, maxprolog,
     &         nprolog, ntraces, nsamples,
     &         dt, stmin, stsec,
     &         distance, data, prolog)
      stop 'makesrc finished successfully'
      end
