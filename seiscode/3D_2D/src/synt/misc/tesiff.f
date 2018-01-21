c this is <tesiff.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c create synthetic filter TEst SIgnals in stuttgart File Format
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
c
c REVISIONS and CHANGES
c    18/11/98   V1.0   Thomas Forbriger
c    11/11/99   V1.1   added white noise and boxcar
c    11/11/02   V1.2   timeshift only on request
c    30/09/11   V1.3   addded sampling rate option
c
c==============================================================================
c
      program tesiff
c
      character*79 version
      parameter(version=
     &  'TESIFF   V1.3   create synthetic filter TEst SIgnals in SFF')
c
      character*79 TESIFF_CVS_ID
      parameter(TESIFF_CVS_ID=
     &  '$Id$')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=5)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical last
      logical optshift
c 
      integer itrace,ntrace,theminute
c 
      integer nsamp
      real samprat
      character station*6, comp*4, auxid*4, instype*7
      character*132 wid2line
      integer year, month, day, hour, minute
      real second, calib, calper, hang, vang
c 
      integer maxsamp
      parameter(maxsamp=10000)
      real fdata(maxsamp)
      integer idata(maxsamp)
      equivalence(fdata,idata)
c
      integer maxfree, nfree
      parameter(maxfree=5)
      character*80 free(maxfree)
c
      character*1 cs
      real c1,c2,c3
      integer nstack
c
      integer ierr, lu
      parameter(lu=12)
c 
      character stype*21, sdate*7, stime*11
c
      character*80 outfile
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-s, '-r', '-n'/
      data opthasarg/3*.FALSE.,2*.true./
      data optarg/3*1h-,'1.','1000'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: tesiff [-n n] [-s] [-r r] outfile'
        print *,'   or: tesiff -help'
        if (iargc().lt.1) stop 'ERROR: missing arguments'
        print *,' '
        print *,'create synthetic filter TEst SIgnals in SFF'
        print *,' '
        print *,'outfile      file to save test signals'
        print *,' '
        print *,'-s           shift each trace by one minute'
        print *,'             times the trace index'
        print *,'-r r         use sampling rate of r Hz'
        print *,'-n n         produce n samples per trace'
        print *,' '
        print *,TESIFF_CVS_ID
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      optshift=optset(3)
      read(optarg(4), *) samprat
      read(optarg(5), *) nsamp
c 
      if (lastarg.eq.iargc()) stop 'ERROR: filename?'
c 
      call getarg(lastarg+1, outfile)
c
c----------------------------------------------------------------------
c 
      nsamp=min(nsamp,maxsamp)
      ntrace=8
c      samprat=1.
c 
      year=1998
      month=11
      day=18
      hour=0
      minute=0
      second=0.
c 
      sdate='981118'
      stime='000000.000'
      stype='testsignal'
c 
      station='NSP'
      comp='NSP'
      auxid='NSP'
      instype='NSP'
c 
      calib=-1.
      calper=-1.
      hang=-1.
      vang=-1.
c 
      nstack=1
      cs='C'
      c1=0.
      c2=0.
      c3=0.
c
c------------------------------------------------------------------------------
c go
c
      free(1)=version
      free(2)='synthetic test signals'
      nfree=2
c 
      print *,'open ',outfile(1:index(outfile, ' '))
      call sff_WOpenFS(lu, outfile, 
     &  free, nfree,
     &  stype, cs, c1, c2, c3, sdate, stime, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
c 
      last=.false.
      do itrace=1,ntrace
c 
        if (itrace.eq.ntrace) last=.true.
c 
        call fillsig(itrace, fdata, nsamp, free(1))
        nfree=1
c 
        theminute=minute
        if (optshift) theminute=theminute+itrace
        call sff_PrepWID2(nsamp, samprat, station, year, month,
     &    day, hour, theminute, comp, auxid, instype, second, calib,
     &    calper, hang, vang, wid2line, ierr)
        if (ierr.ne.0) stop 'ERROR: preparing WID2 line'
c 
        c1=float(itrace)
        call sff_WTraceFI(lu, wid2line, nsamp, fdata, idata, last,
     &    nfree, free,
     &    cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: writing trace'
c 
      enddo
c 
      stop
      end
c
c----------------------------------------------------------------------
c
      subroutine fillsig(select, x, n, c)
c
      integer select, n
      real x(n)
      character c*(80)
c 
      integer nfour
      parameter(nfour=300)
      real phase(nfour), amplitude(nfour), frequency(nfour), tf_rand
      real maxfreq,minfreq,maxamp
c
      integer i, j
      real pi, freq, tau, arg
      parameter(pi=3.1415927)
c
c prepare random signal
      if (select.eq.7) then
        call tf_tsrand
        maxfreq=0.5
        minfreq=1/float(n)
        do i=1,nfour
          amplitude(i)=0.8+0.4*tf_rand()
          phase(i)=2*pi*tf_rand()
          frequency(i)=((maxfreq-minfreq)*tf_rand()+minfreq)*2.*pi
        enddo
      endif
c 
      do i=1,n
        if (select.eq.1) then
          if (i.lt.(n/2)) then
            x(i)=0.
          else
            x(i)=1.
          endif
          c='step function'
        elseif (select.eq.2) then
          if (i.eq.(n/2)) then
            x(i)=1.
          else
            x(i)=0.
          endif
          c='spike'
        elseif (select.eq.3) then
          freq=0.05
          x(i)=sin(i*freq*2.*pi)
          c='20. sec sine'
        elseif (select.eq.4) then
          freq=0.005
          x(i)=sin(i*freq*2.*pi)
          c='200. sec sine'
        elseif (select.eq.5) then
          tau=20.
          arg=float(i)-float(n/2)
          arg=arg/tau
          arg=arg*arg
          x(i)=exp(-arg)
          c='20. sec gauss'
        elseif (select.eq.6) then
          tau=50.
          freq=.05
          x(i)=sin(i*freq*2.*pi)*exp(-i/tau)
          c='20. sec sine, 50. sec damp'
        elseif (select.eq.7) then
          x(i)=0.
          do j=1,nfour
            x(i)=x(i)+amplitude(j)*sin(phase(j)+float(i)*frequency(j))
          enddo
          c='white random noise'
        elseif (select.eq.8) then
          if (i.lt.int((4./10.)*n)) then
            x(i)=0.
          elseif (i.gt.int((6./10.)*n)) then
            x(i)=0.
          else
            x(i)=1.
          endif
          c='boxcar function'
        else
          x(i)=0.
          c='not specified'
        endif
      enddo
c
c scale noise
      if (select.eq.7) then
        maxamp=0.
        do i=1,n
          maxamp=max(maxamp,abs(x(i)))
        enddo
        do i=1,n
          x(i)=x(i)/maxamp
        enddo
      endif
c 
      call sff_trimlen(c,i)
      print *,'trace ',select,' filled with ',c(1:i)
c
      return
      end
c 
c ----- END OF tesiff.f -----
