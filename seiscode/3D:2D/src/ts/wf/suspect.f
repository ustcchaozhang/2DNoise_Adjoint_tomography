c this is <suspect.f>
c------------------------------------------------------------------------------
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c SUm of SPECtra
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
c uses code from susei.f
c
c REVISIONS and CHANGES
c    19/02/99   V1.0   Thomas Forbriger
c    23/03/99   V1.1   introduced -s option
c    24/04/99   V1.2   sets zero source time if no source is given in input
c
c==============================================================================
c
      program suspect
c
      character*79 version
      parameter(version='SUSPECT   V1.2   SUm of SPECTra')
c
c dimensions
      integer maxsamps, maxfree
      parameter(maxsamps=16400, maxfree=20)
c 
      integer i
c 
      real sffu_seconds
      integer time_compare
c 
      logical spherical
c input data
      real xdata(maxsamps), ydata(maxsamps), zdata(maxsamps)
      integer ixdata(maxsamps), iydata(maxsamps), izdata(maxsamps)
      equivalence(xdata,ixdata)
      equivalence(ydata,iydata)
      equivalence(zdata,izdata)
      character*80 inxname, inyname
      integer lux, luy, luz
      parameter(lux=10, luy=11, luz=12)
c output data
      real data(maxsamps)
      integer idata(maxsamps)
      double complex spect(maxsamps)
      equivalence(data,idata)
      character*80 outname
      character*132 wid2line
      integer luo
      parameter(luo=13)
c sff extras
      character*80 free(maxfree)
      integer nfree
      real libversion
      character timestamp*13, code*10, type*20, cs*1, date*6, time*10
      character code2*10, cs2*1
      real c1,c2,c3,c12,c22,c32
      integer ierr,nsamp,nsamp2,nstack,nstack2
      logical last,last2
      real tanf,dt,dt2
      character*132 wid2line2
c 
      logical zerosrce
c 
      integer sdate1(7), sdate2(7), rdate1(7), rdate2(7), toff1(7), toff2(7)
      double precision offsec1, offsec2, offseco
      integer sdateo(7), rdateo(7), toffo(7)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-d,2h-s/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: suspect file1 file2 outfile [-s]'
      print *,'   or: suspect -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'SUm of SEIsmograms'
        print *,' '
        print *,'file1  first input file'
        print *,'file2  second input file'
        print *,'outfile output file'
        print *,' '
        print *,'-s     The default coordinate system for SFF data is set'
        print *,'       to cartesian. This will be set in the case of the'
        print *,'       input file containing no source coordinates.'
        print *,'       Use this option to force using spherical coordinates.'
        print *,' '
        print *,'The program read all traces from the first input file'
        print *,'and stacks them on the corresponding traces of the'
        print *,'second input file. The resulting traces arw written to the'
        print *,'the output file. Data is expected to contain a source'
        print *,'origin time which might differ from the time of first' 
        print *,'for this reason both input traces are first aligned'
        print *,'to origin time (by application of the time shift in'
        print *,'the Fourier domain).'
        print *,' '
        print *,'The original purpose of this program was to calculate'
        print *,'appropriate waveforms for seismograms on the global'
        print *,'sphere for epicentral distances larger than 180 deg'
        print *,'by applying the focal phase shift first and then'
        print *,'stacking them with the waves travelling along the'
        print *,'minor arc.'
        stop
      endif
      if (iargc().lt.3) stop 'ERROR: too few arguments'
c
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(4, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      spherical=optset(2)
c
      call getarg(1, inxname)
      call getarg(2, inyname)
      call getarg(3, outname)
c 
c------------------------------------------------------------------------------
c 
c 
      free(1)=version
      free(2)='first data from  '//inxname(1:index(inxname, ' '))
      free(3)='second data from '//inyname(1:index(inyname, ' '))
      free(4)='all information is taken from first data file'
      free(5)='no plausibility checks were performed'
      nfree=5
c 
      call sff_ROpenS(luy, inyname,
     &  libversion, timestamp, code,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening y-component'
      call sff_New(luo, outname, ierr)
      if (index(code,'S').gt.0) then
        call sffu_timesrce(date, time, sdate2)
        zerosrce=.false.
      else
        print *,'WARNING: SRCE line is missing!'
        print *,'WARNING: setting zero SRCE time!'
        call time_clear(sdate2)
        sdate2(1)=1999
        sdate2(2)=1
        zerosrce=.true.
      endif
c 
      call sff_ROpenS(lux, inxname,
     &  libversion, timestamp, code,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening x-component'
      call sff_New(luo, outname, ierr)
      if (zerosrce) then
        call time_clear(sdate1)
        sdate1(1)=1999
        sdate1(2)=1
      else
        if (index(code,'S').gt.0) then
          call sffu_timesrce(date, time, sdate1)
          zerosrce=.false.
        else
          print *,'WARNING: SRCE line is missing!'
          print *,'WARNING: setting zero SRCE time!'
          call time_clear(sdate1)
          sdate1(1)=1999
          sdate1(2)=1
          call time_clear(sdate2)
          sdate2(1)=1999
          sdate2(2)=1
          zerosrce=.true.
        endif
      endif
c 
      if (time_compare(sdate1,sdate2).lt.0) then
        call time_copy(sdate1,sdateo)
      else
        call time_copy(sdate2,sdateo)
      endif
      call sffu_srcetime(sdateo,date,time)
c 
      if (spherical) cs='S'
c 
      call sff_WOpenFS(luo, outname,
     &  free, nfree,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
c 
      last=.false.
      do while (.not.(last))
        nsamp=maxsamps
        call sff_RTraceI(lux, tanf, dt, wid2line, nsamp, xdata, ixdata,
     &    code, last, cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading x-data'
        nsamp2=maxsamps
        call sffu_timewid2(wid2line, rdate1)
        if (zerosrce) then
          rdate1(1)=1999
          rdate1(2)=1
        endif
        call time_sub(rdate1,sdate1,toff1)
        offsec1=sffu_seconds(toff1) 
c 
        call sff_RTraceI(luy, tanf, dt2, wid2line2, nsamp2, ydata, iydata,
     &    code2, last2, cs2, c12, c22, c32, nstack2, ierr)
        if (ierr.ne.0) stop 'ERROR: reading y-data'
        call sffu_timewid2(wid2line2, rdate2)
        if (zerosrce) then
          rdate2(1)=1999
          rdate2(2)=1
        endif
        call time_sub(rdate2,sdate2,toff2)
c 
c        call time_sprint(sdate1,argument)
c        print *,'sdate1 ',argument(1:35)
c        call time_sprint(sdate2,argument)
c        print *,'sdate2 ',argument(1:35)
c        call time_sprint(rdate1,argument)
c        print *,'rdate1 ',argument(1:35)
c        call time_sprint(rdate2,argument)
c        print *,'rdate2 ',argument(1:35)
c        call time_sprint(toff1,argument)
c        print *,'toff1 ',argument(1:35)
c        call time_sprint(toff2,argument)
c        print *,'toff2 ',argument(1:35)
c 
        offsec2=sffu_seconds(toff2) 
        if (time_compare(rdate1,rdate2).lt.0) then
          call time_copy(rdate1,rdateo)
        else
          call time_copy(rdate2,rdateo)
        endif
        offseco=min(offsec1,offsec2)
        last=(last.or.last2)
c 
        if (nsamp.ne.nsamp2) stop 'ERROR: wrong number of samples in y-data'
        if (dt.ne.dt2) stop 'ERROR: inconsistent sampling rates'
c 
        call shifttrace(maxsamps, nsamp, spect, xdata, dt, offsec1)
        call shifttrace(maxsamps, nsamp2, spect, ydata, dt, offsec2)
c 
        do i=1,nsamp
          data(i)=(xdata(i)+ydata(i))
        enddo
        wid2line(36:38)='NSP'
c 
        call time_sub(rdateo,sdateo,toffo)
        offseco=sffu_seconds(toffo) 
        call shifttrace(maxsamps, nsamp, spect, data, dt, -offseco)
c        print *,offsec1,offsec2,offseco
        call sffu_setwid2time(wid2line, rdateo)
        call sff_ModWID2samps(wid2line, nsamp)
c 
        call sff_WTraceI(luo, wid2line, nsamp, data, idata, last,
     &    cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: writing trace'
      enddo
c
      stop
      end
c
c----------------------------------------------------------------------
c
      subroutine shifttrace(maxsamp, nsamp, spect, data, dt, timeshift)
c 
      integer maxsamp, nsamp
      double complex spect(maxsamp)
      real data(maxsamp)
      real dt
      double precision timeshift
c calculation
      double precision pi
      parameter(pi=3.14159265358979311599796346854418516d0)
c
      integer npow, powsamp,i
      complex*16 factor
      real*8 singback, singto
      parameter(singback=1.d0, singto=-1.d0)
c 
c transform data
        npow=0
        powsamp=2**npow
        do while (powsamp.lt.nsamp)
          npow=npow+1
          powsamp=2**npow
        enddo
        powsamp=powsamp*2
        if (powsamp.gt.maxsamp) then
          print *,'ERROR: dataset has ',nsamp,' samples'
          print *,'ERROR: fourier number of samples should be ',powsamp
          print *,'ERROR: array size is ',maxsamp
          stop
        endif
c 
        do i=1,nsamp
          spect(i)=dcmplx(data(i))
        enddo
        do i=nsamp+1,powsamp
          spect(i)=(0.d0,0.d0)
        enddo
        call tf_dfork(powsamp, spect, singback)
        factor=(0.d0,1.d0)*(1.d0/(powsamp*dt))*2.d0*pi*timeshift
        do i=0,powsamp/2-1
          spect(i+1)=spect(i+1)*exp(i*factor)
        enddo
        do i=0,powsamp/2-2
          spect(powsamp-i)=conjg(spect(i+2))
        enddo
        call tf_dfork(powsamp, spect, singto)
c 
        do i=1,powsamp
          data(i)=sngl(real(spect(i)))
        enddo
        nsamp=powsamp
c 
      return
      end
c 
c ----- END OF suspect.f -----
