c this is <dise.f>
c------------------------------------------------------------------------------
c
c 19/04/99 by Thomas Forbriger (IfG Stuttgart)
c
c DIfferential SEismograms
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
c    19/04/99   V1.0   Thomas Forbriger
c    20/09/00   V1.1   set matching source time
c               V1.2   added normalized datasets
c
c==============================================================================
c
      program dise
c
      character*79 version
      parameter(version='DISE   V1.2   DIfferential SEismograms')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      character*80 infile1,infile2,outfile,intrace1,intrace2,ssecbeg,ssecend
      integer ifirst,ilast,itrace1,itrace2
      real secbeg,secend
      character*132 wid2line
c 
      integer maxsamp
      parameter(maxsamp=100000)
      real data1(maxsamp), data2(maxsamp), data3(maxsamp)
      integer idata1(maxsamp), idata2(maxsamp), idata3(maxsamp)
      real dataout1(maxsamp), dataout2(maxsamp),dataout3(maxsamp)
      real dataout4(maxsamp), dataout5(maxsamp),dataout6(maxsamp)
      integer idataout1(maxsamp), idataout2(maxsamp),idataout3(maxsamp)
      integer idataout4(maxsamp), idataout5(maxsamp),idataout6(maxsamp)
      equivalence(data1,idata1)
      equivalence(data2,idata2)
      equivalence(data3,idata3)
      equivalence(dataout1,idataout1)
      equivalence(dataout2,idataout2)
      equivalence(dataout3,idataout3)
      equivalence(dataout4,idataout4)
      equivalence(dataout5,idataout5)
      equivalence(dataout6,idataout6)
c 
      integer nsamp1, nsamp2, nout
      real dt1, dt2, tanf1, tanf2
      double precision rms1, rms2, rms3, rms4
c 
      integer i, lu, ierr
      parameter(lu=10)
      logical last
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().ne.7)) then
        print *,version
        print *,'Usage: dise infile1 n1 infile2 n2 outfile tb te'
        print *,'   or: dise -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'DIfferential SEismograms'
        print *,' '
        print *,'infile1      SFF input file for trace A'
        print *,'n1           trace number of trace A in file ''infile1'' '
        print *,'infile2      SFF input file for trace B'
        print *,'n2           trace number of trace B in file ''infile2'' '
        print *,'outfile      results will be written to this file'
        print *,'tb           time of first sample (in sec)'
        print *,'te           time of last sample (in sec)'
        print *,' '
        print *,'about DISE'
        print *,'=========='
        print *,' '
        print *,'  This program compares two time series. These may be'
        print *,'  real data and synthetics or synthetics calculated with'
        print *,'  two different methods. The two time series are named A'
        print *,'  and B and are read from the input file. In order to'
        print *,'  calculate a difference trace (A-B), trace B will be'
        print *,'  resampled to match the sampling interval and time range'
        print *,'  of trace A. Root mean square (rms) values are calculated'
        print *,'  from all three traces within the specified time range'
        print *,'  (as defined by ''tb'' and ''te''). Ratios of the rms values'
        print *,'  are reported on the terminal.'
        print *,' '
        print *,'  nA and nB are datasets normalized to an rms of 1.'
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
      call getarg(1,infile1)
      call getarg(2,intrace1)
      call getarg(3,infile2)
      call getarg(4,intrace2)
      call getarg(5,outfile)
      call getarg(6,ssecbeg)
      call getarg(7,ssecend)
c
c------------------------------------------------------------------------------
c go
      read(intrace1, *)itrace1
      read(intrace2, *)itrace2
      read(ssecbeg, *)secbeg
      read(ssecend, *)secend
      call getfile(infile1, itrace1, data1, idata1, maxsamp, nsamp1,
     &  tanf1, dt1)
      call getfile(infile2, itrace2, data2, idata2, maxsamp, nsamp2,
     &  tanf2, dt2)
c 
c do only as much as necessary
      ifirst=1+int((secbeg-tanf1)/dt1)
      ilast=1+int((secend-tanf1)/dt1)
      if ((ifirst.gt.ilast).or.(ifirst.lt.1).or.(ilast.gt.nsamp1))
     &  stop 'ERROR: check time range'
      nout=ilast-ifirst+1
      call resamp(data2, data3, nsamp1, nsamp2, tanf1, dt1, tanf2, dt2,
     &  ifirst, ilast)
c 
      print *,'calculating difference trace (A-B)...'
      rms1=0.
      rms2=0.
      rms3=0.
      rms4=0.
      do i=1,nout
        dataout2(i)=data1(i-1+ifirst)
        dataout3(i)=data3(i)
        dataout1(i)=dataout2(i)-dataout3(i)
        rms1=rms1+dataout1(i)*dataout1(i)
        rms2=rms2+dataout2(i)*dataout2(i)
        rms3=rms3+dataout3(i)*dataout3(i)
      enddo
      rms1=sqrt(rms1/nout)
      rms2=sqrt(rms2/nout)
      rms3=sqrt(rms3/nout)
c
c notice related values:
c   data1   dataout2    rms2
c   data3   dataout3    rms3  (resampled version of data2)
c 
      do i=1,nout
        dataout5(i)=data1(i-1+ifirst)/rms2
        dataout6(i)=data3(i)/rms3
        dataout4(i)=dataout5(i)-dataout6(i)
        rms4=rms4+dataout4(i)*dataout4(i)
      enddo
      rms4=sqrt(rms4/nout)
c 
      last=.false.
      print *,'going to write A-B, A and B to ',
     &  outfile(1:index(outfile, ' ')),'...'
      call sff_WOpenS(lu, outfile, 'nil                           ',
     &                'C', 0., 0., 0., '990420','000000.000', ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
c 
      call sff_PrepWID2(nout, 1./dt1, 'NSP   ', 1999, 4, 20, 0, 0,
     &  'A-B', 'NSP   ', 'NSP   ', 0., 1., 1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line for trace one'
      call sff_WTrace(lu, wid2line, nout, dataout1, idataout1, last, ierr)
      if (ierr.ne.0) stop 'ERROR: writing first trace to output file'
c 
      call sff_PrepWID2(nout, 1./dt1, 'NSP   ', 1999, 4, 20, 0, 0,
     &  'A', 'NSP   ', 'NSP   ', 0., 1., 1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line for trace two'
      call sff_WTrace(lu, wid2line, nout, dataout2, idataout2, last, ierr)
      if (ierr.ne.0) stop 'ERROR: writing second trace to output file'
c 
      call sff_PrepWID2(nout, 1./dt1, 'NSP   ', 1999, 4, 20, 0, 0,
     &  'B', 'NSP   ', 'NSP   ', 0., 1., 1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line for trace three'
      call sff_WTrace(lu, wid2line, nout, dataout3, idataout3, last, ierr)
      if (ierr.ne.0) stop 'ERROR: writing third trace to output file'
c 
      call sff_PrepWID2(nout, 1./dt1, 'NSP   ', 1999, 4, 20, 0, 0,
     &  'nA-nB', 'NSP   ', 'NSP   ', 0., 1., 1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line for trace one'
      call sff_WTrace(lu, wid2line, nout, dataout4, idataout4, last, ierr)
      if (ierr.ne.0) stop 'ERROR: writing fourth trace to output file'
c 
      call sff_PrepWID2(nout, 1./dt1, 'NSP   ', 1999, 4, 20, 0, 0,
     &  'nA', 'NSP   ', 'NSP   ', 0., 1., 1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line for trace two'
      call sff_WTrace(lu, wid2line, nout, dataout5, idataout5, last, ierr)
      if (ierr.ne.0) stop 'ERROR: writing fifth trace to output file'
c 
      call sff_PrepWID2(nout, 1./dt1, 'NSP   ', 1999, 4, 20, 0, 0,
     &  'nB', 'NSP   ', 'NSP   ', 0., 1., 1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing WID2 line for trace three'
      last=.true.
      call sff_WTrace(lu, wid2line, nout, dataout6, idataout6, last, ierr)
      if (ierr.ne.0) stop 'ERROR: writing sixth trace to output file'
c 
      print *,'results are:'
      print *,'  Arms/Brms:     ',rms2,'/',rms3,'=',rms2/rms3
      print *,'  (A-B)rms/Arms: ',rms1,'/',rms2,'=',rms1/rms2
      print *,'  (A-B)rms/Brms: ',rms1,'/',rms3,'=',rms1/rms3
      print *,'     (nA-nB)rms: ',rms4
c
      stop
      end
c
c======================================================================
c
      subroutine getfile(filename, itrace, data, idata, maxsamp,
     &                   nsamp, tanf, dt)
c 
      character filename*(*)
      integer itrace, nsamp, maxsamp
      real data(maxsamp)
      integer idata(maxsamp)
      real tanf, dt
c
      character cs*1, code*20, time*20, date*20, tist*20, type*30, rs*1
      character wid2line*132
      real inv
      real c1,c2,c3,r1,r2,r3,sffu_tfirst
      integer ierr, lu, nstack, i
      parameter(lu=10)
      logical last
c 
      print *,'going to read ',filename(1:index(filename, ' ')),'...'
c 
      call sff_ROpenS(lu, filename, inv, tist, code, type, cs, c1, c2, c3,
     &  date,time,ierr)
      if (ierr.ne.0) stop 'ERROR opening input file'
      nsamp=maxsamp
      last=.false.
      do i=1,itrace
        if (last) stop 'ERROR: too few traces in input file'
        call sff_RTraceI(lu, tanf, dt, wid2line, nsamp, data, idata, code,
     &                   last, rs, r1, r2, r3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading input file'
      enddo
      print *,'  read trace no.',itrace
      if (.not.(last)) close(lu)
      tanf=sffu_tfirst(wid2line, time, date)  
      print *,'  trace begins at ',tanf,'sec'
      print *,'  sampling interval is ',dt,'sec'
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine resamp(data2, data3, nsamp1, nsamp2, tanf1, dt1, tanf2, dt2,
     &  ifirst, ilast)
c 
      real data2(nsamp2), data3(nsamp1)
      integer nsamp1, nsamp2, ifirst, ilast
      real tanf1, tanf2, dt1, dt2
c 
      logical verbose
      parameter(verbose=.true.)
c
      integer maxspec
      parameter(maxspec=100000)
      complex spect(maxspec)
      integer nspec
c 
      real pi, t, sum,dper,selbeg,selend,oselbeg,oselend
      integer ispec,i,oifirst,oilast,onspec,nout
      integer limit,percount,nextper
      complex ime, expfac, expfacbase
      parameter(pi=3.1415926535897931159979,ime=(0.,1.))
c 
      if (verbose) then
        print *,' '
        print *,'do some resampling...'
      endif
c 
      selbeg=(ifirst-1)*dt1+tanf1
      selend=(ilast-1)*dt1+tanf1
      oifirst=int((selbeg-tanf2)/dt2)
      oilast=2+int((selend-tanf2)/dt2)
      if ((oifirst.gt.oilast).or.(oifirst.lt.1).or.(oilast.gt.nsamp2))
     &  stop 'ERROR: check times'
      onspec=oilast-oifirst+1
      nout=ilast-ifirst+1
      oselbeg=(oifirst-1)*dt2+tanf2
      oselend=(oilast-1)*dt2+tanf2
c 
      print *,'  original trace:'
      print *,'    first sample: ',tanf2,'sec; last sample: ',
     &  (nsamp2-1)*dt2+tanf2,'sec'
      print *,'    sampling interval: ',dt2,'sec'
      print *,'    selected first sample: ',oifirst
      print *,'    selected last sample:  ',oilast
      print *,'    selected start time:   ',oselbeg,'sec'
      print *,'    selected end time:     ',oselend,'sec'
      print *,'  master trace:'
      print *,'    first sample: ',tanf1,'sec; last sample: ',
     &  (nsamp1-1)*dt1+tanf1,'sec'
      print *,'    sampling interval: ',dt1,'sec'
      print *,'    selected first sample: ',ifirst
      print *,'    selected last sample:  ',ilast
      print *,'    selected start time:   ',selbeg,'sec'
      print *,'    selected end time:     ',selend,'sec'
c 
      nspec=2
      do while(nspec.lt.onspec)
        nspec=nspec*2
      enddo
      if (nspec.gt.maxspec) stop 'ERROR: maxspec too small!'
c
      print *,'preparing spectrum with ',nspec,' samples'
      do i=1,nspec
        spect(i)=(0.,0.)
      enddo
c
      print *,'   extracting'
      print *,'     original trace ',oifirst,'-',oifirst-1+onspec
      print *,'     to spectrum trace ',1,'-',onspec
      do i=1,onspec
        spect(i)=cmplx(data2(oifirst+i-1))
      enddo
c 
      call tf_fork(nspec,spect,-1.)
c 
      if (verbose) then
        print *,'  we hold the spectrum now'
        print *,'  going to perform resampling DFT (which will be slow)...'
      endif
c 
c
c dt*sqrt(nsamples) is the factor we have to multiply in order to get
c   spectral coefficients corresponding to a foruier integral transform
c
c dt*(nsamples-1) is the time length of the original trace, which is the 
c   recurrence time of the time limited fourier signal
c 
c 1./(dt*nsamples) is the frequency step size
c
      if (dt1.gt.dt2) then
        print *,'      old sampling interval (',dt2,'sec)'
        print *,'      is less than new one (',dt1,'sec) !'
        print *,
     &  '      it''s up to you to perform a correct low pass filtering'
      endif
c 
c 
      expfacbase=2.*pi*ime/(dt2*float(nspec))
      limit=nspec/2
      dper=10.
      percount=1
      nextper=nout*dper/100.
c 
      do i=1,nout
        t=(i-1)*dt1+selbeg
c        print *,'calc sample ',i,' at ',t,'sec'
        expfac=expfacbase*(t-oselbeg)
        if ((t.lt.oselbeg).or.(t.gt.oselend)) then
          data3(i)=0.
          print *,'trapped'
        else
           sum=0.5*real(spect(1))
           do ispec=2,limit
             sum=sum+real(spect(ispec)*cexp(expfac*float(ispec-1)))
           enddo
           sum=sum+0.5*real(spect(limit+1)*cexp(expfac*(limit)))
           data3(i)=2.*sum/sqrt(float(nspec))
        endif
        if (i.gt.nextper) then
          print *,'    finished ',percount*dper,'%'
          percount=percount+1
          nextper=percount*nout*dper/100.
        endif
      enddo
      print *,'    finished 100.%'
c 
      return
      end
c 
c ----- END OF dise.f -----
