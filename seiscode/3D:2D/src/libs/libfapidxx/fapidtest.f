c this is <fapidtest.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
c
c this is a program to test several steps of fapid development
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
c    17/11/2010   V1.0   Thomas Forbriger
c    01/04/2011   V1.1   test file writing
c
c ============================================================================
c
      program fapidtest
c
      character*(*) version
      parameter(version=
     &  'FAPIDTEST  V1.1  this is a program to test libfapidxx.a')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=7)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
      double precision dt

c here are the keys to our commandline options
      data optid/'-D', '-v', '-x', '-t', '-w', '-r', '-d'/
      data opthasarg/3*.FALSE.,4*.TRUE./
      data optarg/3*'-','sff','testfile.sff','junk.sff','1.e-3'/
c
      character*20 formatid
      character*40 infile, outfile
      logical doread, dowrite
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: fapidtest [-r file] [-w file] [-t type]'
        print *,'                 [-d t]'
        print *,'   or: fapidtest -help'
        print *,'   or: fapidtest -x'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'-x           print online help'
        print *,'-r file      test reading from file'
        print *,'-w file      test writing to file'
        print *,'-t type      select file type for input/output'
        print *,'-d t         set sampling interval to ''t'' seconds'
        print *,' '
        call sff_help_formats
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)

      if (optset(3)) then
        call sff_help_details
        stop
      endif

      debug=optset(1)
      verbose=optset(2)
      formatid=optarg(4)
      dowrite=optset(5)
      outfile=optarg(5)
      doread=optset(6)
      infile=optarg(6)
      read(optarg(7), *, err=99) dt
c
c------------------------------------------------------------------------------
c go
c
      print *,'select format ',formatid
      call sff_select_format(formatid, ierr)
      if (ierr.ne.0) then
        stop 'ERROR: selecting format'
      endif
      if (dowrite) then
        if (verbose) then
          print *,'write data with sampling interval ',dt,' s'
        endif
        call writetest(outfile, dt)
      endif
      if (doread) then
        if (verbose) then
          print *,'read data'
        endif
        call readtest(infile)
      endif
      stop
   99 stop 'ERROR: reading command line arument'
      end

c======================================================================
c functions
c ---------

      subroutine writetest(filename, indt)
c
      character*(*) filename
      double precision indt
c
      integer m
      parameter(m=1024)
      real d(m)
      integer id(m)
      equivalence(d,id)
      integer n,i,j,s,k
      real p,dx,dt,rate
c 
      integer lu, ierr
      parameter(lu=10)
      character date*7, time*12,stype*22, wid2line*132, nil*10
      parameter(nil='NIL')
c 
      print *,'write test'
      print *,'----------'
      print *,'open ',filename(1:index(filename,' '))
      dt=indt
      rate=1./dt
      dx=1.
      n=40
      p=(dt*m)/(dx*n)
      stype='synthetic'
      date='110401'
      time='123209.012345'
      call sff_PrepWid2(m, rate, 'sta   ',
     &   2011, 04, 01, 12, 32, 'chan   ', 'aux   ', 'ins     ', 
     &   10.023456, -1., -1., -1., -1., wid2line, ierr)
      if (ierr.ne.0) stop 'ERROR: preparing wid2line'
      call sff_WOpenS(lu, filename, stype, 'C', 0., 0., 0., date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening file'
      s=int((dx*p)/dt)
      k=s
      do i=1,n
        print *,'write trace #',i
        do j=1,m
          d(j)=0.
        enddo
        if (k.le.m) d(k)=1./sqrt(i*dx)
        k=k+s
        if (i.eq.n) then
          call sff_WTraceI(lu, wid2line, m, d, id, .true., 'C',
     &                     i*dx, 0., 0., 1, ierr)
        else
          call sff_WTraceI(lu, wid2line, m, d, id, .false., 'C',
     &                     i*dx, 0., 0., 1, ierr)
        endif
        if (ierr.ne.0) stop 'ERROR: writing trace'
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine readtest(filename)
c
      character*(*) filename
      integer lu
      parameter(lu=10)
c time series
      integer maxsamp, nsamp
      parameter(maxsamp=100000)
      real fdata(maxsamp)
      integer idata(maxsamp)
      equivalence(fdata, idata)

c sff file header
      character code*10, timestamp*13, scs*1, date*6, time*10
      real sffversion, sc1, sc2, sc3
      character*20 source
c
c sff trace 
      logical last
      character rcs*1, wid2line*132
      real rc1, rc2, rc3, tanf, dt
      integer nstack
c 
      print *,'read test'
      print *,'---------'
      print *,'open ',filename(1:index(filename,' '))
      call sff_ROpenS(lu, filename, sffversion, timestamp, code, 
     &  source, scs, sc1, sc2, sc3, date, time, ierr)
      if (ierr.ne.0) then
        stop 'ERROR: opening file'
      endif
      print *,'sffversion: ', sffversion
      print *,'timestamp: ', timestamp
      print *,'code: ', code
      print *,'source: ', source
      print *,'scs, sc1, sc2, sc3: ', scs, sc1, sc2, sc3
      print *,'date time: ', date, ' ', time
      last=.false.
      do while (.not.last)
        print *,' '
        print *,'read next trace'
        nsamp=maxsamp
        call sff_RTraceI(lu, tanf, dt,
     &    wid2line, nsamp, fdata,
     &    idata, code, last,
     &    rcs, rc1, rc2, rc3, nstack, ierr)
        print *,wid2line
        print *,'nsamp: ',nsamp
        print *,'rcs, rc1, rc2, rc3: ',rcs, rc1, rc2, rc3
        print *,'nstack: ',nstack
        print *,'dt: ',dt
        print *,'tanf: ',tanf
        print *,'code: ',code
        if (ierr.ne.0) then
          stop 'ERROR: reading trace'
        endif
      enddo
      return
      end
c
c ----- END OF fapidtest.f ----- 
