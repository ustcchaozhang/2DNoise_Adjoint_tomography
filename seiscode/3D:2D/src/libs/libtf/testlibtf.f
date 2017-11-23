c this is <testlibtf.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
c
c program to test libtf functions
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
c    09/05/2007   V1.0   Thomas Forbriger
c    17/12/2007   V1.1   use keyword file in open statement
c    27/11/2009   V1.2   test magic numbers
c    04/02/2010   V1.3   test taper reading
c    03/06/2012   V1.4   added test for libc random number generator
c    24/08/2012   V1.5   test list selection
c
c ============================================================================
c
      program testlibtf
c
      character*(*) version
      parameter(version=
     &'TESTLIBTF   V1.5   program to test libtf functions')
      character*(*) TESTLIBTF_CVS_ID
      parameter(TESTLIBTF_CVS_ID=
     &'$Id$')
c
      integer m,i,n
      parameter(m=10000)
      double precision a(m)
      character*4 cmagic
c
      real tf_rand
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=9)
      character*7 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical testrng, fileoutput, testmagic, testrtaper, testrand
      logical testselect
      character*80 taperfile, selectstring
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 4h-rng, 2h-f, 2h-n, '-magic', '-rtaper',
     &  '-rand', '-select'/
      data opthasarg/4*.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,.TRUE./
      data optarg/4*1h-,3h150,3*'-','1'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: testlibtf [-v] [-f] [-n n] [-rand]'
        print *,'                 [-rng] [-magic] [-rtaper f]'
        print *,'                 [-select l]'
        print *,'   or: testlibtf -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'test libtf.a components'
        print *,' '
        print *,TESTLIBTF_CVS_ID
        print *,' '
        print *,'-v           be verbose'
        print *,'-n n         test n samples'
        print *,'-f           write results to files'
        print *,' '
        print *,'select tests:'
        print *,'-rng         test gsl random number generator'
        print *,'-rand        test libc random number generator'
        print *,'-magic       test magic numbers'
        print *,'-rtaper f    test reading a quad taper from file ''f'''
        print *,'-select l    select traces as defined by strin ''l'' '
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
      testrng=optset(3)
      fileoutput=optset(4)
      read(optarg(5), *, err=96) n
      testmagic=optset(6)
      taperfile=optarg(7)
      testrtaper=optset(7)
      testrand=optset(8)
      testselect=optset(9)
      selectstring=optarg(9)
c
c------------------------------------------------------------------------------
c go
      if (testrng) then
        print *,'test gsl random number generators'
        if (n.gt.m) stop 'ERROR: too many samples requested'
        print *,' '
        print *,'gaussian distribution:'
        call tf_gsl_rng_ugaussian(a, n)
        print '(8(2x,f7.4))', (a(i), i=1,n)
        open(unit=10,file='test_gaussian.xxx', err=99)
        write(unit=10, fmt='(f7.4)', err=98) (a(i), i=1,n)
        close(unit=10, err=97)
        print *,'uniform distribution:'
        call tf_gsl_rng_uniform(a, n)
        print '(8(2x,f7.4))', (a(i), i=1,n)
        open(unit=10, file='test_uniform.xxx', err=99)
        write(unit=10, fmt='(f7.4)', err=98) (a(i), i=1,n)
        close(unit=10, err=97)
      endif
c----------------------------------------------------------------------
      if (testrand) then
        print *,'test libc random number generator'
        do i=1,n
        print *,tf_rand()
          a(i)=dble(tf_rand())
        enddo
        print '(8(2x,f7.4))', (a(i), i=1,n)
        open(unit=10, file='test_rand.xxx', err=99)
        write(unit=10, fmt='(f7.4)', err=98) (a(i), i=1,n)
        close(unit=10, err=97)
      endif
c----------------------------------------------------------------------
      if (testmagic) then
        print *,'test magic numbers'
        cmagic='1234'
        call tf_magic(cmagic, n)
        print *,'magic number for "',cmagic,'" is ',n
      endif
c----------------------------------------------------------------------
      if (testrtaper) then
        print *,'read quad-taper from file'
        print *,taperfile
        call rtapertest(taperfile)
      endif
c----------------------------------------------------------------------
      if (testselect) then
        print *,'test list selection:'
        print *,selectstring
        call selecttest(selectstring)
      endif
c----------------------------------------------------------------------
c
      stop
   99 stop 'ERROR: opening file'
   98 stop 'ERROR: writing file'
   97 stop 'ERROR: closing file'
   96 stop 'ERROR: reading command line'
      end
c
c======================================================================
c
      subroutine rtapertest(filename)
c
      character*(*) filename
c 
      character text*80
      integer maxpicks
      parameter(maxpicks=100)
      real t(maxpicks,4)
      real x(maxpicks,4)
      integer npicks(maxpicks)
      integer i,j
c
      call tf_ttapread(filename, t, x, npicks, maxpicks, text)
      print *,'comment in file:'
      print *,text
      do i=1,4
        print *,'taper curve #',i
        do j=1,npicks(i)
          print *,j,': ',t(j,i),'s ',x(j,i),'m'
        enddo
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine selecttest(selectstring)
c
      character*(*) selectstring
c 
      integer maxselect, ierr
      parameter(maxselect=20)
      logical selection(maxselect)
c 
      call tf_listselect(maxselect, selection, 1, selectstring, ierr)
      if (ierr.ne.0) stop 'ERROR in tf_listselect'
c 
      do ierr=1,maxselect
        if (selection(ierr)) then
          print '(i3.3,x,a)', ierr, 'is selected'
        else
          print '(i3.3,x,a)', ierr, 'is not selected'
        endif
      enddo
c
      return
      end
c
c ----- END OF testlibtf.f ----- 
