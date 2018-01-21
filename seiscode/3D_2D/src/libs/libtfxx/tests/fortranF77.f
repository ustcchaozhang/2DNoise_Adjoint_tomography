c this is <fortranF77.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c Fortran part of the Fortran I/O test routines
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
c    15/11/2002   V1.0   Thomas Forbriger
c    20/11/2002   V1.1   passed tests on AIX and Intel Linux
c
c ============================================================================
c
      program fortranF77
c
      character*(*) version
      parameter(version=
     &  'FORTRANF77   V1.0   Fortran I/O test routines')
      character*(*) FORTRANF77_CVS_ID
c
c 
      logical optread,optwrite
      character*80 filename
      integer lu
      parameter(lu=10)
      integer imagic
      character*4 cmagic
      parameter(cmagic='ABCD')
      integer inmagic
      character*4 incmagic
      equivalence(inmagic, incmagic)
c 
      integer nval, i, mval
      parameter(mval=10)
      integer val1(mval),val2(mval)
      double precision dval(mval)
      integer*8 llint
      integer*4 lint
      double complex dcplx
      complex scplx
c 
      integer icpu, imatch
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-r, 2h-w/
      data opthasarg/2*.FALSE.,2*.true./
      data optarg/2*1h-,2*4hjunk/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: fortranF77 -r file|-w file [-v]'
        print *,'   or: fortranF77 -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'-v           be verbose'
        print *,'-r file      read from file created by fortraniotest'
        print *,'-w file      write file to be read by fortraniotest'
        print *,' '
        print *,FORTRANF77_CVS_ID
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
      optread=optset(3)
      optwrite=optset(4)
      if (optread) then
        optwrite=.false.
        filename=optarg(3)
      elseif (optwrite) then
        filename=optarg(4)
      else
        stop 'ERROR: you must either set -r or -w!'
      endif
c
c------------------------------------------------------------------------------
c go
      if (verbose) then
        print *,' '
        print *,version
        print *,FORTRANF77_CVS_ID
        print *,' '
      endif
c 
      if (optread) then
        if (verbose) then
          print *,'reading from ',filename(1:index(filename,' ')-1)
        endif
        open(lu, file=filename, form='unformatted', 
     &           status='old', err=96)
        read(lu, err=95, end=94) inmagic
        call tf_bytesex(cmagic, inmagic, icpu, imatch)
        print *,'read magic number: ''',incmagic,', ',inmagic,''''
        if (icpu.eq.1) then
          print *,'  I''m running on Intel'
        elseif (icpu.eq.2) then
          print *,'  I''m running on Motorola'
        else
          print *,'  I don''t know this CPU model'
          stop 'ERROR: aborting...'
        endif
        if (imatch.eq.1) then
          print *,'  file data matches CPU model'
        elseif (icpu.eq.2) then
          print *,'  file data must be swapped to match CPU'
          stop 'ERROR: cannot perform swapping'
        else
          print *,'  I do not know about the data encoding'
          stop 'ERROR: aborting...'
        endif
        read(lu, err=95, end=94) nval, (val1(i), val2(i), i=1,nval)
        read(lu, err=95, end=94) (dval(i), i=1,nval)
        read(lu, err=98, end=94) llint,lint,dcplx,scplx
        print *,'  nval: ',nval
        print 50, (val1(i), val2(i), i=1,nval)
        print 51, (dval(i), i=1,nval)
        print *, 'extra:', llint,lint,dcplx,scplx
        close(lu, err=97)
      else
        if (verbose) then
          print *,'writing to ',filename(1:index(filename,' ')-1)
        endif
        open(lu, file=filename, form='unformatted', err=99)
        call tf_magic(cmagic, imagic)
        print *,'writing magic number: ''',cmagic,', ',imagic,''''
        write(lu, err=98) imagic
        nval=mval
        do i=1,nval
          val1(i)=i*4
          val2(i)=100*val1(i)
          dval(i)=dble(i)*15.d0
        enddo
        llint=1551
        lint=2662
        scplx=(14.5d0,15.4d0)
        dcplx=(4.5d0,5.4d0)
        write(lu, err=98) nval, (val1(i), val2(i), i=1,nval)
        write(lu, err=98) (dval(i), i=1,nval)
        write(lu, err=98) llint,lint,dcplx,scplx
        print *,'  nval: ',nval
        print 50, (val1(i), val2(i), i=1,nval)
        print 51, (dval(i), i=1,nval)
        print *, 'extra:', llint,lint,dcplx,scplx
        close(lu, err=97)
      endif
c
      stop
   50 format(2x,'val1/2:',2(1x,i10))
   51 format(2x,'val: ',f10.3)
   99 stop 'ERROR: opening for writing'
   98 stop 'ERROR: writing'
   97 stop 'ERROR: closing'
   96 stop 'ERROR: opening for reading'
   95 stop 'ERROR: reading'
   94 stop 'ERROR: reading - unexpected end'
      end
c
c
c ----- END OF fortranF77.f ----- 
