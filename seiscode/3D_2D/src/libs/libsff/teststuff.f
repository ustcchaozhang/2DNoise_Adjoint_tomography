c this is <teststuff.f>
c
c Copyright (c) 1996 by Wolfgang Friederich
c Copyright (c) 1996, 2010 by Thomas Forbriger 
c
c ----
c This file is part of libsff.
c
c libsff is free software; you can redistribute it and/or modify
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
c just some code to test the stuff library
c
c
c
      call test_modwid2
c
      call test_timefunc
      stop
      end
c
c
c======================================================================
c
c modwid2
c
      subroutine test_modwid2
      character*132 wid2line
      integer ierr
      print *,'TESTING modify wid2 routines'
      call sff_PrepWid2(1000,0.25,'sta',0,0,0,0,0,'com',
     &  'aux','ins',0.,2.,3.,4.,5.,wid2line,ierr)
      print *,'  wid2line is:'
      print *,'  :',wid2line,':'
      print *,' '
      print *,'  set date to 30/09/1996'
      call sff_ModWid2date(wid2line, 1996,09,30)
      print *,'  wid2line is:'
      print *,'  :',wid2line,':'
      print *,' '
      print *,'  set time to 10:23:56.312'
      call sff_ModWid2time(wid2line, 10, 23, 56.312)
      print *,'  wid2line is:'
      print *,'  :',wid2line,':'
      print *,' '
      print *,'  set samprat to 34.'
      call sff_ModWid2samprat(wid2line, 34.)
      print *,'  wid2line is:'
      print *,'  :',wid2line,':'
      print *,' '
      print *,'  set nsamp to 50'
      call sff_ModWid2samps(wid2line, 50)
      print *,'  wid2line is:'
      print *,'  :',wid2line,':'
      print *,' '
      print *,'  shift with 650 minutes and 30.5 seconds'
      call sff_ModWid2shift(wid2line, 650., 30.5)
      print *,'  wid2line is:'
      print *,'  :',wid2line,':'
      return
      end
c----------------------------------------------------------------------
c
c timefunc
c
      subroutine test_timefunc
      logical sff_TimeIsLeapYear
      integer sff_TimeGetDOY
      integer years(15), leaps(15),i
      integer year1, year2, month1, month2, day1, day2, doy
      integer hour1, hour2, minute1, minute2
      integer doy1, doy2, year, hour, minute
      real second1, second2, second
      print *,' '
      print *,' '
      print *,'TESTING time handling routines'
c 
c leap year routine
c
      print *,'  leap years are marked 1'
      do i=1,15
        years(i)=i+1896
        leaps(i)=0
        if (sff_TimeIsLeapYear(years(i))) leaps(i)=1
      enddo
      print 1,(years(i),i=1,15),(leaps(i),i=1,15)
    1 format(2x,15(i5)/2x,15(i5))
      do i=1,15
        years(i)=i+1988
        leaps(i)=0
        if (sff_TimeIsLeapYear(years(i))) leaps(i)=1
      enddo
      print 1,(years(i),i=1,15),(leaps(i),i=1,15)
c 
c DOY routines
c
      print *,' '
      print *,'  calculate day of year'
      year1=1996
      month1=3
      day1=1
      doy=sff_TimeGetDOY(year1,month1,day1)
      year2=year1
      call sff_TimeSetDOY(doy, year2, month2, day2)
      print 2,year1,month1,day1,doy,year2,month2,day2
    2 format(2x,i4.4,1h/,i2.2,1h/,i2.2,' --> ',i3,' --> ',
     &       i4.4,1h/,i2.2,1h/,i2.2)
      year1=1995
      month1=3
      day1=1
      doy=sff_TimeGetDOY(year1,month1,day1)
      year2=year1
      call sff_TimeSetDOY(doy, year2, month2, day2)
      print 2,year1,month1,day1,doy,year2,month2,day2
      year1=1996
      month1=2
      day1=18
      doy=sff_TimeGetDOY(year1,month1,day1)
      year2=year1
      call sff_TimeSetDOY(doy, year2, month2, day2)
      print 2,year1,month1,day1,doy,year2,month2,day2
      year1=1995
      month1=2
      day1=18
      doy=sff_TimeGetDOY(year1,month1,day1)
      year2=year1
      call sff_TimeSetDOY(doy, year2, month2, day2)
      print 2,year1,month1,day1,doy,year2,month2,day2
      year1=1996
      month1=12
      day1=31
      doy=sff_TimeGetDOY(year1,month1,day1)
      year2=year1
      call sff_TimeSetDOY(doy, year2, month2, day2)
      print 2,year1,month1,day1,doy,year2,month2,day2
      year1=1995
      month1=12
      day1=31
      doy=sff_TimeGetDOY(year1,month1,day1)
      year2=year1
      call sff_TimeSetDOY(doy, year2, month2, day2)
      print 2,year1,month1,day1,doy,year2,month2,day2
      year1=1996
      month1=1 
      day1=1 
      doy=sff_TimeGetDOY(year1,month1,day1)
      year2=year1
      call sff_TimeSetDOY(doy, year2, month2, day2)
      print 2,year1,month1,day1,doy,year2,month2,day2
c
c add routine
c
    3 format(2x,3(i4.4,1h/,i3,1x,2(i2.2,1h:),f6.3:3x)) 
      year1=1996
      doy1=366
      hour1=1
      minute1=1
      second1=1.
      year2=0
      doy2=0
      hour2=0
      minute2=0
      second2=5.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=0
      minute2=10
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=10
      minute2=0
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=10
      hour2=0
      minute2=0
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=10
      doy2=0
      hour2=0
      minute2=0
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=0
      minute2=0
      second2=50.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=0
      minute2=0
      second2=60.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=0
      minute2=50
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=0
      minute2=60
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=0
      minute2=600
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=23
      minute2=00
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=25
      minute2=0
      second2=0.
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      year2=0
      doy2=0
      hour2=0
      minute2=650
      second2=30.5
      call sff_TimeAdd(year1, doy1, hour1, minute1, second1,
     &                 year2, doy2, hour2, minute2, second2,
     &                 year, doy, hour, minute, second)
      print 3, year1,doy1,hour1,minute1,second1,
     &         year2,doy2,hour2,minute2,second2,
     &         year,doy,hour,minute,second
      return
      end
