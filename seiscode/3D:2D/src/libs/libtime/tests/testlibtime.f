c this is <testlibtime.f>
c
c Copyright 1997 Thomas Forbriger
c
c a program to test libtime.a
c
c ----
c libtime is free software; you can redistribute it and/or modify
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
c     05/12/2007  internal functions are tested after migration to g77 on
c                 fenoglio; SFF reading waits for migration of libsffu
c

      program testlibtime
c
      integer date1(7), date2(7), date3(7), iargc, day, month, year
      character*40 string, filename
      character*40 date, time
      character*200 line
      character*132 wid2line
      real sffu_seconds, seconds
      real time_libversion

      date1(1)=97
      date1(2)=150
      date1(3)=12
      date1(4)=10
      date1(5)=9
      date1(6)=123
      date1(7)=456

      call head('time_libversion')
      print *,'libversion: ',time_libversion()

      call head('time_isleapyear')
      call testleap(2000)
      call testleap(1996)
      call testleap(1997)
      call testleap(92)
      call testleap(0)
      call testleap(1900)

      call head('time_fullyear')
      year=0
      call time_fullyear(year)
      print *,'year    0: ',year
      year=15
      call time_fullyear(year)
      print *,'year   15: ',year
      year=97
      call time_fullyear(year)
      print *,'year   97: ',year
      year=70
      call time_fullyear(year)
      print *,'year   70: ',year
      year=100
      call time_fullyear(year)
      print *,'year  100: ',year
      year=69
      call time_fullyear(year)
      print *,'year   69: ',year
      year=99 
      call time_fullyear(year)
      print *,'year   99: ',year
      year=1831
      call time_fullyear(year)
      print *,'year 1831: ',year
      year=2061
      call time_fullyear(year)
      print *,'year 2061: ',year
      call time_fullyear(date1(1))
      call time_sprint(date1, string)
      print *,'full year: ',string
      date1(1)=50
      call time_fullyear(date1(1))
      call time_sprint(date1, string)
      print *,'full year 50: ',string

      call head('time_sprint (time_getdate is implicit)')
      date1(1)=97
      date1(2)=150
      date1(3)=12
      date1(4)=10
      date1(5)=9
      date1(6)=123
      date1(7)=456
      call arout(date1)
      call time_sprint(date1, string)
      print *,'date: ',string
      date1(1)=96
      call time_sprint(date1, string)
      print *,'other year: ',string
      date1(2)=366
      call time_sprint(date1, string)
      print *,'last doy (set by doy): ',string
      date1(1)=0
      date1(2)=12345
      call time_sprint(date1, string)
      print *,'relative time value: ',string

      call head('time_setdoy')
      date1(1)=97
      date1(2)=150
      date1(3)=12
      date1(4)=10
      date1(5)=9
      date1(6)=123
      date1(7)=456
      call time_setdoy(1, 1, date1)
      call time_sprint(date1, string)
      print *,'jan first: ',string
      call time_setdoy(31, 12, date1)
      call time_sprint(date1, string)
      print *,'dec last: ',string

      call head('time_clear')
      call time_clear(date1)
      call arout(date1)
      call time_sprint(date1, string)
      print *,'after clear: ',string

      call head('time_norm')
      date1(1)=96
      date1(2)=1
      date1(3)=24
      date1(4)=60
      date1(5)=60
      date1(6)=1000
      date1(7)=1000
      call arout(date1)
      call time_norm(date1)
      call arout(date1)
      call time_sprint(date1, string)
      print *,'after norm: ',string
      date1(1)=96
      date1(2)=1
      date1(3)=0
      date1(4)=0
      date1(5)=0
      date1(6)=000
      date1(7)=-1
      call arout(date1)
      call time_norm(date1)
      call arout(date1)
      call time_sprint(date1, string)
      print *,'after norm: ',string
      date1(1)=0
      date1(2)=5003
      date1(3)=24
      date1(4)=60
      date1(5)=60
      date1(6)=1000
      date1(7)=1000
      call arout(date1)
      call time_norm(date1)
      call arout(date1)
      call time_sprint(date1, string)
      print *,'after norm: ',string
      date1(1)=0
      date1(2)=123
      date1(3)=0
      date1(4)=0
      date1(5)=0
      date1(6)=000
      date1(7)=-1
      call arout(date1)
      call time_norm(date1)
      call arout(date1)
      call time_sprint(date1, string)
      print *,'after norm: ',string

      call head('time_getdate after time_clear')
      call time_clear(date1)
      call arout(date1)
      call time_getdate(day, month, date1)
      print *,'date after time_clear ',day,month

      call head('time_add and time_sub')
      date1(1)=1997
      date1(2)=150
      date1(3)=12
      date1(4)=34
      date1(5)=56
      date1(6)=123
      date1(7)=456

      call subhead('absolute & absolute')
      call time_copy(date1, date2)
      date2(1)=1999
      call subaddtest(date1, date2)
      call subaddtest(date2, date1)
      call time_copy(date1, date2)
      date2(3)=11
      date2(4)=33
      date2(5)=55
      date2(6)=122
      date2(7)=455
      call subaddtest(date2, date1)
      date2(6)=127
      call subaddtest(date2, date1)

      call subhead('absolute & relative')
      call time_clear(date2)
      date2(2)=12
      date2(3)=40
      date2(4)=40
      date2(5)=20
      date2(6)=20
      date2(7)=20
      call subaddtest(date2, date1)
      call subaddtest(date1, date2)
      
      call subhead('relative & relative')
      date1(1)=0
      date1(2)=10
      date1(3)=11
      date1(4)=12
      date1(5)=13
      date1(6)=14
      date1(7)=15
      date1(1)=0
      date1(2)=1
      date1(3)=12
      date1(4)=13
      date1(5)=14
      date1(6)=15
      date1(7)=16
      call subaddtest(date2, date1)
      call subaddtest(date1, date2)

      call head('time_copy')
      date1(1)=1997
      date1(2)=150
      date1(3)=12
      date1(4)=30
      date1(5)=31
      date1(6)=123
      date1(7)=456
      call time_clear(date2)
      call time_sprint(date1, string)
      print *,'date1: ',string
      call time_sprint(date2, string)
      print *,'date2 after clear: ',string
      call time_copy(date1, date2)
      call time_sprint(date2, string)
      print *,'date2 after copy: ',string

      call head('time_finish')
      date1(1)=57
      date1(2)=450
      date1(3)=12
      date1(4)=0
      date1(5)=4781
      date1(6)=0
      date1(7)=45456
      call arout(date1)
      call time_finish(date1)
      call arout(date1)

      call head('time_compare')
      date1(1)=1997
      date1(2)=150
      date1(3)=12
      date1(4)=30
      date1(5)=31
      date1(6)=123
      date1(7)=456
      call time_copy(date1, date2)
      call cmptest(date1, date2)
      date1(2)=146
      call cmptest(date1, date2)
      date1(1)=0
      date1(2)=5667
      call time_copy(date1, date2)
      date2(4)=12
      date2(5)=45
      call cmptest(date1, date2)

      call head('time_mul and time_div and time_nfit')
      date1(1)=0
      date1(2)=0
      date1(3)=0
      date1(4)=0
      date1(5)=1
      date1(6)=2
      date1(7)=3
      call time_copy(date1, date2)
      date2(5)=0
      date2(6)=1
      date2(7)=500
      call divmultest(date1, 5, date2)
      call divmultest(date1, 8567, date2)
      call divmultest(date1, 2658567, date2)

      if (iargc().eq.1) then
      call head('sffu_timesrce and sffu_timewid2 and sffu_setwid2time')
        call getarg(1, filename)
	print *,'evaluate sff file ',filename
	open(10, file=filename, status='old', err=99)
    1 read(10, '(a200)', end=2, err=98) line
	if (line(1:5).eq.'SRCE ') then
        print *,'found SRCE line:'
        print *,line(1:78)
	  date(1:6)=line(75:80)
	  time(1:10)=line(82:91)
	  call sffu_timesrce(date, time, date1)
	  call time_sprint(date1, string)
	  print *,'SRCE time: ',string
	elseif (line(1:5).eq.'WID2 ') then
        write(wid2line, '(a132)') line(1:132)
	  call sffu_timewid2(wid2line, date1)
	  call time_sprint(date1, string)
	  print *,'WID2 time: ',string
	endif
      goto 1
    2 close(10)
      print *,'closed file'
      date1(1)=1997
      date1(2)=150
      date1(3)=12
      date1(4)=30
      date1(5)=31
      date1(6)=123
      date1(7)=456
      call time_sprint(date1, string)
      print *,'set WID2 to ',string
      print *,wid2line
      call sffu_setwid2time(wid2line, date1)
      print *,wid2line
      date1(1)=98
      date1(7)=0
      call time_sprint(date1, string)
      print *,'set WID2 to ',string
      print *,wid2line
      call sffu_setwid2time(wid2line, date1)
      call time_copy(date1, date2)
      date2(7)=0
      date2(6)=0
      date2(5)=0
      date2(4)=0
      date2(3)=0
      print *,wid2line
      call time_sprint(date2, string)
      call time_sub(date1, date2, date3)
      print *,'time span from ',string
      seconds=sffu_seconds(date3)
      print *,'to first sample is ',seconds,' seconds:'
      call time_sprint(date3, string)
      print *,string
      endif

      call normbugtest

      stop
   99 stop 'ERROR: opening file'
   98 stop 'ERROR: reading from file'
      end

c======================================================================

      subroutine subaddtest(date1,date2)
      integer date1(8), date2(8), date3(8), date4(8)
      character*40 string
      call time_sprint(date1, string)
      print *,'***           date1: ',string
      call time_sprint(date2, string)
      print *,'              date2: ',string
      call time_sub(date1, date2, date3)
      call time_sprint(date3, string)
      print *,'  date1-date2=date3: ',string
      call time_add(date3, date2, date4)
      call time_sprint(date4, string)
      print *,'  date3+date2=date4: ',string
      return
      end

c----------------------------------------------------------------------
      subroutine head(routine)
      character routine*(*)
      print *,' '
      print *,'TEST ',routine
      print *,'===='
      return
      end
c----------------------------------------------------------------------
      subroutine subhead(sub)
      character sub*(*)
      print *,' '
      print *,'**** ',sub
      print *,' '
      return
      end
c----------------------------------------------------------------------
      subroutine arout(date)
      integer date(7)
      integer i
      print 50,(date(i), i=1,7)
   50 format('date-array: ',7(i6,1x))
      return
      end
c----------------------------------------------------------------------
      subroutine cmptest(date1, date2)
      integer date1(7), date2(7)
      character*34 string1, string2
      integer time_compare, val
      call time_sprint(date1, string1)
      call time_sprint(date2, string2)
      val=time_compare(date1, date2)
      if (val.gt.0) then
        print *,string1, '> ',string2
      else if (val.lt.0) then
        print *,string1, '< ',string2
      else
        print *,string1, '= ',string2
      endif
      val=time_compare(date2, date1)
      if (val.gt.0) then
        print *,string2, '> ',string1
      else if (val.lt.0) then
        print *,string2, '< ',string1
      else
        print *,string2, '= ',string1
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine divmultest(date1, n, date2)
      integer date1(7), date2(7), n, nfit
      integer date3(7), date4(7), rest, full(7)
      character*40 string1, string3, string4, stringf
      print *,'****'
      call time_sprint(date1, string1)
      call time_mul(date1, date3, n)
      call time_sprint(date3, string3)
      print *,string1,' * ',n,' -> ',string3
      call time_nfit(date3, date1, nfit, full)
      call time_sprint(full, stringf)
      print *,'fitting: ',nfit,' to ',string3,' leads to ',stringf
      call time_div(date3, date4, n, rest)
      call time_sprint(date4, string4)
      print *,string3,' / ',n,' -> ',string4, ' rest ', rest
      call time_add(date3, date2, date4)
      call time_div(date4, date3, n, rest)
      call time_sprint(date4, string4)
      call time_sprint(date3, string3)
      print *,string4,' / ',n,' -> ',string3, ' rest ', rest
      call time_nfit(date4, date1, nfit, full)
      call time_sprint(full, stringf)
      print *,'fitting: ',nfit,' to ',string4,' leads to ',stringf
      call time_add(date4, date1, date3)
      call time_sprint(date3, string3)
      call time_nfit(date3, date1, nfit, full)
      call time_sprint(full, stringf)
      print *,'fitting: ',nfit,' to ',string3,' leads to ',stringf
      return
      end
c----------------------------------------------------------------------
      subroutine testleap(year)
      integer year
      logical time_isleapyear
      if (time_isleapyear(year)) then
      else
        print 50,year
      endif
        print 51,year
      return
   50 format('year ',i4,' is a leap year')
   51 format('year ',i4,' is no leap year')
      end
c ----------------------------------------------------------------------
      subroutine normbugtest()
c
c test bug of norm-subroutine found in 11/2017
c if seconds are integer multiples of -60 or hours are integer multiples
c of -24 or similar, the norm function does not provide the desired
c output
c
      integer date1(7), date2(7), date3(7)
      character string*40
c

      print *,'isolate'
      call time_clear(date1)
      date1(1)=2018
      date1(2)=13   
      date1(3)=-23
      date1(4)=0
      call time_util_warning_report_time('norm bug test',date1)
      call time_norm(date1)
      call time_util_warning_report_time('norm bug test',date1)
      call time_norm(date1)
      call time_util_warning_report_time('norm bug test',date1)
      return
      end

