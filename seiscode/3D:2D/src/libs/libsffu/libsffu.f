c this is <libsffu.f>
cS
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c a fortran library to manipulate and evaluate SFF data
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
c----------------------------------------------------------------------
c
c REVISIONS and CHANGES
c   27/05/97   V1.00   first version
c   24/06/97   V1.01   new dt-routine and changes sffu_seconds to
c                      evaluate full relative time
c   27/06/97   V1.02   - just in case we read year=month=day=0 from
c                        a SRCE or a WID2 line we set a relative date
c                      - added some new functions 
c   08/06/98   V1.03   - took code for epicentral distance in spherical
c                        coordinates as used in xseife.tcl by wolle
c   19/11/98   V1.04   - tfirst has to provide signed times (did not)
c                      - dttoseconds did miscalculate milliseconds
c   19/02/99   V1.05   - introduced sffu_srcetime
c   25/01/00   V1.06   call time_fullyear in sffu_timesrc to handle
c                      year 2000 correctly
c   29/05/00   V1.07   - relative dates in WID2 and SRCE are senseless  
c                        just leads to trouble with Y2K
c                      updated format for sffu_srcetime (fill with zeroes)
c   04/11/02   V1.08   - changed date to timearray in dttodate
c                        date is a reserved keyword
c   13/04/11   V1.09   check whether WID2 manipulating functions are
c                      called with a WID2 character string created by
c                      libfapidxx
c   01/12/2016 V1.10   use double precision for spherical geometry
c
c----------------------------------------------------------------------
c 
c subroutines and what they do
c
c sffu_libversion       returns current library version            V1.00
c sffu_timesrce         set date from SFF SRCE line                V1.07
c sffu_srcetime         set SRCE date from date                    V1.07
c sffu_timewid2         set date from SFF WID2 line                V1.07
c sffu_setwid2time      set date in SFF WID2 line                  V1.00
c sffu_seconds          calculate real seconds from date           V1.00
c sffu_dttodate         fill real sampling interval in date record V1.01
c sffu_offset           calculate receiver offset                  V1.10
c sffu_tfirst           calculate relative time of first sample    V1.02
c sffu_checkwid2        check WID2 ID sequence                     V1.09
c
c most subroutines that deal with time and date call subroutines from
c libtime.a
c
c======================================================================
c 
      real function sffu_libversion()
c
c returns current library version
c 
cE
      real version
      parameter(version=1.10)
      sffu_libversion=version
      return
      end
cS
c----------------------------------------------------------------------
c 
      subroutine sffu_srcetime(odate, date, time)
c
c set date and time string for SFF SRCE line from odate
c 
      integer odate(7)
      character date*(*), time*(*)
cE
      integer i, day, month, year
c
      year=odate(1)-(100*int(odate(1)/100))
      call time_getdate(day, month, odate)
      write(date, '(3(i2.2))') year, month, day
      write(time, '(3(i2.2),1x,i3.3)') (odate(i), i=3,6)
      return
      end
cS
c----------------------------------------------------------------------
c 
      subroutine sffu_timesrce(date, time, odate)
c
c set odate from date and time string taken from SFF SRCE line
c 
c 27/06/97 evaluate correct relative date
c 25/01/00 call time_fullyear with odate(1) to handle year 2000
c 29/05/00 relative dates in SRCE are senseless  
c          just leads to trouble with Y2K
c
      integer odate(7)
      character date*(*), time*(*)
cE
      integer i, day, month
c
      read(date, '(3(i2))') odate(1), month, day
      call time_fullyear(odate(1))
      read(time, '(3(i2),1x,i3)') (odate(i), i=3,6)
      call time_setdoy(day, month, odate)
      odate(7)=0
      call time_norm(odate)
      return
      end
cS
c----------------------------------------------------------------------
c 
      subroutine sffu_timewid2(wid2line, odate)
c
c set odate from SFF WID2 line
c
c 27/06/97 evaluate correct relative date
c 29/05/00 relative dates in WID2 are senseless  
c          just leads to trouble with Y2K
c
      integer odate(7)
      character wid2line*(*)
cE
      integer i, day, month
c
      call sffu_checkwid2(wid2line, 'sffu_timewid2')
      read(wid2line(6:28), '(i4,5(1x,i2),1x,i3)') 
     &  odate(1), month, day, (odate(i), i=3,6)
      call time_fullyear(odate(1))
      call time_setdoy(day, month, odate)
      odate(7)=0
      call time_norm(odate)
      return
      end
cS
c----------------------------------------------------------------------
c 
      subroutine sffu_setwid2time(wid2line, date)
c
c set date in SFF WID2 line
c
      integer date(7)
      character wid2line*(*)
cE
      character*23 string
      integer d(7), i, day, month
c
      call sffu_checkwid2(wid2line, 'sffu_setwid2time')
      call time_copy(date, d)
      call time_norm(d)
      call time_getdate(day, month, d)
      if (d(7).ne.0) write(0, *)
     &  'WARNING (sffu_setwid2time): will ignore microseconds'
      write(string, '(i4.4,2(1h/,i2.2),1x,2(i2.2,1h:),i2.2,1h.,i3.3)') 
     &  d(1), month, day, (d(i), i=3,6)
      wid2line=wid2line(1:5)//string(1:23)//wid2line(29:)
      return
      end
cS
c----------------------------------------------------------------------
c
      real function sffu_seconds(date)
c
c calculate real seconds from relative date record
c
      integer date(7)
cE
      real seconds
c
      if (date(1).ne.0) then
        print *,'WARNING (sffu_seconds): will ignore year value'
      endif
      seconds=float(date(2)*86400)+
     &        float(date(3)*3600)+
     &        float(date(4)*60)+
     &        float(date(5))+
     &        float(date(6))*1.e-3+
     &        float(date(7))*1.e-6
      sffu_seconds=seconds
      return
      end
cS
c----------------------------------------------------------------------
c
      subroutine sffu_dttotime(dt, timearray)
c
c a real valued sampling interval in units of seconds will be
c filled into a relative date array
c
      integer timearray(7)
      real dt
cE 
      real cdt
c
      cdt=dt
      call time_clear(timearray)
      timearray(1)=0
      timearray(2)=int(cdt/86400.)
      cdt=cdt-float(timearray(2)*86400)
      timearray(3)=int(cdt/3600.)
      cdt=cdt-float(timearray(3)*3600)
      timearray(4)=int(cdt/60.)
      cdt=cdt-float(timearray(4)*60)
      timearray(5)=int(cdt)
      cdt=(cdt-float(timearray(5)))*1000.
      timearray(6)=int(cdt)
      cdt=(cdt-float(timearray(6)))*1000.
      timearray(7)=int(cdt)
      return
      end
cS
c----------------------------------------------------------------------
c 
      real function sffu_offset(cs,c1,c2,c3,rs,r1,r2,r3)
c
c calculate distance between source and receiver
c
      character*1 cs,rs
      real c1,c2,c3,r1,r2,r3
cE
      double precision distance
      double precision rtet, stet, rphi, sphi
      double precision convfac, earthradius, cosepi
      parameter(convfac=0.017453293d0, earthradius=6371.d0)
c
      if (cs.ne.rs) stop 'ERROR (sffu_offset): different reference frames'
      if (cs.eq.'C') then
        distance=sqrt((c1-r1)**2+(c2-r2)**2+(c3-r3)**2)
      elseif (cs.eq.'S') then
c        print *,'source ',c1,c2,c3
c        print *,'receiver ',r1,r2,r3
        rtet=(90.-r1)*convfac 
        stet=(90.-c1)*convfac 
        rphi=r2*convfac
        sphi=c2*convfac
c        print *,'source ',stet,sphi
c        print *,'receiver ',rtet,rphi
        cosepi=cos(stet)*cos(rtet)+sin(stet)*sin(rtet)*cos(sphi-rphi)
        distance=acos(cosepi)*earthradius*1.d3
c        print *,'distance ',distance
      else
        stop 
     &'ERROR (sffu_offset): reference frame is neither cartesian nor spherical'
      endif
      sffu_offset=sngl(distance)
      return
      end
cS
c----------------------------------------------------------------------
c
      real function sffu_tfirst(wid2line, time, date)
c 
c a simple hack to calculate a time difference between source and
c receiver onset
c
      character wid2line*132, time*10, date*6
cE 
      integer sdate(7), rdate(7), diff(7)
      real sffu_seconds, secs
      integer time_compare
c 
      call sffu_checkwid2(wid2line, 'sffu_tfirst')
      call sffu_timesrce(date, time, sdate)
      call sffu_timewid2(wid2line, rdate)
      call time_sub(sdate, rdate, diff)
      secs=sffu_seconds(diff)
      if (time_compare(sdate, rdate).eq.1) secs=-1.*secs
      sffu_tfirst=secs
      return
      end
c 
cS
c----------------------------------------------------------------------
c
      subroutine sffu_checkwid2(wid2line, caller)
c
c check WID2 character string ID
c
      character*132 wid2line
      character*(*) caller
c
c wid2line: WID2 line character string
c caller:   name of calling function or program
cE
      if (wid2line(1:4).ne.'WID2') then
        print *,'ERROR: sffu_checkwid2 called by ',
     &    caller,':'
        if (wid2line(1:4).eq.'WIDY') then
          print *,'       WID2 character string apparently ',
     &      'was created by libfapidxx'
          print *,'       identifier is WIDY, not WID2'
        else if (wid2line(1:4).eq.'WIDX') then
          print *,'       WID2 character string apparently ',
     &      'was created by libsffxx'
          print *,'       identifier is WIDX, not WID2'
        else
          print *,'       WID2 character string has unknown ',
     &      'identifier: ', wid2line(1:4)
        endif
        stop 'cannot handle this...'
      endif
      return
      end
c
c ----- END OF libsffu.f -----
