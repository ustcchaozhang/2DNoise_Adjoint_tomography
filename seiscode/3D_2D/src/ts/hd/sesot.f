c this is <sesot.f>
c------------------------------------------------------------------------------
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c SEt SOurce Time
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
c    02/02/99   V1.0   Thomas Forbriger
c    16/09/03   V1.1   allow to handle real data (no receiver shift)
c
c==============================================================================
c
      program sesot
c
      character*79 version
      parameter(version='SESOT   V1.1   SEt SOurce Time')
c
      character*80 infile,outfile,timestring
      character*200 line
      integer luin, luout, month, day, tfstr_trimlen, time_compare
      parameter(luin=10,luout=11)
      integer newsrct(7), oldsrct(7), newrect(7), oldrect(7), shift(7)
      character*6 srcdate
      character*10 srctime
      logical infree, addshift, noshift
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-n/
      data opthasarg/3*.FALSE./
      data optarg/3*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.3)) then
        print *,version
        print *,'Usage: sesot timestring infile outfile [-n]'
        print *,'   or: sesot -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'SEt SOurce Time'
        print *,' '
        print *,'This program shifts all time information in an sff datafile'
        print *,' '
        print *,'timestring   new time for source to be used'
        print *,'infile       input dataset'
        print *,'outfile      output dataset'
        print *,' '
        print *,'-n           do not shift receivers'
        print *,'             useful for real data'
        print *,' '
        call timeinfo
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
      noshift=optset(3)
c 
      call getarg(1, timestring)
      call getarg(2, infile)
      call getarg(3, outfile)
c
c------------------------------------------------------------------------------
c go
      call time_clear(oldsrct)
      call time_read(timestring, newsrct)
      oldsrct(1)=newsrct(1)
      oldsrct(2)=newsrct(2)
      call time_sprint(newsrct, timestring)
      print *,'new source time will be ',timestring(1:35)
      call time_sub(newsrct,oldsrct,shift)
      addshift=.true.
      if (time_compare(newsrct, oldsrct).lt.0) addshift=.false.
      call time_sprint(shift, timestring)
      if (addshift) then
        print *,'time-shift will be     +',timestring(1:35)
      else
        print *,'time-shift will be     -',timestring(1:35)
      endif
c 
      open(luin, file=infile, status='old', err=99)
      open(luout, file=outfile, status='new', err=98)
c 
    2 continue
        read(luin, '(a200)', err=95, end=1) line
        if (infree) then
          if (line(1:5).eq.'FREE ') infree=.false.
        else
          if (line(1:5).eq.'SRCE ') then
            print *,' '
            srcdate=line(75:80)
            srctime=line(82:91)
            call sffu_timesrce(srcdate, srctime, oldsrct)
            call time_sprint(oldsrct, timestring)
            print *,'read SRCE time:         ',timestring(1:35)
            call time_sub(newsrct,oldsrct,shift)
            addshift=.true.
            if (time_compare(newsrct, oldsrct).lt.0) addshift=.false.
            call time_sprint(shift, timestring)
            if (addshift) then
              print *,'time-shift will now be +',timestring(1:35)
            else
              print *,'time-shift will now be -',timestring(1:35)
            endif
            call time_getdate(day, month, newsrct)
            write(srcdate, '(3(i2.2))')
     &        int(newsrct(1))-100*int(newsrct(1)/100),
     &        month, day
            write(srctime, '(2(i2.2)f6.3)') newsrct(3),newsrct(4),
     &        float(newsrct(5))+1.e-3*float(newsrct(6))
            line(75:80)=srcdate
            line(82:91)=srctime
            print *,'new SRCE line is'
            print *,line(1:91)
          elseif (line(1:5).eq.'WID2 ') then
            if (.not.(noshift)) then
              print *,' '
              call sffu_timewid2(line, oldrect)
              call time_sprint(oldrect, timestring)
              print *,'read WID2 date:         ',timestring(1:35)
              if (addshift) then
                call time_add(oldrect,shift,newrect)
              else
                call time_sub(oldrect,shift,newrect)
              endif
              call time_sprint(newrect, timestring)
              print *,'new receiver date:      ',timestring(1:35)
              call sffu_setwid2time(line, newrect)
              print *,'new WID2 line is'
              print *,line(1:132)
            endif
          elseif (line(1:5).eq.'FREE ') then
            infree=.true.
          endif
        endif
        write(luout, '(a)', err=94) line(1:tfstr_trimlen(line))
      goto 2
c 
    1 close(luout, err=97)
      close(luin, err=96)
c
      stop
   99 stop 'ERROR: opening input file'
   98 stop 'ERROR: opening output file'
   97 stop 'ERROR: closing output file'
   96 stop 'ERROR: closing input file'
   95 stop 'ERROR: reading input file'
   94 stop 'ERROR: writing input file'
      end
c
c ----- END OF sesot.f -----
