c this is <sesoc.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c SEt SOurce Coordinate
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
c    14/03/2002   V1.0   Thomas Forbriger
c    29/06/2007   V1.1   program is able to generate a new SRCE line
c    03/07/2007   V1.2   we have to write the file header FREE block prior to
c                        the SRCE line in any case, since this order is hard
c                        coded in libsffxx reading and libstuff.f sff_WOpenFS
c
c ============================================================================
      program sesoc
c
      character*79 version
      parameter(version='SESOC   V1.2   SEt SOurce Coordinate')
c
      character*80 infile,outfile,coordstring
      character*200 line
      character*10 code
      character*1 CS
      real CX,CY,CZ
      integer luin, luout, tfstr_trimlen
      parameter(luin=10,luout=11)
      logical infree, hasfree, hassrce
      logical createnewsrce,writenewsrcenow
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
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
      if ((argument(1:5).eq.'-help').or.(iargc().ne.3)) then
        print *,version
        print *,'Usage: sesoc C,x,y,z infile outfile'
        print *,'   or: sesoc -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'SEt SOurce Coordinate'
        print *,' '
        print *,'This program sets the source ',
     &    'location in an SFF data file'
        print *,' '
        print *,'C,x,y,z      coordinate according to SFF specification'
        print *,'             C: coordinate system'
        print *,'             x,y,z: coordinates ',
     &    '(for definition see below)'
        print *,'infile       input dataset'
        print *,'outfile      output dataset'
        print *,' '
        print *,'This program is able to generate a new SRCE line, if'
        print *,'there is none present in the input file.'
        print *,' '
        call coordinfo
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
c 
      call getarg(1, coordstring)
      call getarg(2, infile)
      call getarg(3, outfile)
c
c------------------------------------------------------------------------------
c go
      read (coordstring, '(a1,1x,3(g10.6))') CS,cx,cy,cz
c 
      open(luin, file=infile, status='old', err=99)
      open(luout, file=outfile, status='new', err=98)
c 
      createnewsrce=.false.
      writenewsrcenow=.false.
    2 continue
        read(luin, '(a200)', err=95, end=1) line
        if (infree) then
          if (line(1:5).eq.'FREE ') infree=.false.
          writenewsrcenow=.true.
        else
          if (line(1:5).eq.'STAT ') then
            code=line(28:37)
            hasfree=(index(code,'F').gt.0)
            hassrce=(index(code,'S').gt.0)
            if (.not.hassrce) then
              createnewsrce=.true.
              if (hasfree) then
                write(line(28:37), '(a)') 'FS'
              else
                write(line(28:37), '(a)') 'S'
                writenewsrcenow=.true.
              endif
            endif
          elseif (line(1:5).eq.'SRCE ') then
            print *,' '
            line(27:27)=CS
            write(line(29:43), '(f15.6)') CX
            write(line(44:58), '(f15.6)') CY
            write(line(59:73), '(f15.6)') CZ
            print *,'new SRCE line is'
            print *,line(1:91)
          elseif (line(1:5).eq.'FREE ') then
            infree=.true.
          endif
        endif
        if (writenewsrcenow) then
          write(luout, '(a)', err=94) line(1:tfstr_trimlen(line))
          write(line, '(a200)') ' '
   50      format(a5, a20, 1x, a1, 1x, 3f15.6, 1x, a6, 1x, a10)
          write(line,50) 'SRCE ', 'NSP                 ', 
     &      cs, cx, cy, cz, '700101', '000000.000'
          print *,'file contains no SRCE line - I add one'
          print *,'new SRCE line is'
          print *,line(1:91)
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
c======================================================================
c
      subroutine coordinfo
c
      print *,'Definition of SRCE line in SFF'
      print *,'------------------------------'
      print *,' '
      print *,'  This line holds information about the source '
     &       ,'that caused the'
      print *,'  seismic signal'
      print *,' '
      print *,'  position   format   contents'
      print *,'  1-5        a5       SRCE  (identifier)'
      print *,'  6-25       a20      type of source (any '
     &       ,'string like "earthquake")'
      print *,'  27         a1       type of coordinate '
     &       ,'system:'
      print *,'                          C: cartesian    S: '
     &       ,'spherical'
      print *,'  29-43      f15.6    c1   x               '
     &       ,'latitude'
      print *,'  44-58      f15.6    c2   y               '
     &       ,'longitude'
      print *,'  59-73      f15.6    c3   z               '
     &       ,'height'
      print *,'                      see below for comments '
     &       ,'on coordinate specification'
      print *,'  75-80      a6       date of source event: '
     &       ,'yymmdd'
      print *,'  82-91      a10      time of source event: '
     &       ,'hhmmss.sss'
      print *,' '
      print *,' '
      print *,'Coordinate Specification'
      print *,'------------------------'
      print *,' '
      print *,'  Notice that given coordinates imply a '
     &       ,'spatial relation between'
      print *,'  the source location and the receiver '
     &       ,'locations. While spherical'
      print *,'  coordinates refer to a fixed reference '
     &       ,'frame on the earth, cartesian'
      print *,'  coordinates refer to an arbitrary origin. '
     &       ,'The creator of the'
      print *,'  datafile is responsible to take care that '
     &       ,'coordinate information'
      print *,'  is consistent between the SRCE line and the '
     &       ,'several possible'
      print *,'  INFO lines.'
      print *,' '
      print *,'  cartesian coordinates'
      print *,' '
      print *,'    x, y and z are vector components in a '
     &       ,'right handed cartesian'
      print *,'    reference frame. While x and y lie '
     &       ,'arbitrary orientated in the'
      print *,'    horizontal plane, z is counted positive '
     &       ,'upwards from an arbitrary'
      print *,'    reference level (preferably the free '
     &       ,'surface). All three coordinate'
      print *,'    values are measured in meters.'
      print *,' '
      print *,'  spherical coordinates'
      print *,' '
      print *,'    Latitude and longitude are given in the '
     &       ,'geographical reference frame'
      print *,'    and measured in degrees. The height value '
     &       ,'gives the height above'
      print *,'    the geoid and is measured in meters.'
c
      return
      end
c
c ----- END OF sesoc.f ----- 
