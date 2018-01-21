c this is <chaco.f>
c
c Copyright 1998, 2010 by Thomas Forbriger
c
c a little program to change the coordinate information in
c an sff datafile
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
c 30/04/1997   Thomas Forbriger   (IfG Stuttgart)
c 09/01/98   V1.1   set source type too
c 24/06/00   V1.2   don't use the error output channel for standard usage info
c 17/01/11   V1.3   use libfapidxx interface
c 12/04/11   V1.4   support different input and output file type
c
c======================================================================
      program chaco
      character*79 version
      parameter(version='CHACO   V1.4   change coordinates')
   
      integer maxsamp, maxfree
      parameter(maxsamp=100000, maxfree=400)

      integer luin, luout, luco
      parameter(luin=10, luout=11, luco=12)

      character*80 infile, outfile, cofile

      real c1,c2,c3,o1,o2,o3
      character*1 cs, os

      character*80 free(maxfree)
      integer nfree, nsamp, lenmax, nstack, chan
      real tanf, dt
      real fdata(maxsamp)
      integer idata(maxsamp)
      equivalence(fdata, idata)

      integer ierr
      character code*10, timestamp*20, date*20, time*20, type*40
      character stype*40
      character wid2line*132
      real sffversion
      logical last


c commandline
      integer maxopt, lastarg, iargc
      character*80 arg, fileformat, outformat
      parameter(maxopt=2)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)

c here are the keys to our commandline options
      data optid/'-ty','-ot'/
      data opthasarg/2*.true./
      data optarg/2*'sff'/

c----------------------------------------------------------------------
c output usage
      print *, version
      print *, 'Usage: chaco infile outfile cofile c1,c2,c3 cs'
      print *,'              sourcetype [-ty f] [-ot f]'
      print *, 'or:    chaco -help'
      print *, 'or:    chaco -xhelp'
      if (iargc().eq.1) then
        call getarg(1, arg)
        if (arg(1:5).eq.'-xhelp') then
          call sff_help_details
          stop
        else if (arg(1:5).eq.'-help') then
          print *,'infile      sff input data file'
          print *,'outfile     sff output data file'
          print *,'cofile      file containing receiver coordinates'
          print *,'            (each line containes a vector triple in'
          print *,'            free format)'
          print *,'c1,c2,c3    vector coordinate triple for the source'
          print *,'            location'
          print *,'cs          either C or S indicating the type of'
          print *,'            coordinate system to use'
          print *,'sourcetype  something like ''sledge-hammer'' or ''SISSY'' '
          print *,' '
          print *,'-ty f       select data file format f'
          print *,'-ot f       select output data file format f, if'
          print *,'            output format differs from input format'
          print *,' '
          print *,'As infile and outfile will be open at the same time'
          print *,'the must not have the same name!'
          print *,' '
          print *,'This binary is compiled for a maximum of'
          print *,'  FREE block lines: ',maxfree
          print *,'           samples: ',maxsamp
          print *,' '
          call sff_help_formats
          stop
        endif
      endif
      if (iargc().lt.6) stop 'ERROR (chaco): wrong number of arguments'

c----------------------------------------------------------------------
c get arguments
      call getarg(1, infile)
      call getarg(2, outfile)
      call getarg(3, cofile)
      call getarg(4, arg)
      read(arg, *, err=99) c1,c2,c3
      call getarg(5, arg)
      call getarg(6, stype)
      cs=arg(1:1)

      call tf_cmdline(7, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      fileformat=optarg(1)
      outformat=fileformat
      if (optset(2)) outformat=optarg(2)

c----------------------------------------------------------------------
c open coordinate file
      print *,'opening inputfile ',cofile(1:index(cofile,' '))
      open(luco, file=cofile, status='old', err=93)

c----------------------------------------------------------------------
c process file header
c open input file
      print *,'opening input file ',infile(1:index(infile,' '))
      call sff_select_format(fileformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting file format'
      call sff_ROpenFS(luin, infile,
     &  sffversion, timestamp, code, nfree, free, lenmax, maxfree,
     &  type, os, o1, o2, o3,
     &  date, time, ierr)
      if (ierr.ne.0) stop 'ERROR (chaco): opening input file'
c check return values
      print *,'file was created ',timestamp(1:index(timestamp,' ')),
     &  ' by SFF version ',sffversion
      if (index(code,'S').eq.0)
     &  stop 'ERROR (chaco): file containes no SRCE line'
c add FREE line
      nfree=nfree+1
      write(free(nfree), 50, err=90) os, o1, o2, o3
   50 format("changed SRCE line from: (cs,c1,c2,c3) ",a1,3(1x,f12.3))
c open output file
      print *,'opening output file ',outfile(1:index(outfile,' '))
      call sff_select_output_format(outformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting output file format'
      call sff_WOpenFS(luout, outfile, free, nfree, stype,
     &  cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR (chaco): opening output file'

c----------------------------------------------------------------------
c process channels
      chan=1
    1 print *,'working on trace ',chan
      nsamp=maxsamp
c read input trace
      call sff_RTraceFI(luin, tanf, dt, wid2line, nsamp, fdata, idata,
     &  code, last, nfree, free, maxfree, lenmax,
     &  os, o1, o2, o3, nstack, ierr)
      if (ierr.ne.0) stop 'ERROR (chaco): reading input trace'
c check return values
      if (index(code,'I').eq.0)
     &  stop 'ERROR (chaco): trace containes no INFO line'
c add FREE line
      nfree=nfree+1
      write(free(nfree), 51, err=90) os, o1, o2, o3
   51 format('changed INFO line from: (cs,c1,c2,c3) ',a1,3(1x,f12.3))
c read coordinates
      read(luco, *, err=97, end=96) c1,c2,c3
c write output trace
      call sff_WTraceFI(luout, wid2line, nsamp, fdata, idata,
     &  last, nfree, free, 
     &  cs, c1, c2, c3, nstack, ierr)
      if (ierr.ne.0) stop 'ERROR (chaco): writing output trace'
      chan=chan+1
      if (.not.last) goto 1

c----------------------------------------------------------------------
c close files
      print *,'closing coordinate file'
      close(luco, err=98)
      stop

c----------------------------------------------------------------------
c catch error conditions
   99 stop 'ERROR (chaco): reading source location from arguments'
   98 stop 'ERROR (chaco): closing coordinate file'
   97 stop 'ERROR (chaco): reading coordinate file'
   96 stop 'ERROR (chaco): reading coordinate file - unexpected end of file'
   93 stop 'ERROR (chaco): opening coordinate file'
   90 stop 'ERROR (chaco): writing FREE block'
      end
