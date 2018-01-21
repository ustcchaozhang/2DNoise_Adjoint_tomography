c this is <offli.f>
c------------------------------------------------------------------------------
c
c Copyright 2000, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c OFFset LIsting
c
c REVISIONS and CHANGES
c    30/05/2000   V1.0   Thomas Forbriger
c    20/09/2000   V1.1   introduced chaco option
c    23/09/2002   V1.2   introduced wolle infofile option
c    09/09/2004   V1.3   teleseismic mode
c    17/01/2011   V1.4   use libfapidxx interface for file reading
c
c==============================================================================
c
      program offli
c
      character*(*) version
      parameter(version='OFFLI   V1.3   OFFset LIsting')
c
      integer ierr,nsamp,msamp,nstack
      parameter(msamp=100000)
      real data(msamp)
      integer idata(msamp)
      equivalence (data,idata)
      real tanf,dt,sc1,sc2,sc3,rc1,rc2,rc3,sffversion
      character scs*1, rcs*1, wid2line*132,date*7,time*11,code*20
      character timestamp*13,source*20
      integer lu,i 
      parameter(lu=10)
      logical last
      double precision pi
      parameter(pi=3.141592653589)
c 
      real sffu_offset
c 
      integer itrace,mtrace,ntrace
      parameter(mtrace=600)
      real offset(mtrace)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument, infile, informat
      parameter(maxopt=10)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical refmetform,chacoform,infoform,teleseismic
c 
      integer datear(7), day, month, year
      real coord(mtrace,3)
      character*5 stationid(mtrace)
      logical setid
      character*80 argid, argsline
      integer argstyp
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v,2h-r,2h-c,2h-i,2h-I,2h-S,2h-s,2h-t,'-ty'/
      data opthasarg/5*.FALSE.,3*.TRUE.,.FALSE.,.true./
      data optarg/6*1h-,1h2,12h0. 0. 1. -1.,1h-,'sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:6).eq.'-xhelp')) then
        call sff_help_details
        stop
      else if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: offli filename [-r|-c|-i|-t] [-ty f]'
        print *,'             [-I ID] [-S typ] [-s line]'
        print *,'   or: offli -help'
        print *,'   or: offli -xhelp'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'OFFset LIsting'
        print *,' '
        print *,'filename     SFF data file to list offsets from'
        print *,' '
        print *,'-r           write file appropriate for refmet'
        print *,'-c           write file appropriate for chaco'
        print *,'-i           write wolle info-file format'
        print *,'-t           teleseismic mode'
        print *,'-ty f        read date file of type f'
        print *,' '
        print *,'-I ID        define event ID'
        print *,'-S typ       define source type'
        print *,'             typ=1: moment tensor'
        print *,'             typ=2: single force'
        print *,'             default is single force'
        print *,'-s line      source definition line'
        print *,'             moment-tensor or force components'
        print *,'             default is vertical force or explosion'
        print *,' '
        call sff_help_formats
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call getarg(1,infile)
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      refmetform=optset(3)
      chacoform=optset(4)
      infoform=optset(5)
      setid=optset(6)
      argid=infile
      if (setid) argid=optarg(6)
      read(optarg(7), *) argstyp
      argsline=optarg(8)
      teleseismic=optset(9)
      informat=optarg(10)
c
c------------------------------------------------------------------------------
c go
      if (verbose) print *,'open ',infile(1:index(infile,' '))
      call sff_select_input_format(informat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting input file format'
      call sff_ROpenS(lu, infile, sffversion, timestamp, code,
     &  source, scs, sc1, sc2, sc3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening data file'
      last=.false.
      ntrace=0
c
      do while (.not.(last))
        nsamp=msamp
        call sff_RTraceI(lu, tanf, dt,
     &    wid2line, nsamp, data, idata, code, last,
     &    rcs, rc1, rc2, rc3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR (readdata): reading trace'
        ntrace=ntrace+1
        if (ntrace.gt.mtrace) stop 'ERROR: too many traces'
        offset(ntrace)=sffu_offset(scs,sc1,sc2,sc3, rcs,rc1,rc2,rc3)
        call sffu_timewid2(wid2line, datear)
        coord(ntrace,1)=rc1
        coord(ntrace,2)=rc2
        coord(ntrace,3)=rc3
        call sff_GetStation(wid2line, stationid(ntrace))
      enddo
c 
      if (refmetform) then
        print 52,version,infile(1:index(infile,' ')-1),ntrace
        do itrace=1,ntrace
          print 53,offset(itrace)*1.e-3
        enddo
      elseif (chacoform) then
        do itrace=1,ntrace
          print 54,offset(itrace)
        enddo
      elseif (infoform) then
c print event ID
        print 55,argid(1:index(argid,' ')-1)
c print event quality - never used by wolle - is NIL
        print 55,'NIL'
c print source time
        call sffu_timesrce(date, time, datear)
        call time_getdate(day, month, datear)
        year=datear(1)
c   flgevas bundle can not handle years>2000 :-(
        if (year.gt.99) year=year-100*int(year/100)
        print 56,year,month, day, datear(3), datear(4),
     &    datear(5)+1.e-3*(datear(6)+1.e-3*datear(7)),0.
c print source location and type
        print 57, sc1, sc2, sc3, argstyp
c print source parameters
        print 55, argsline
c print number of stations
        print 58, ntrace
c print station
        do itrace=1,ntrace
          print 59, itrace, stationid(itrace), 
     &      (coord(itrace, i), i=1,3)
        enddo
      elseif (teleseismic) then
        print 50,ntrace,infile(1:index(infile,' ')-1)
        print 60,sc1,sc2,sc3/1.e3
        do itrace=1,ntrace
          print 61,itrace,stationid(itrace),offset(itrace)/1.e3,
     &      180.*offset(itrace)/(pi*6371.e3),
     &      (coord(itrace, i),i=1,3)
        enddo
      else
        print 50,ntrace,infile(1:index(infile,' ')-1)
        do itrace=1,ntrace
          print 51,itrace,offset(itrace)
        enddo
      endif
c
      stop
   50 format(i3,' offsets read from file ',a,':')
   51 format(i3,' ',f12.3,'m')
   52 format(a,/,a,/,'mode, v_red, Tl, Tr,   N',/
     &              ,'   1,    0., 0., 0.,',i4,//,11x,'r, phi')
   53 format(f12.6,', 0.')
   54 format(f12.6,2(',0.'))
   55 format(a)
   56 format(5(i4,1x),f7.2,1x,f2.0)
   57 format(3(f10.5,1x),i2)
   58 format(i4)
   59 format(i4.4,1x,a5,1x,3(f7.2,1x))
   60 format('source coordinates: lat=',f6.3,'N lon=',f7.3,
     &       'E z=',f7.2,'km')
   61 format(i3.3,1x,a5,1x,f8.2,'km',1x,f7.3,'° ',1x,
     &       f6.3,'N ',f7.3, 'E z=',f6.2,'m')
      end
c
c ----- END OF offli.f -----
