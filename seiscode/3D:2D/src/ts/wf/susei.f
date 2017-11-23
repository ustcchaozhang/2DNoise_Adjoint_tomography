c this is <susei.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c SUm of SEIsmograms
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
c uses code from coro.f
c
c REVISIONS and CHANGES
c    16/06/98   V1.0   Thomas Forbriger
c    27/02/99   V1.1   add more than two
c    22/10/01   V1.2   calculate arithmetic mean of traces
c    27/01/04   V1.3   switch for normalization
c    28/10/13   V1.4   use libfapidxx to support additional file formats
c
c==============================================================================
c
      program susei
c
      character*79 version
      parameter(version='SUSEI   V1.4   SUm of SEIsmograms')
c
c dimensions
      integer maxsamps, maxfree
      parameter(maxsamps=150000, maxfree=20)
c 
      integer i, nsumfiles,ifile
c input data
      real xdata(maxsamps), ydata(maxsamps)
      integer ixdata(maxsamps), iydata(maxsamps)
      equivalence(xdata,ixdata)
      equivalence(ydata,iydata)
      character*80 inxname, inyname, inputformat
      integer lui
      parameter(lui=11)
c output data
      real data(maxsamps)
      integer idata(maxsamps)
      equivalence(data,idata)
      character*80 outname, outputformat
      character*132 wid2line
      integer luo
      parameter(luo=10)
c sff extras
      character*80 free(maxfree)
      integer nfree
      real libversion
      character timestamp*13, code*10, type*20, cs*1, date*6, time*10
      character code2*10, cs2*1
      real c1,c2,c3,c12,c22,c32
      integer ierr,nsamp,nsamp2,nstack,nstack2
      logical last,last2
      real tanf,dt
      character*132 wid2line2
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=5)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical nonormalize
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/'-d','-nn','-ty','-it','-ot'/
      data opthasarg/2*.FALSE.,3*.TRUE./
      data optarg/2*'-','sff','sff','sff'/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: susei [-nn] [-ty f] [-it f] [-ot f]'
      print *,'             file1 file2 ... outfile'
      print *,'   or: susei -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'SUm of SEIsmograms'
        print *,' '
        print *,'file1  first input data file'
        print *,'file2  second input data file to be stacked to file1'
        print *,'...    further input data files to be stacked'
        print *,'outfile output data file'
        print *,' '
        print *,'-nn    do not normalize'
        print *,'       The program is inteded to be used for stacking.'
        print *,'       In that case the amplitude should be normalized'
        print *,'       by the number of traces stacked. This option'
        print *,'       turns off amplitude normalization.'
        print *,'-ty f  choose file format ''f'' instead of SFF'
        print *,'       for input and output files'
        print *,'-it f  choose input file format ''f'' instead of SFF'
        print *,'-ot f  choose output file format ''f'' instead of SFF'
        print *,' '
        print *,'The data files are expected to have the same number of'
        print *,'traces each. The number of traces is determined'
        print *,'from the file with the least number of traces.'
        print *,'All traces of equal index within each file are'
        print *,'expected to have the same number of samples. The'
        print *,'programs aborts, if this is not the case. No further'
        print *,'consistency checks are applied.'
        print *,' '
        call sff_help_formats
        stop
      endif
      if (iargc().lt.3) stop 'ERROR: too few arguments'
c
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      nonormalize=optset(2)
      inputformat=optarg(3)
      outputformat=optarg(3)
      if (optset(4)) inputformat=optarg(4)
      if (optset(5)) outputformat=optarg(5)
c
      nsumfiles=iargc()-lastarg-2
c 
      call getarg(lastarg+1, inxname)
      call getarg(lastarg+nsumfiles+2, outname)
c 
c 
      call sff_select_input_format(inputformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selected input format is not supported'
      call sff_select_output_format(outputformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selected output format is not supported'
c
c------------------------------------------------------------------------------
c 
c 
      free(1)=version
      free(2)='first data from  '//inxname(1:index(inxname, ' '))
      free(3)='second data from '//inyname(1:index(inyname, ' '))
      free(4)='all information is taken from first data file'
      free(5)='no plausibility checks were performed'
      nfree=5
c 
      print *,'open ',inxname(1:index(inxname,' ')),'for input'
      call sff_ROpenS(lui, inxname,
     &  libversion, timestamp, code,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening x-component'
      call sff_New(luo, outname, ierr)
      print *,'open ',outname(1:index(outname,' ')),'for output'
      call sff_WOpenFS(luo, outname,
     &  free, nfree,
     &  type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
      do ifile=1,nsumfiles
        call getarg(lastarg+ifile+1, inyname)
        print *,'open ',inyname(1:index(inyname,' ')),'for input'
        call sff_ROpenS(lui+ifile, inyname,
     &    libversion, timestamp, code,
     &    type, cs, c1, c2, c3, date, time, ierr)
        if (ierr.ne.0) stop 'ERROR: opening y-component'
      enddo
c      call sff_ROpenS(luz, inzname,
c     &  libversion, timestamp, code,
c     &  type, cs, c1, c2, c3, date, time, ierr)
c      if (ierr.ne.0) stop 'ERROR: opening z-component'
c 
      last=.false.
      do while (.not.(last))
        nsamp=maxsamps
        call sff_RTraceI(lui, tanf, dt, wid2line, nsamp, xdata, ixdata,
     &    code, last, cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading x-data'
        do i=1,nsamp
          data(i)=xdata(i)
        enddo
        do ifile=1,nsumfiles
          nsamp2=maxsamps
          call sff_RTraceI(lui+ifile, tanf, dt, wid2line2, 
     &      nsamp2, ydata, iydata,
     &      code2, last2, cs2, c12, c22, c32, nstack2, ierr)
          if (ierr.ne.0) then
            print *,'ERROR: reading y-data no. ',ifile
            stop 
          endif
          if (nsamp.ne.nsamp2) then
            print *,'ERROR: wrong number of samples in y-data no. ',ifile
            stop
          endif
          last=(last.or.last2)
          do i=1,nsamp
            data(i)=(data(i)+ydata(i))
          enddo
        enddo
        if (.not.nonormalize) then
          do i=1,nsamp
            data(i)=data(i)/float(nsumfiles+1)
          enddo
        endif
        wid2line(36:38)='NSP'
c 
        call sff_WTraceI(luo, wid2line, nsamp, data, idata, last,
     &    cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: writing trace'
      enddo
c
      stop
      end
c
c ----- END OF susei.f -----
