c this is <fidase_writedata.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
cS
c
c write complete prepared dataset to disk
c
c REVISIONS and CHANGES
c    09/07/98   V1.0   Thomas Forbriger
c    23/10/01   V1.1   write stack counter
c    06/12/02   V1.2   support inv1d from flgevask bundle (write single)
c
c==============================================================================
c
      subroutine writedata(outfile, overwrite, optwritesingle)
c 
c write full dataset to filename
c overwrite all dataset if true
c 
      include 'fidase_dim.inc'
      include 'fidase_data.inc'
      include 'fidase_para.inc'
      include 'fidase_strings.inc'
c 
      character outfile*(*)
      logical overwrite, optwritesingle
c 
cE
      integer lu, ierr, i, j
      parameter(lu=11)
c 
      integer itrace, ifile
c 
c source
      character stype*20, sdate*6, stime*10
      character*90 singletracefile
c data
      integer idata(maxsamples)
      equivalence(data, idata)
c trace
      logical last
c FREE block
      integer maxfree, nfree
      parameter(maxfree=10)
      character*80 free(maxfree)
c WID2 info
      character*132 wid2line
      integer wid2date(7), refdate(7), startdate(7), day, month
      real second
c 
      if (overwrite) then
        if (verbose) print *,'removing file ',
     &    outfile(1:index(outfile, ' '))
        call sff_New(lu, outfile, ierr)
        if (ierr.ne.0) then
          print *,'WARNING (writedata): could not remove old file'
          return
        endif
      endif
c 
c prepare time and date values
c refdate: SRCE time and date
c startdate: time offset of trace with respect to SRCE
c wid2date: time and date of first sample
      call time_clear(refdate)
      refdate(1)=1998
      refdate(2)=1
c dummy reference time and SRCE information
      call sffu_srcetime(refdate, sdate, stime)
c      sdate='980101'
c      stime='000000.000'
      stype='unknown'
c create file FREE block
      free(1)='file was created by'
      free(2)=string_version
      nfree=2
c 
c open if in write-at-once mode
      if (.not.(optwritesingle)) then
c 
        if (overwrite) then
          if (verbose) print *,'removing file ',
     &      outfile(1:index(outfile, ' '))
          call sff_New(lu, outfile, ierr)
          if (ierr.ne.0) then
            print *,'WARNING (writedata): could not remove old file'
            return
          endif
        endif
c 
        if (verbose) print *,'writing to file ',
     &    outfile(1:index(outfile, ' '))
        call sff_WOpenFS(lu, outfile, free, nfree, stype, 'C',
     &    0., 0., 0., sdate, stime, ierr)
        if (ierr.ne.0) stop 'ERROR (writedata): could not open file'
      endif
c 
      i=firstinchain
      if (debug) print *,'DEBUG: number of traces to be written: ',
     &  ntraces
      do itrace=1,ntraces
        if (verbose) print 50,itrace,ntraces,i,roffset(i)
        last=.false.
        if (itrace.eq.ntraces) last=.true.
c   
c   open if in write-single-traces mode
        if (optwritesingle) then
c   
          singletracefile=outfile
          j=index(singletracefile,' ')
          singletracefile(j:j)='.'
          singletracefile(j+1:j+5)=station(i)
          if (overwrite) then
            if (verbose) print *,'removing file ',
     &        singletracefile(1:index(singletracefile, ' '))
            call sff_New(lu, singletracefile, ierr)
            if (ierr.ne.0) then
              print *,'WARNING (writedata): could not remove old file'
              return
            endif
          endif
c   
          if (verbose) print *,'writing to file ',
     &      singletracefile(1:index(singletracefile, ' '))
          call sff_WOpenFS(lu, singletracefile, free, nfree, stype, 'C',
     &      0., 0., 0., sdate, stime, ierr)
          if (ierr.ne.0) stop 'ERROR (writedata): could not open file'
c 
          last=.true.
        endif
c
        ifile=fileindex(i)
c prepare time and date values
c refdate: SRCE time and date
c startdate: time offset of trace with respect to SRCE
c wid2date: time and date of first sample
        call time_clear(startdate)
        if (timeofsample(firstsample(i)).lt.0.) 
     &    stop 'ERROR: cannot handle negative delays'
        call sffu_dttotime(timeofsample(firstsample(i)), startdate)
        call time_add(refdate, startdate, wid2date)
        call time_getdate(day, month, wid2date)
        second=wid2date(5)+1.e-3*wid2date(6)
c prepare WID2 line
        call sff_PrepWid2(nsamples(i), 1./dt(i), station(i), 
     &    wid2date(1), month, day, wid2date(3), wid2date(4),
     &    channel(i), auxid(i), instype(i), second, -1., -1., -1., -1., 
     &    wid2line, ierr)
        if (ierr.ne.0) stop 'ERROR (writedata): preparing WID2 line'
c create FREE block
        free(1)='seismic source was '//source(ifile)
        free(2)='original datafile was'
        free(3)=filename(ifile)
        nfree=3
c 
        call sff_WTraceFI(lu, wid2line, nsamples(i), 
     &    data(firstsample(i)), idata(firstsample(i)), last,
     &    nfree, free,
     &    'C', roffset(i), 0., 0., nstack(i), ierr)
        if (ierr.ne.0) stop 'ERROR (writedata): writing trace'
        i=chain(i)
      enddo
      if (verbose) print *,'file written and closed'
c 
      return
   50 format(3x,'writing trace ',i3,'/',i3,': ',i3,' at ',f10.3,'m')
      end
c
c ----- END OF fidase_writedata.f -----
