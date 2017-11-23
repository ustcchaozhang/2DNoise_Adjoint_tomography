c this is <refract_readdata.f>
c------------------------------------------------------------------------------
c
c 17/03/98 by Thomas Forbriger (IfG Stuttgart)
c
c read a complete set of data files
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    17/03/98   V1.0   Thomas Forbriger
c    24/05/00   V1.1   - allow more file-specific flags like n: or s:
c                      - introduced offset shift option
c    12/01/2001 V1.2   introduced plflag_hypoffset
c    19/06/2003 V1.3   count traces in file
c    18/11/2010 V1.4   has to call sff_close to be compatible with
c                      libfapidxx
c    26/11/2010 V1.5   support additional formats
c    23.12.2010 V1.6   specifically select input format
c    14/01/2011 V1.7   sff_close is not required and will trigger an
c                      error, since all traces are read up to the last
c                      trace and sff_skiptrace as well as sff_rtracei
c                      will close the file upon reading the last trace
c    14/11/2011 V1.8   store minimum and maximum offset
c    12/11/2012 V1.9   store offset shift
c    20/11/2012 V1.10  read variable offset flag
c                      store field offset, not offset shift
c                      read 'plot baseline' flag
c    24/10/2013 V1.11  added alternative definitions of ordinate scale
c
c==============================================================================
c
      subroutine readdata(lastarg)
c
c lastarg:    where to start in command line argument array
c
      integer lastarg
c
cE
c get common blocks
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_opt.inc'
c
c use data space as input buffer
      integer idata(maxsamples)
      equivalence(idata, data)
c
c some helpfull things
      integer i, iargc, ierr, lu, j, allnsamples, k
      integer tfstr_trimlen
      parameter(lu=10)
      character*120 infile
      character*120 fileformat
      character*240 selection
      logical useselect, hot, moreflags
c
c trace selection
      integer maxselect
      parameter(maxselect=1000)
      logical takethis(maxselect), takeit
c
c sff file header
      character code*10, timestamp*13, scs*1, date*6, time*10
      real sffversion, sc1, sc2, sc3
c
c sff trace
      logical last
      character rcs*1, wid2line*132
      real rc1, rc2, rc3, tanf
      integer nstack
c
c functions
      real sffu_offset, sffu_tfirst, ts_max, ts_min, ts_average
      real offshift
c 
c hls color values
      real colhls(3)
c
c counting traces
      integer tifcount
c
c
c----------------------------------------------------------------------
c
      if (debug) then
        print *,'DEBUG (readdata): lastarg=',lastarg
        print *,'DEBUG (readdata): iargc=',iargc()
        print *,'DEBUG (readdata): expecting to read ',
     &    iargc()-lastarg,' files'
      endif
c ok go
      nfiles=0
      tifcount=0
      ntraces=0
      allnsamples=0
      hot=.true.
c
      i=lastarg+1
c      do i=(lastarg+1),iargc()
      do while (i.le.iargc())
c still enough data space left?
        if (debug) print *,'DEBUG (readdata): arg#:',i
        if (hot) then
c
c get filename and selection
          useselect=.false.
          call getarg(i, infile)
          if (debug) print *,'DEBUG (readdata): operate on file ',
     &      infile(1:index(infile,' '))
c
c check filespace
          if (nfiles.lt.maxfiles) then
            nfiles=nfiles+1
            tifcount=0
            filename(nfiles)=infile
c set defaults
            usevarplot(nfiles)=2
            plotbaseline(nfiles)=.false.
c set default style values
            pg_file_ls(nfiles)=-1
            pg_file_lw(nfiles)=-1
            pg_file_ci(nfiles)=-1
            pg_file_rgb(1,nfiles)=-1.
            pg_file_rgb(2,nfiles)=-1.
            pg_file_rgb(3,nfiles)=-1.
c no offset shift
            offshift=0.
c read extra flags values
            moreflags=.true.
            fileformat=opt_Fformat
            do while ((i.lt.iargc()).and.(moreflags)) 
              call getarg(i+1, selection)
              if (debug) then
                print *,'DEBUG (readdata): ',
     &            'test additional flag at argument ',i+1,
     &            ': ',selection
              endif
              if (selection(1:2).eq.'t:') then
                i=i+1
                useselect=.true.
c
c eval selection
                call tf_listselect(maxselect, takethis, 3, selection, ierr)
                if (ierr.ne.0) print *,
     &            'WARNING (readdata): something wrong with your ',
     &            'selections for ',
     &            infile(1:(index(infile,' ')-1)),'?'
                if (debug) then
                  print *,'DEBUG: use selection list'
                  print *,'DEBUG: list: ',selection
c                  do j=1,maxselect
c                    print *,'DEBUG: select ',j,takethis(j)
c                  enddo
                endif
c set file format
              elseif (selection(1:2).eq.'f:') then
                i=i+1
                fileformat=selection(3:)
c set style
              elseif (selection(1:2).eq.'s:') then
                i=i+1
                read(selection(3:), *) pg_file_ci(nfiles),
     &            pg_file_ls(nfiles), pg_file_lw(nfiles)
c offset shift
              elseif (selection(1:2).eq.'o:') then
                i=i+1
                read(selection(3:), *) offshift
c set rgb color
              elseif (selection(1:2).eq.'r:') then
                i=i+1
                read(selection(3:), *) (pg_file_rgb(j,nfiles),j=1,3)
c set hls color
              elseif (selection(1:2).eq.'h:') then
                i=i+1
                read(selection(3:), *) (colhls(j),j=1,3)
                call grxrgb(colhls(1), colhls(2), colhls(3),
     &            pg_file_rgb(1,nfiles),
     &            pg_file_rgb(2,nfiles),
     &            pg_file_rgb(3,nfiles))
c set legend string
              elseif (selection(1:2).eq.'n:') then
                i=i+1
                if (opt_Tfilename) then
                filename(nfiles)=selection(3:tfstr_trimlen(selection))//
     &            ' ('//
     &            filename(nfiles)(1:tfstr_trimlen(filename(nfiles)))//
     &            ')'
                else
                  filename(nfiles)=selection(3:)
                endif
c read variable area plot flag
              elseif (selection(1:2).eq.'v:') then
                i=i+1
                read(selection(3:), *) usevarplot(nfiles)
c read baseline plot flag
              elseif (selection(1:2).eq.'b:') then
                i=i+1
                read(selection(3:), *) plotbaseline(nfiles)
              else
                moreflags=.false.
                if (debug) print *,'DEBUG (readdata): is not a flag'
              endif
            enddo
c
c go and open file
            print *,' '
            print *,'open ',infile(1:index(infile,' ')), 
     &        'with file format ',fileformat(1:index(fileformat,' '))
            call sff_select_input_format(fileformat, ierr)
            if (ierr.ne.0) stop 'ERROR (readdata): selecting file type'
            call sff_ROpenS(lu, infile, sffversion, timestamp, code,
     &        source(nfiles), scs, sc1, sc2, sc3, date, time, ierr)
            if (ierr.ne.0) stop 'ERROR (readdata): opening data file'
c
c cycle through all traces
            j=0
            last=.false.
            do while (.not.last) 
              j=j+1
              if (debug) print *,'DEBUG: start reading trace ',j
c
c find out whether we want this trace
              takeit=.true.
              if (useselect) then
                if (j.le.maxselect) then
                  takeit=takethis(j)
                else 
                  takeit=.false.
                endif
              endif
              if (debug) then
                print *,'DEBUG: trace ',j,' takeit: ',takeit,
     &            ' selection: ',takethis(j)
              endif
c
c work on trace
              if (takeit) then
                print *,'  read trace no. ',j
                if (ntraces.lt.maxtraces) then
                  ntraces=ntraces+1
                  tifcount=tifcount+1
                  fileindex(ntraces)=nfiles
                  firstsample(ntraces)=allnsamples+1
                  traceinfile(ntraces)=tifcount
                  nsamples(ntraces)=maxsamples-allnsamples
                  call sff_RTraceI(lu, tanf, dt(ntraces),
     &              wid2line, nsamples(ntraces), data(firstsample(ntraces)),
     &              idata(firstsample(ntraces)), code, last,
     &              rcs, rc1, rc2, rc3, nstack, ierr)
                  if (ierr.ne.0) stop 'ERROR (readdata): reading trace'
c
c extract info
                  station(ntraces)=wid2line(30:34)
                  channel(ntraces)=wid2line(36:38)
                  auxid(ntraces)=wid2line(40:43)
                  instype(ntraces)=wid2line(89:94)
                  if (plflag_hypoffset) then
                    roffset(ntraces)=
     &                  sffu_offset(scs,sc1,sc2,sc3,rcs,rc1,rc2,rc3)
                  else
                    roffset(ntraces)=
     &                  sffu_offset(scs,sc1,sc2,0.,rcs,rc1,rc2,0.)
                  endif
                  fieldoffset(ntraces)=roffset(ntraces)
                  if ((opt_Sordinate.ge.1)
     &                .and.(opt_Sordinate.le.3)
     &                .and.(rcs.ne.'C')) then
                    print *,'WARNING (readdata): '//
     &                'with non-Cartesian coordinates the ordinate '//
     &                'label might be inappropriate!'
                  endif
                  if (opt_Sordinate.eq.1) then
                    roffset(ntraces)=rc1
                  elseif (opt_Sordinate.eq.2) then
                    roffset(ntraces)=rc2
                  elseif (opt_Sordinate.eq.3) then
                    roffset(ntraces)=rc3
                  elseif (opt_Sordinate.ne.0) then
                    stop 'ERROR (readdata): '//
     &                'invalid definition of ordinate'
                  endif
                  roffset(ntraces)=roffset(ntraces)+offshift
                  if (ntraces.eq.1) then
                    minoffset=roffset(ntraces)
                    maxoffset=roffset(ntraces)
                  else
                    minoffset=min(roffset(ntraces),minoffset)
                    maxoffset=max(roffset(ntraces),maxoffset)
                  endif
                  toffset(ntraces)=sffu_tfirst(wid2line, time, date)
                  if (debug) print *,'DEBUG: roff ',roffset(ntraces), ' toff ',
     &              toffset(ntraces),' sc1,sc2,sc3,rc1,rc2,rc3 ',sc1,sc2,sc3,
     &              rc1,rc2,rc3
c 
c prepare time of sample
c                  print *,'range: ',nsamples(ntraces)+firstsample(ntraces)-1
                  do k=1,nsamples(ntraces)
                    timeofsample(k+firstsample(ntraces)-1)=
     &                toffset(ntraces)+(k-1)*dt(ntraces)
                  enddo
c
c some final settings
                  allnsamples=allnsamples+nsamples(ntraces)
                  maxval(ntraces)=ts_max(data(firstsample(ntraces)), 
     &              nsamples(ntraces))
                  minval(ntraces)=ts_min(data(firstsample(ntraces)), 
     &              nsamples(ntraces))
                  average(ntraces)=ts_average(data(firstsample(ntraces)), 
     &              nsamples(ntraces))
                  if (debug) print *,'DEBUG (readdata): ',
     &              'min,max,avg,first ',minval(ntraces),maxval(ntraces),
     &              average(ntraces),data(firstsample(ntraces))
                else
                  print *,'WARNING (readdata): exceeded trace data space',
     &             ' - stopped reading'
                  hot=.false.
                  last=.true.
c to me this appears to be a close which should not be here
                  call sff_close(lu, ierr)
                  if (ierr.ne.0) 
     &              stop 'ERROR (readdata): closing data file'
                endif
              else
                print *,'  skip trace no. ',j
                if (debug) print *,'DEBUG: processing trace ',j
                call skipdata(lu, last)
                if (debug) print *,'DEBUG: processing trace ',j
                tifcount=tifcount+1
              endif
              if (debug) print *,'DEBUG: end reading trace ',j
c enddo while (.not.last) 
            enddo
            if (ierr.ne.0) stop 'ERROR (readdata): closing data file'
c else (nfiles.lt.maxfiles) then
          else
            print *,'WARNING (readdata): exceeded file data space',
     &       ' - stopped reading'
            hot=.false.
c endif (nfiles.lt.maxfiles) then
          endif
c endif (hot)
        endif
        i=i+1
        if (debug) then
          print *,'DEBUG (readdata): finished this file'
          print *,'DEBUG (readdata): next argument index: ',i
          print *,'DEBUG (readdata): total number of arguments: ',iargc()
          if (hot) print *,'DEBUG (readdata): reader is hot'
        endif
c enddo while (i.le.iargc())
      enddo
c 
c build chain now
      call tf_rchain(roffset, chain, ntraces, firstinchain, 1)
      call tf_rchain(roffset, revchain, ntraces, firstinrevchain, -1)
c 
      if (debug) then
        do i=1,ntraces
          print *,'DEBUG: fileindex(',i,') ',fileindex(i)
        enddo
      endif
c 
      return
      end
c
c ----- END OF refract_readdata.f -----
