c this is <sffu_fullargread.f>
c------------------------------------------------------------------------------
cS
c Copyright (c) 2001 by Thomas Forbriger (IfG Stuttgart)
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
c a routine for easy-to-use section reading
c 
c this routine takes filenames from the command line arguments
c
c REVISIONS and CHANGES
c    22/02/2001   V1.0   Thomas Forbriger
c    16/11/2001   V1.1   make code g77-proof
c
c==============================================================================
c
      subroutine sffu_fullargread(lu, maxtraces, maxsamples, maxfiles,
     &  data, idata, timeofsample, nfiles, ntraces, verbose, debug,
     &  nsamples, firstsample, fileindex, chain,
     &  revchain, firstinchain, firstinrevchain, filename, source, station,
     &  channel, auxid, instype, toffset, dt, roffset, maxval, average,
     &  minval, lastarg)
c
c lastarg:    where to start in command line argument array
c 
c this version does read all files given on the commandline after
c lastarg except the last element on the commandline
c
      integer lastarg, lu
c
      integer maxtraces, maxsamples, maxfiles
c
      integer nsamples(maxtraces), firstsample(maxtraces)
      integer fileindex(maxtraces)
      integer chain(maxtraces)
      integer revchain(maxtraces)
      integer firstinchain
      integer firstinrevchain
      character*80 filename(maxfiles)
      character*20 source(maxfiles)
      character*5 station(maxtraces)
      character*3 channel(maxtraces)
      character*4 auxid(maxtraces)
      character*6 instype(maxtraces)
      real toffset(maxtraces), dt(maxtraces)
      real roffset(maxtraces)
      real maxval(maxtraces), average(maxtraces), minval(maxtraces)
c
c as we do not know how many samples the longest trace will have, we
c must expect to need maxsamples of time values (tos)
      real data(maxsamples), timeofsample(maxsamples)
      integer nfiles, ntraces
c
      logical debug, verbose
c 
c use data space as input buffer
      integer idata(maxsamples)
cE
c
c some helpfull things
      integer i, iargc, ierr, j, allnsamples, k
      character*80 infile, selection
      logical useselect, hot
c
c trace selection
      integer maxselect
      parameter(maxselect=400)
      logical takethis(maxselect), takeit, skip
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
c
c
c----------------------------------------------------------------------
c
c ok go
      useselect=.false.
      nfiles=0
      ntraces=0
      allnsamples=0
      hot=.true.
      skip=.false.
c
      do i=(lastarg+1),(iargc()-1)
c still enough data space left?
        if (hot) then
          if (skip) then
            skip=.false.
          else
c
c get filename and selection
            useselect=.false.
            call getarg(i, infile)
            if (i.lt.iargc()) then
              call getarg(i+1, selection)
              if (selection(1:2).eq.'t:') then
                skip=.true.
                useselect=.true.
c
c eval selection
                call tf_listselect(maxselect, takethis, 3, selection, ierr)
                if (ierr.ne.0) print *,
     &            'WARNING (readdata): something wrong with your selections for ',
     &            infile(1:(index(infile,' ')-1)),'?'
              endif
            endif
          endif
c
c check filespace
          if (nfiles.lt.maxfiles) then
            nfiles=nfiles+1
            filename(nfiles)=infile
c
c go and open file
            if (verbose) then
              print *,' '
              print *,'open ',infile(1:index(infile,' '))
            endif
            call sff_ROpenS(lu, infile, sffversion, timestamp, code,
     &        source(nfiles), scs, sc1, sc2, sc3, date, time, ierr)
            if (ierr.ne.0) stop 'ERROR (readdata): opening data file'
c
c cycle through all traces
            j=0
            last=.false.
            do while (.not.last) 
              j=j+1
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
c
c work on trace
              if (takeit) then
                if (verbose) print *,'  * read trace no. ',j
                if (ntraces.lt.maxtraces) then
                  ntraces=ntraces+1
                  fileindex(ntraces)=nfiles
                  firstsample(ntraces)=allnsamples+1
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
                  roffset(ntraces)=sffu_offset(scs,sc1,sc2,sc3,rcs,rc1,rc2,rc3)
                  toffset(ntraces)=sffu_tfirst(wid2line, time, date)
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
                  close(lu)
                endif
              else
                if (verbose) print *,'    skip trace no. ',j
                call skipdata(lu, last)
              endif
            enddo
          else
            print *,'WARNING (readdata): exceeded file data space',
     &       ' - stopped reading'
            hot=.false.
          endif
        endif
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
c ----- END OF sffu_fullargread.f -----
