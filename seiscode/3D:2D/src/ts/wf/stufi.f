c this is <stufi.f> by Thomas Forbriger 1996
c
c this is a special version of SEIFE by E. Wielandt
c for input and output fo sff format files 
c 
c Copyright 1996, 2010 by Thomas Forbriger
c
c ----
c STUFI is free software; you can redistribute it and/or modify
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
c (sff is the Stuttgart File Format as defined in sff.doc)
c
c REVISIONS and CHANGES
c V1.0   11/11/96   first running version
c V1.1   13/12/96   set correct file status on open action
c V1.2   14/12/96   - added overwrite option
c                   - and fixed some decleration bugs (not serious)
c                   - fixed trace number output decimals
c                   - fixed more traces estimation when not using selection    
c V1.3   21/01/97   correct array-dimension to sff_RData in libstuff V1.07
c V1.4   10/02/97   changed line splitting for seife commands
c V1.5   15/07/98   * give usage information only without arguments
c                   * introduced verbose mode
c V1.6   21/02/99   changed tflib calls to tf_
c V1.7   09/02/00   allow longer path names
c V1.8   15/04/05   catch sample rates that do not fit on the output format
c V1.9   08/10/10   code relied on Fortran to initialize variables
c
c======================================================================
      program stufi
      character*77 version, creator
      parameter(version=
     &  'STUFI V1.9 E. Wielandts filter routines for sff files')
      parameter(creator='1996 by Thomas Forbriger (IfG Stuttgart)')
c dimensions
      integer maxcontrol, maxsamples, maxfree, maxselect
      parameter(maxcontrol=50, maxsamples=500000, maxfree=400)
      parameter(maxselect=200)
      character*200 junkfile
      parameter (junkfile='stufijunkforreplace')
c common definitions
      integer filep,trace,i,j
      character*200 line
      logical debug
c control file
      character*200 command(maxcontrol)
      integer ncommand
c sff specific
      character*20 code, outcode
      integer sffversion, ierr, nfilefree, ntracefree,flmax
      character*13 timestamp
      character*80 filefree(maxfree), tracefree(maxfree)
      character*200 infile
      character*132 wid2line
      character soutyp*20, soucs*1, soudate*6, soutime*10
      real souc1, souc2, souc3, tanf, ampfac
      double precision srat
      integer idata(maxsamples)
      logical moretraces, expectmoretraces
      integer hour, minute
      real second
      real tracec1, tracec2, tracec3
      character tracecs*1
      integer tracenstack
c using selections
      logical useselect
      logical selection(maxselect)
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=6)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
      character*20 extension
      character*80 outfile, controlfile
      logical setext, replace, newfile, overwrite, verbose
c seife-sepcific variables
      double precision x(maxsamples)
      character typ*3, par*80, msg*79
      integer nsamples
      real dt, tmin, tsec
c here are the keys to our commandline options
      data optid/2h-e,2h-r,2h-t,2h-d,2h-o,2h-v/
      data opthasarg/.TRUE.,.FALSE.,.TRUE.,3*.FALSE./
      data optarg/4h.sfi,1h-,4hjunk,3*1h-/
      
c----------------------------------------------------------------------
c 
c go on with some basic information
c
      controlfile=' '
      if (iargc().eq.1) call getarg(1, controlfile)
c give control-file information
      if ((controlfile(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,' ',creator
        print *,'Usage: stufi controlfile [-e ext | -r | -t file] [-o] [-v]'
        print *,'             file [t:list] ...'
        print *,'or:    stufi -help'
        if (iargc().lt.1) stop 'ERROR: missing arguments\n'
        print *,' '
        print *,'controlfile  Contains a sequence of seife commands'
        print *,'             that will be executed on every'
        print *,'             selected dataset. See below for'
        print *,'             available seife commands. A maximum'
        print *,'             of ',maxcontrol,' lines is allowed.'
        print *,'-e ext       Write results to a file with the same'
        print *,'             name as the original dataset but with'
        print *,'             filename extension ext. The resluting'
        print *,'             file will contain all original data'
        print *,'             traces (the ones not selected will be'
        print *,'             unchanged. the default extension is:'
        print *,'             .sfi'
        print *,'-r           Replace the original datasets with'
        print *,'             the results of the filter operation.'
        print *,'-t file      Write all results to one file with'
        print *,'             the given name. This file will contain'
        print *,'             only the selected traces. Any file'
        print *,'             related FREE blocks or source'
        print *,'             definitions will be omitted.'
        print *,'-o           Overwrite existing file (is default together'
        print *,'             with -r option.'
        print *,'-v           be verbose'
        print *,'-d           produce debug output'
        print *,' '
        print *,'Each datafile name may be followed by a list of'
        print *,'traces. This list selects a range of traces in'
        print *,'the file which will be processed. This list may'
        print *,'contain no blank (which is the separator to the'
        print *,'next filname). The traces will always be processed'
        print *,'in the order they appear in the data file.'
        print *,' '
        print *,'Examples:'
        print *,'  t:2           will select only trace 2'
        print *,'  t:4-6,2,4     will select traces 2, 4, 5 and 6'
        print *,'  t:9,8,10,14   will select traces 8, 9, 10 and 14'
        print *,' '
        print *,'The message returned by each seife command will be'
        print *,'appended to the FREE block of each trace.'
        print *,' '
c----------------------------------------------------------------------
        print *,'Some comments that come with the underlying library:'
        print *,''
        typ='hel'
        par=''
        nsamples=1
        dt=1.
        call seife(typ,par,nsamples,dt,tmin,tsec,x,msg)
        print *,' '
        print *,'This program is compiled for:'
        print *,'                maximum number of samples: ',
     &          maxsamples
        print *,'    maximum number of lines in FREE block: ',
     &          maxfree
        print *,'  maximum number of lines in control file: ',
     &          maxcontrol
        print *,'  maximum number of traces to be selected: ',
     &          maxselect
        stop
      endif
c----------------------------------------------------------------------
c
c extract information from command line
c
      extension='.sfi'
      outfile='junk'
      setext=.true.
      replace=.false.
      newfile=.false.
      overwrite=.false.
c first read the commandline
      call tf_cmdline(2, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      if (optset(1)) then
        setext=.true.
        replace=.false.
        newfile=.false.
        extension=optarg(1)
      endif
      if (optset(2)) then
        setext=.false.
        replace=.true.
        newfile=.false.
      endif
      if (optset(3)) then
        setext=.false.
        replace=.false.
        newfile=.true.
        outfile=optarg(3)
      endif
      debug=optset(4)
      overwrite=optset(5)
      verbose=optset(6)
      if (debug) print *,'DEBUG: debug messages are switched on'
      lastarg=lastarg+1
c get name of commandfile
      call getarg(1, controlfile)
c check for datafiles
      if (iargc().lt.lastarg) stop 'ERROR: missing data file\n'
c----------------------------------------------------------------------
c initialize counters
      ntracefree=0
      nfilefree=0
      ncommand=0
      tracenstack=0
c----------------------------------------------------------------------
c 
c read in control file
c
      if (debug) print *,'DEBUG: read control file'
      ncommand=0
      open(10, file=controlfile, err=99, status='old')
    1 continue
        read(10, '(a)', err=98, end=96) line
        ncommand=ncommand+1
        command(ncommand)=line
        if (line(1:3).ne.'end') goto 1
      ncommand=ncommand-1
      if (ncommand.lt.1) stop 'ERROR: missing commands\n'
      close(10, err=97)  
c----------------------------------------------------------------------
c
c open output file if necessary
c
      if (newfile) then
        if (verbose) print 80,'writing to file ',outfile
        if (overwrite) then
          call sff_New(10, outfile, ierr)
          if (ierr.ne.0) stop 'ERROR: deleting file'
        endif
        open(10, file=outfile, err=95, status='new')
        code='F'
        call sff_WStatus(10, code)
        write(filefree(1), '(a)') version
        write(filefree(2), '(a)') 
     &  'collecting all results in one file'
        write(filefree(3), '(a)') 
     &  'file related FREE blocks and source informations are ignored'
        nfilefree=3
        call sff_WFree(10, nfilefree, filefree)
      endif
c----------------------------------------------------------------------
c 
c go through all data files
c
      filep=lastarg
      if (debug) print *,'DEBUG: enter main loop'
    2 continue
c 
c get file names
c
        call getarg(filep, infile) 
        if (verbose) then
          print *,' '
          print *,' '
          print 80,'reading new file ',infile
        endif
        if (setext) then
          outfile=infile
          call tf_nameext(outfile, extension)
          if (verbose) print 80,'writing to file ',outfile
        elseif (replace) then
          outfile=junkfile
          if (verbose) print *,'replacing input file'
        endif
c 
c open files
c
        open(11, file=infile, err=92, status='old')
        if (.not.(newfile)) then
          if (overwrite) then
            call sff_New(10, outfile, ierr)
            if (ierr.ne.0) stop 'ERROR: deleting file'
          endif
          open(10, file=outfile, err=95, status='new')
        endif
c 
c do selections
c
        call getarg(filep+1, line)
        if (line(1:2).eq.'t:') then
          filep=filep+1
          useselect=.true.
          call tf_listselect(maxselect, selection, 3, line, ierr)
          if (ierr.eq.1) then
            print *,'WARNING: selection exceeds possible range',
     &              ' from 1 to',maxselect
            print *,'         selecting only up to no.',maxselect
          elseif (ierr.eq.2) then
            print *,'WARNING: missing selection list - selecting ',
     &              'all traces'
            useselect=.false.
          elseif (ierr.ne.0) then
            print *,'WARNING: unknown error code by tf_listselect'
            print *,'         selecting all traces'
            useselect=.false.
          endif
        else
          useselect=.false.
        endif
c
c go through file
c
c +++++++++++
c read file header
c
        call sff_RStatus(11,sffversion,timestamp,code,ierr)
        if (debug) print *,'DEBUG: read status'
        if (ierr.ne.0) stop 'ERROR: reading status of input file\n'
        if (.not.(newfile)) then
          call sff_WStatus(10, code)
          if (debug) print *,'DEBUG: wrote status'
        endif
        i=1
   10   if (code(i:i).ne.' ') then
          if (debug) print *,'DEBUG: code: ',code(i:i)
          if (code(i:i).eq.'F') then
            if (newfile) then
              if (verbose) 
     &          print *,'MESSAGE: skipping FREE block of input file'
              call sff_SkipFree(11, ierr)
              if (debug) print *,'DEBUG: skipped FREE block of file'
              if (ierr.ne.0) stop 'ERROR: skipping FREE block\n'
            else
              if (debug) print *,'DEBUG: read FREE block'
              call sff_RFree(11, nfilefree, filefree, 
     &                       flmax, maxfree, ierr)
              if (debug) print *,'DEBUG: read FREE block of file'
              if (ierr.ne.0) stop 'ERROR: reading FREE block\n'
              call sff_WFree(10, nfilefree, filefree)
              if (debug) print *,'DEBUG: wrote FREE block of file'
            endif
          elseif (code(i:i).eq.'S') then
            call sff_RSource(11, soutyp, soucs, souc1, souc2, souc3,
     &                       soudate, soutime, ierr)
            if (debug) print *,'DEBUG: read SOURCE line of file'
            if (ierr.ne.0) stop 'ERROR: reading SOURCE line\n'
            if (newfile) then
              if (verbose) print *,'MESSAGE skipping SOURCE line'
            else
              call sff_WSource(10, soutyp, soucs, souc1, souc2,
     &                         souc3, soudate, soutime)
              if (debug) print *,'DEBUG: wrote SOURCE line of file'
            endif
          endif
          i=i+1
          goto 10
        endif
c +++++++++++
c go through traces
c 
        trace=0
   20   trace=trace+1
          if (DEBUG) print *,'DEBUG: next trace:',trace
          moretraces=.false.
          if (verbose) then
            print *,' '
            print 81,'trace no.',trace,':'
          endif
          nsamples=maxsamples
          call sff_RData(11, wid2line, nsamples, tanf, dt,
     &                   idata, ampfac, code, ierr)
          if (nsamples.gt.maxsamples) 
     &      stop 'ERROR: too many samples\n'
          if (debug) print *,'DEBUG: read data'
          if (ierr.ne.0) stop 'ERROR: reading trace' 
          i=1
   21     if (code(i:i).ne.' ') then
            if (code(i:i).eq.'F') then
              call sff_RFree(11, ntracefree, tracefree, 
     &                       flmax, maxfree, ierr)
              if (debug) print *,'DEBUG: read trace FREE block'
              if (ierr.ne.0) stop 'ERROR: reading FREE block\n'
            elseif (code(i:i).eq.'I') then
              call sff_RInfo(11, tracecs, tracec1, tracec2, tracec3,
     &                       tracenstack, ierr)
              if (debug) print *,'DEBUG: read trace INFO line'
              if (ierr.ne.0) stop 'ERROR: reading INFO line\n'
            elseif (code(i:i).eq.'D') then
              moretraces=.true.
            endif
            i=i+1
            goto 21
          endif
c ++++++++++++
c do seife
c
          if (newfile.and.useselect.and.(.not.selection(trace))) then
            if (verbose) print *,'MESSAGE skipping trace'
          else
            if ((.not.useselect).or.selection(trace)) then
c ++++++++++++
c watch out for more traces
c (in case of extracting only selected traces we have to estimate
c  whether there will be one more trace ore not)
c
              expectmoretraces=.false.
              if (newfile) then
                if (debug) print *,'DEBUG: looking for more traces'
                outcode=' '
                if (useselect) then
                  do i=trace+1,maxselect
                    if (selection(i)) expectmoretraces=.true.
                  enddo
                else
                  expectmoretraces=moretraces
                endif
                if ((debug).and.(expectmoretraces))
     &            print *,'DEBUG: more traces selected'
                if (iargc().gt.filep) then
                  expectmoretraces=.true.
                  if (debug) print *,'DEBUG: more files selected'
                endif
                i=1
                j=1
   23           if (code(i:i).ne.' ') then
                  if (code(i:i).eq.'F') then
                    outcode(j:j)='F'
                    j=j+1
                  endif
                  if (code(i:i).eq.'I') then
                    outcode(j:j)='I'
                    j=j+1
                  endif
                  i=i+1
                  goto 23
                endif
                if (expectmoretraces) outcode(j:j)='D'
                if (debug) print *,
     &            'DEBUG: code for this trace was: >',code,'<'
                code=outcode
                if (debug) print *,
     &            'DEBUG: code for this trace is: >',code,'<'
              endif
c
c  BEGIN
c  SEIFE BLOCK
c
c
c prepare data for seife
              if (debug) print *,'DEBUG: entering seife'
              if (debug) print *,'DEBUG: convert int to double'
              call tf_inttodouble(nsamples, maxsamples, 
     &                          idata, x, ampfac)
              tmin=float(int(tanf/60.))
              tsec=tanf-(60.*tmin)
c call seife
              do i=1,ncommand
                line=command(i)
                typ=line(1:3)
                par=line(4:)
c                read(line(1:3), '(a3)') typ
c                read(line(6:), '(a75)') par
                if (debug) print *,'DEBUG: +',typ,'++',par,'+'
                call seife(typ, par, nsamples, dt, tmin, tsec,
     &                     x, msg)
                if (debug) print *,'DEBUG: returned from seife: ',msg
                if (ntracefree.lt.maxfree) then
                  if (debug) print *,'DEBUG: write to FREE block'
                  if (debug) print *,'DEBUG: ',ntracefree
                  if (debug) print *,'DEBUG: ',maxfree
                  ntracefree=ntracefree+1
                  write(tracefree(ntracefree), '(a)') msg
                else
                  print *,
     &             'WARNING: reached maximum length of FREE block'
                endif
                if (verbose) print *,msg
              enddo
c prepare seife data to be written in sff format
c to WID2: nsamples, dt, tmin, tsec
              hour=int(tmin/60.)
              if ((hour.gt.23).or.(hour.lt.0)) then
                print *,'WARNING: time of first sample out of range'
                print *,'WARNING: time of first sample set to zero'
                hour=0
                minute=0
                second=0.
              else
                minute=int(tmin)-hour*60
                if (minute.lt.0) then
                  print *,'WARNING: time of first sample out of range'
                  print *,'WARNING: time of first sample set to zero'
                  hour=0
                  minute=0
                  second=0.
                else
                  second=tsec
                endif
              endif
              write(wid2line(17:28), '(i2,a1,i2,a1,f6.3)')
     &          hour,':',minute,':',second
              srat=1./dt
              if ((srat.lt.1.e-4).or.(srat.gt.100.)) then
                write(wid2line(58:68), '(e11.6)') 1./dt
              else
                write(wid2line(58:68), '(f11.6)') 1./dt
              endif
              write(wid2line(49:56), '(i8)') nsamples
              call tf_doubletoint(nsamples, maxsamples, 
     &                          idata, x, ampfac)
              if (debug) print *,'DEBUG: exiting seife'
c
c
c  SEIFE BLOCK
c  END
c

            endif
c ++++++++++++
c write data
c
            if (debug) print *,'DEBUG: ',wid2line
            if (debug) print *,'DEBUG: nsamples ',nsamples
            if (debug) print *,'DEBUG: ampfac ',ampfac
            if (debug) print *,'DEBUG: code ',code
            call sff_WData(10, wid2line, nsamples, idata,
     &                     ampfac, code)
            if (debug) print *,'DEBUG: wrote data'
            i=1
   22       if (code(i:i).ne.' ') then
              if (code(i:i).eq.'F') then
                call sff_WFree(10, ntracefree, tracefree)
                if (debug) print *,'DEBUG: wrote FREE block of trace'
              elseif (code(i:i).eq.'I') then
                call sff_WInfo(10, tracecs, tracec1, tracec2, tracec3,
     &                         tracenstack)
                if (debug) print *,'DEBUG: wrote INFO line of trace'
              endif
              i=i+1
              goto 22
            endif
          endif
        if (moretraces) goto 20
c close files
c
        if (.not.(newfile)) then
          close(10, err=93)
        endif
        close(11, err=91)
c
c do replace
c
        if (replace) then
          call system('/bin/mv '//junkfile//' '//infile)
          if (debug) print *,'DEBUG: /bin/mv '//junkfile//' '//infile
        endif
        filep=filep+1
        if (filep.le.iargc()) goto 2
c----------------------------------------------------------------------
c
c close output file if necessary
c
      if (newfile) close(10, err=93)
      if (expectmoretraces) then
        print *,'WARNING: there were still some traces expected'
        print *,'WARNING: last trace of output file may be not'
        print *,'         marked correctly'
      endif
      stop
   80 format(a,a)
   81 format(a,i3,a)
   99 stop 'ERROR: opening control file\n'
   98 stop 'ERROR: reading control file\n'
   97 stop 'ERROR: closing control file\n'
   96 stop 'ERROR: unexpected end of control file\n'
   95 stop 'ERROR: opening output file\n'
   94 stop 'ERROR: writing output file\n'
   93 stop 'ERROR: closing output file\n'
   92 stop 'ERROR: opening input file\n'
   91 stop 'ERROR: closing input file\n'
      end
