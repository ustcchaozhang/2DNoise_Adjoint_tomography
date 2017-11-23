c this is <merse.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c MERge SEismograms in SFF files
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
c    09/01/98   V1.0   Thomas Forbriger
c    13/01/98   V1.1   allow trace selection
c    13/05/00   V1.2   do not use idate anymore (it's not Y2K proof)
c    13/13/07   V1.3   small change to satisfy g77
c
c==============================================================================
c
      program merse
c
      character*79 version
      parameter(version='MERSE   V1.3   MERge SEismograms in SFF files')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      real time_libversion, sff_libversion, sffu_libversion
c 
      integer firstsource, lastsource, ifile
      character*80 source, target
      integer lus, lut
      parameter(lus=10, lut=11)
c 
      integer maxsel, firstchar, itrace
      character*80 selstring
      parameter(maxsel=400)
      logical expectmore, nouseselect, selection(maxsel)
c     
      integer ierr
c free block
      integer mfree, nfree
      parameter(mfree=5)
      character free(mfree)*80
c file header
      character stype*20, sdate*6, stime*10, scs*1, code*10
      real sc1, sc2, sc3, libversion
      integer today(3), htime(7), srctime(7)
c trace header
      character rcs*1, wid2line*132
      real rc1, rc2, rc3, nc1, nc2, nc3
      integer tracetime(7), difftime(7), newtime(7), nstack
c 
      character timestring*40, oneline*80
      logical slast, tlast, alast
      integer i
c 
      character cnsp*4, cartesian*1
      parameter(cnsp='NSP', cartesian='C')
c 
      logical overwrite
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-d,2h-o/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: merse [-o] source1 [source2 ...] target'
      print *,'   or: merse -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'MERge SEismograms in SFF files'
        print *,' '
        print *,'All time series contained in the input files (source1,...'
        print *,'sourceN) will be read and written to file target. The file'
        print *,'header FREE blocks will be lost. All receiver and source'
        print *,'coordinates must be given in cartesian coordinates and'
        print *,'will be relative to 0,0,0 in the target file. All times'
        print *,'will be relative to zero source time.'
        print *,' '
        print *,'-o           overwrite existing target file'
        print *,' '
        print *,'The usual ''t:n'' construct may be used to select traces.'
        print *,' '
        print *,'linked libraries:'
        print *,'libtime      version: ',time_libversion()
        print *,'libstuff     version: ',sff_libversion()
        print *,'libsffu      version: ',sffu_libversion()
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      overwrite=optset(2)
      firstsource=lastarg+1
      lastsource=iargc()-1
      call getarg(iargc(), target)
      if (firstsource.gt.lastsource) stop 'ERROR: missing filename(s)'
c
c------------------------------------------------------------------------------
c go
c
c prepare file header information
      stype=cnsp
      scs=cartesian
      sc1=0
      sc2=0
      sc3=0
c 
      call idate(today)
      today(1)=99
      today(2)=01
      today(3)=01
      write(sdate, '(3(i2.2))') today(3), today(2), today(1)
      stime='000000.000'
      call time_clear(htime)
      htime(1)=today(3)
      call time_setdoy(today(1), today(2), htime)
      call time_sprint(htime, timestring)
c 
      free(1)=version
      free(2)='header free blocks of source files are lost'
      free(3)='all times will be made relative to '//timestring
      nfree=3
c 
      print *,'NOTICE: ',free(2)
      print *,'NOTICE: ',free(3)
c open target
      if (overwrite) then
        print *,'clear file ',target(1:index(target, ' '))
        call sff_New(lut, target, ierr)
        if (ierr.ne.0) stop 'ERROR: clearing file'
      endif
c     
      print *,'create file ',target(1:index(target, ' '))
      call sff_WOpenFS(lut, target, free, nfree, stype, scs,
     &  sc1, sc2, sc3, sdate, stime, ierr)
      if (ierr.ne.0) stop 'ERROR: creating target file'
c 
      ifile=firstsource
      do while (ifile.le.lastsource)
c 
        call getarg(ifile, source)
c get selection
        do i=1,maxsel
          selection(i)=.true.
        enddo
c 
        call getarg(ifile+1, selstring)
        if (selstring(1:2).eq.'t:') then
          ifile=ifile+1
          print *,'NOTICE: found trace selection ',
     &      selstring(1:index(selstring, ' '))
          firstchar=3
          call tf_listselect(maxsel, selection, 
     &      firstchar, selstring, ierr)
          if (ierr.eq.0) then
            nouseselect=.false.
          elseif (ierr.eq.1) then
            print *,'ERROR: selection index out of range'
            print *,'       all traces will be selected'
            nouseselect=.true.
          elseif (ierr.eq.2) then
            print *,'ERROR: empty selection string'
            print *,'       all traces will be selected'
            nouseselect=.true.
          else
            print *,'ERROR: unknown trace selection error'
            print *,'       all traces will be selected'
            nouseselect=.true.
          endif
        else
          nouseselect=.true.
        endif
c
        print *,'open source file ',source(1:index(source, ' '))
        tlast=.false.
        if (ifile.eq.lastsource) tlast=.true.
c 
        call sff_ROpenS(lus, source, libversion, timestring, code,
     &    stype, scs, sc1, sc2, sc3, sdate, stime, ierr)
        if (ierr.ne.0) stop 'ERROR: opening source file'
        if (index(code,'S').eq.0) stop 'ERROR: missing SRCE line'
        if (index(code,'F').ne.0) print *,'NOTICE: ignoring file FREE block'
        if (scs.ne.cartesian) stop 'ERROR: missing cartesian coordinate system'
        call sffu_timesrce(sdate, stime, srctime)
        call time_sprint(srctime, timestring)
        print 50,'src',timestring,sc1,sc2,sc3
c 
        slast=.false.
        itrace=0
        do while (.not.(slast))
          itrace=itrace+1
          if ((nouseselect).or.(selection(itrace))) then
            expectmore=.false.
          else
            print *,'ignoring trace ',itrace
          endif
c do we expect another trace
          alast=.false.
          if ((tlast).and.(.not.(nouseselect))) then
            do i=itrace+1,maxsel
              if (selection(i)) expectmore=.true.
            enddo
            if (.not.(expectmore)) then
              alast=.true.
              slast=.true.
              print *,'NOTICE: will ignore rest following that trace'
            endif
          endif
c 
c process DAST line
          read(lus, '(a44)', err=99, end=98) oneline
          if (oneline(1:5).ne.'DAST ') stop 'ERROR: missing DAST identifier'
          code=oneline(35:44)
          if (index(code, 'I').eq.0) stop 'ERROR: missing INFO line'
c is this really the last trace for the target file
          if (index(code, 'D').eq.0) then
            slast=.true.
            if ((slast).and.(tlast)) then 
              alast=.true.
              if ((expectmore).and.
     &          (.not.((nouseselect).or.(selection(itrace))))) then
                print *,'OARGG... we got trapped...'
                print *,'         Following your trace selection I did expect'
                print *,'         another trace following - but there is none.'
                print *,'         Your target file will be corrupt. Sorry,'
                print *,'         there is no way to prevent this now.'
              endif
            endif
            if (.not.(alast)) then
              i=index(oneline(35:44),' ')+34
              oneline(i:i)='D'
            endif
          else
            if (alast) then
              i=index(oneline(35:44),'D')+34
              oneline(i:i)=' '
            endif
          endif
          if ((nouseselect).or.(selection(itrace)))
     &      write(lut, '(a44)', err=97) oneline
c 
c process WID2 line
          read(lus, '(a132)', err=99, end=98) wid2line
          if (wid2line(1:5).ne.'WID2 ') stop 'ERROR: missing WID2 identifier'
          call sffu_timewid2(wid2line, tracetime)
          call time_sub(tracetime, srctime, difftime)
          call time_add(difftime, htime, newtime)
          call sffu_setwid2time(wid2line, newtime)
          if ((nouseselect).or.(selection(itrace)))
     &      write(lut, '(a132)', err=97) wid2line
c 
c process DAT2 block
          do while (oneline(1:5).ne.'CHK2 ')
            read(lus, '(a80)', err=99, end=98) oneline
            if ((nouseselect).or.(selection(itrace)))
     &        write(lut, '(a80)', err=97) oneline
          enddo
c 
c process FREE and INFO coda
          do i=1,len(code)
c INFO line
            if (code(i:i).eq.'I') then
              call sff_RInfo(lus, rcs, rc1, rc2, rc3, nstack, ierr)
              if (ierr.ne.0) stop 'ERROR: reading INFO line'
              if (rcs.ne.cartesian) 
     &          stop 'ERROR: no cartesian coordinate system for receiver'
              call time_sprint(tracetime, timestring)
              if ((nouseselect).or.(selection(itrace)))
     &          print 50,'rvc',timestring,rc1,rc2,rc3
              nc1=rc1-sc1
              nc2=rc2-sc2
              nc3=rc3-sc3
              if ((nouseselect).or.(selection(itrace)))
     &          call sff_WInfo(lut, rcs, nc1, nc2, nc3, nstack)
c FREE block
            elseif (code(i:i).eq.'F') then
              read(lus, '(a5)', err=99, end=98) oneline
              if (oneline(1:5).ne.'FREE ') stop 'ERROR: missing FREE block'
              if ((nouseselect).or.(selection(itrace)))
     &          write(lut, '(a5)', err=97) oneline
              read(lus, '(a80)', err=99, end=98) oneline
              do while (oneline(1:5).ne.'FREE ')
                if ((nouseselect).or.(selection(itrace)))
     &            write(lut, '(a80)', err=97) oneline
                read(lus, '(a80)', err=99, end=98) oneline
              enddo
              if ((nouseselect).or.(selection(itrace)))
     &          write(lut, '(aa)', err=97) 'trace taken from ',
     &          source(1:index(source, ' '))
              call time_sprint(tracetime, timestring)
              if ((nouseselect).or.(selection(itrace)))
     &          write(lut, '(aa)', err=97) 'original trace start: ',
     &          timestring
              if ((nouseselect).or.(selection(itrace)))
     &          write(lut, '(a,3(a3,f15.6))', err=97) 'original coordinates: ',
     &          ' x=',rc1,' y=',rc2,' z=',rc3
              if ((nouseselect).or.(selection(itrace)))
     &          write(lut, '(a80)', err=97) oneline
            endif
          enddo
c 
        enddo
        close(lus, err=96)
        print *,'source file read and closed'
c 
      enddo
c 
      close(lut, err=95)
      print *,'target file written and closed'
c 
      stop
   50 format(1x,a3,': ',a35,' x=',g9.3,' y=',g9.3,' z=',g9.3)
   99 stop 'ERROR: reading source file'
   98 stop 'ERROR: reading source file - unexpected end'
   97 stop 'ERROR: writing target file'
   96 stop 'ERROR: closing source file'
   95 stop 'ERROR: closing target file'
      end
c
c ----- END OF merse.f -----
