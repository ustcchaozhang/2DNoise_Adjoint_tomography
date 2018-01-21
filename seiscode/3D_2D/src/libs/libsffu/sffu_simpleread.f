c this is <sffu_simpleread.f>
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
c
c==============================================================================
c
      subroutine sffu_simpleread(lu, filename, maxtraces, maxsamples, 
     &  data, idata, toffset, dt, roffset, nsamples, firstsample, ntraces,
     &  verbose)
c
c
      integer maxtraces, maxsamples
      character*(*) filename
      integer lu
c
      integer nsamples(maxtraces), firstsample(maxtraces)
      real toffset(maxtraces), dt(maxtraces)
      real roffset(maxtraces)
c
c as we do not know how many samples the longest trace will have, we
c must expect to need maxsamples of time values (tos)
      real data(maxsamples)
      integer ntraces
c 
c use data space as input buffer
      integer idata(maxsamples)
      logical verbose
cE
c
c some helpfull things
      integer ierr, j, allnsamples
c
c sff file header
      character code*10, timestamp*13, scs*1, date*6, time*10
      character source*30
      real sffversion, sc1, sc2, sc3
c
c sff trace
      logical last
      character rcs*1, wid2line*132
      real rc1, rc2, rc3, tanf
      integer nstack
c
c functions
      real sffu_offset, sffu_tfirst
c
c
c----------------------------------------------------------------------
c
c ok go
      ntraces=0
      allnsamples=0
c
c go and open file
      if (verbose) then
        print *,' '
        print *,'open ',filename(1:index(filename,' '))
      endif
      call sff_ROpenS(lu, filename, sffversion, timestamp, code,
     &  source, scs, sc1, sc2, sc3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR (readdata): opening data file'
c
c cycle through all traces
      j=0
      last=.false.
      do while (.not.last) 
        j=j+1
        if (verbose) print *,'  * read trace no. ',j
        if (ntraces.lt.maxtraces) then
          ntraces=ntraces+1
          firstsample(ntraces)=allnsamples+1
          nsamples(ntraces)=maxsamples-allnsamples
          call sff_RTraceI(lu, tanf, dt(ntraces),
     &      wid2line, nsamples(ntraces), data(firstsample(ntraces)),
     &      idata(firstsample(ntraces)), code, last,
     &      rcs, rc1, rc2, rc3, nstack, ierr)
          if (ierr.ne.0) stop 'ERROR (readdata): reading trace'
c
c extract info
          roffset(ntraces)=sffu_offset(scs,sc1,sc2,sc3,rcs,rc1,rc2,rc3)
          toffset(ntraces)=sffu_tfirst(wid2line, time, date)
c
c some final settings
          allnsamples=allnsamples+nsamples(ntraces)
        else
          print *,'WARNING (readdata): exceeded trace data space',
     &     ' - stopped reading'
          last=.true.
          close(lu)
        endif
      enddo
c 
      return
      end
c
c ----- END OF sffu_simpleread.f -----
