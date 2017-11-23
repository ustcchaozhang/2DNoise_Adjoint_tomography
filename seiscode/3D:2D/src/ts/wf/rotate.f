c this is <rotate.f> by Thomas Forbriger 1997
c
c Copyright 1997, 2010 by Thomas Forbriger
c
c rotate horizontal components
c 
c (sff is the Stuttgart File Format as defined in sff.doc)
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
c V1.0   25/03/97   first running version
c V1.1   21/01/03   some corrections
c
c======================================================================
      program rotate
      character*79 version
      parameter(version='ROTATE   V1.1   rotate horizontal components')
c dimensions
      integer maxtraces, maxsamples, maxfree
      parameter (maxtraces=3,maxsamples=1000000,maxfree=6)
      integer lu
      parameter (lu=10)
c common definitions
      character*80 infile, outfile, angle
      integer iargc
c sff specific
      character*20 code
      real fversion
      integer ierr
      character*13 timestamp
      character*80 free(maxfree)
      character*132 wid2line(maxtraces), wid2
      integer idata(maxsamples)
      real fdata(maxsamples)
      equivalence (idata,fdata)
      integer firstsamp(maxtraces)
      real tanf(maxtraces), dt(maxtraces)
      integer nsamp(maxtraces), sample, trace
      logical last
c else
      integer cNsamp, cEsamp
      real vcos, vsin, pi, radial, trans, rangle
c----------------------------------------------------------------------
c 
c go on with some basic information
c
      print *,version
      print *,'Usage: rotate infile outfile angle'
      print *,'or:    rotate -help'
c give control-file information
      call getarg(1, infile)
      if (infile(1:5).eq.'-help') then
        print *,' '
        print *,' Data will be read from infile an written to outfile'
        print *,' in sff data format. The file has to consist of three'
        print *,' traces which will be called Z, N and E component'
        print *,' (in that order). The horizontal components will be'
        print *,' rotated by the amount of angle. Angle is counted in'
        print *,' degrees from north to east. Hence the input north'
        print *,' component will be the output east component if you'
        print *,' set angle to 90.'
        print *,' '
        print *,'The second trace will be ''R'' on output and the'
        print *,'third will be ''T''.'
        stop
      endif
      if (iargc().ne.3) stop 'ERROR: check argumemnts'
c----------------------------------------------------------------------
      call getarg(1, infile)
      call getarg(2, outfile)
      call getarg(3, angle)
      read(angle, *) rangle
c----------------------------------------------------------------------
c read file
      print *,'open input data...'
      call sff_ROpen(lu, infile, fversion, timestamp, code, ierr)
      if (ierr.ne.0) stop 'ERROR: opening input file'
      print *,'read input data...'
      firstsamp(1)=1
      do trace=1,3
        nsamp(trace)=maxsamples-firstsamp(trace)
        call sff_RTrace(lu, tanf(trace), dt(trace), wid2line(trace), 
     &    nsamp(trace), 
     &    fdata(firstsamp(trace)), idata(firstsamp(trace)),
     &    code, last, ierr)
        wid2=wid2line(trace)
        print *,wid2(1:78)
        if (ierr.ne.0) stop 'ERROR: reading trace\n'
        if (trace.lt.3) then
          firstsamp(trace+1)=firstsamp(trace)+nsamp(trace)
          if ((firstsamp(trace+1)+nsamp(trace)).gt.maxsamples)
     &      stop 'ERROR: too many samples\n'
          if (last) stop 'ERROR: less than 3 traces in file\n'
        endif
        if ((nsamp(trace).ne.nsamp(1)).or.
     &      (dt(trace).ne.dt(1)).or.
     &      (tanf(trace).ne.tanf(1))) stop 'ERROR: inconsistent dataset\n'
      enddo
      if (.not.(last)) close (lu)
c----------------------------------------------------------------------
c do transformation
      print *,'rotate...'
      pi=4.*atan(1.)
      vcos=cos(pi*rangle/180.)
      vsin=sin(pi*rangle/180.)
      print 50, 'R', vcos, vsin
      print 50, 'T', -vsin, vcos
   50 format('    ',a,'=',f7.4,'*N + ',f7.4,'*E')
      do sample=1,nsamp(1)
        cNsamp=sample+firstsamp(2)-1
        cEsamp=sample+firstsamp(3)-1
        radial=fdata(cNsamp)*vcos+fdata(cEsamp)*vsin
        trans=fdata(cEsamp)*vcos-fdata(cNsamp)*vsin
        fdata(cNsamp)=radial
        fdata(cEsamp)=trans
      enddo
c----------------------------------------------------------------------
c create prolog
      print *,'prepare comments...'
      write(free(1), '(a)') version
      write(free(2), '(aa)') 'input file: ',infile(1:index(infile,' ')-1)
      write(free(3), '(aa)') 'output file: ',outfile(1:index(outfile,' ')-1)
      write(free(4), '(af10.3)') 'angle of rotation (degrees): ',rangle
c----------------------------------------------------------------------
c modify wid2 line
      call modchan(wid2line(1), 'Z')
      call modchan(wid2line(2), 'R')
      call modchan(wid2line(3), 'T')
c----------------------------------------------------------------------
c write file
      print *,'open output data...'
      call sff_WOpenF(lu, outfile, free, maxfree, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
      print *,'write output data...'
      last=.false.
      do trace=1,3
        if (trace.eq.3) last=.true.
        wid2=wid2line(trace)
        print *,wid2(1:78)
        call sff_WTrace(lu, wid2line(trace), 
     &    nsamp(trace), 
     &    fdata(firstsamp(trace)), idata(firstsamp(trace)),
     &    last, ierr)
        if (ierr.ne.0) stop 'ERROR: writing trace\n'
      enddo
      stop
      end
c======================================================================
c subroutine
c 
c    
      subroutine modchan(wid2line, chan)
      character*132 wid2line
      character*(*) chan
      write(wid2line(36:38), '(a3)') chan
      return
      end
