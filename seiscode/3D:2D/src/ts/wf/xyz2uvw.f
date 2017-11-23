c this is <xyz2uvw.f> by Thomas Forbriger 1997
c
c Copyright 1997, 2010 by Thomas Forbriger
c
c this program calculates the original components uvw from xyz for
c STS-2 seismometers
c 
c (sff is the Stuttgart File Format as defined in sff.doc)
c
c ----
c XYZ2UVW is free software; you can redistribute it and/or modify
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
c V1.0   09/01/97   first running version
c V1.1   18/10/00   pass maxsamples to sff library when reading
c V1.2   07/10/05   print more specific error message
c        14/12/11   checked transformation matrices and confirmed that
c                   they are correct (thof)
c  see http://gpitrsvn.gpi.uni-karlsruhe.de:8000/TFSoftware/ticket/146
c
c======================================================================
      program xyz2uvw
      character*79 version
c dimensions
      integer maxtraces, maxsamples, maxfree
      parameter (maxtraces=3,maxsamples=1000000,maxfree=4)
      integer lu
      parameter (lu=10)
c common definitions
      character*80 infile, outfile, switch
      integer i,j, iargc
      logical inx, debug
c sff specific
      character*20 code
      integer ierr
      character*13 timestamp
      character*80 free(maxfree)
      character*132 wid2line(maxtraces)
      integer idata(maxsamples)
      real fdata(maxsamples)
      equivalence (idata,fdata)
      integer firstsamp(maxtraces)
      real tanf(maxtraces), dt(maxtraces)
      integer nsamp(maxtraces), sample, trace
      logical last
c transform definitions
      real t(3,3), r(3)
      integer a(3),b(3)
c----------------------------------------------------------------------
c 
c go on with some basic information
c
      version='XYZ2UVW   V1.2   calculate STS-2 components'
      debug=.false.
      print *,version
      print *,'Usage: xyz2uvw -x|-u infile outfile'
      print *,'or:    xyz2uvw -help'
      if (iargc().lt.1) stop 'ERROR: check argumemnts\n'
c give control-file information
      call getarg(1, infile)
      if (infile(1:5).eq.'-help') then
        print *,' '
        print *,'STS-2 output components X, Y, Z are not identical to'
        print *,'the sensor components U, V, W. For some purposes it'
        print *,'is desireable to have U, V, W seismograms.'
        print *,' '
        print *,'-x     input file has components X,Y,Z'
        print *,'       trace  infile            outfile'
        print *,'       1      Z  =  Z           U'
        print *,'       2      N  =  Y           V'
        print *,'       3      E  =  X           W'
        print *,' '
        print *,'-u     input file has components U,V,W'
        print *,'       trace  infile            outfile'
        print *,'       1      U                 Z  =  Z'
        print *,'       2      V                 N  =  Y'
        print *,'       3      W                 E  =  X'
        print *,' '
        stop
      endif
      if (iargc().ne.3) stop 'ERROR: check argumemnts\n'
c----------------------------------------------------------------------
      call getarg(1, switch)
      call getarg(2, infile)
      call getarg(3, outfile)
      if (switch.eq.'-x') then
        inx=.true.
      elseif (switch.eq.'-u') then
        inx=.false.
      else
        stop 'ERROR: wrong option (use -x or -u)\n'
      endif
c----------------------------------------------------------------------
c read file
      call sff_ROpen(lu, infile, version, timestamp, code, ierr)
      if (ierr.ne.0) stop 'ERROR: opening input file'
      firstsamp(1)=1
      do trace=1,3
        nsamp(trace)=maxsamples
        call sff_RTrace(lu, tanf(trace), dt(trace), wid2line(trace), 
     &    nsamp(trace), 
     &    fdata(firstsamp(trace)), idata(firstsamp(trace)),
     &    code, last, ierr)
        if (ierr.ne.0) stop 'ERROR: reading trace\n'
        if (trace.lt.3) then
          firstsamp(trace+1)=firstsamp(trace)+nsamp(trace)
          if ((firstsamp(trace+1)+nsamp(trace)).gt.maxsamples)
     &      stop 'ERROR: too many samples\n'
          if (last) stop 'ERROR: less than 3 traces in file\n'
        endif
        if (nsamp(trace).ne.nsamp(1))
     &    stop 'ERROR: inconsistent number of samples\n'
        if (dt(trace).ne.dt(1))
     &    stop 'ERROR: inconsistent sampling intervals\n'
        if (tanf(trace).ne.tanf(1))
     &    stop 'ERROR: inconsistent time of first sample\n'
      enddo
      if (.not.(last)) close (lu)
c----------------------------------------------------------------------
c prepare transform matrix
      if (inx) then
        t(1,1)=-sqrt(2./3.)
        t(1,2)=0.
        t(1,3)=sqrt(1./3.)
        t(2,1)=sqrt(1./6.)
        t(2,2)=sqrt(1./2.)
        t(2,3)=sqrt(1./3.)
        t(3,1)=sqrt(1./6.)
        t(3,2)=-sqrt(1./2.)
        t(3,3)=sqrt(1./3.)
        a(1)=firstsamp(3)-1
        a(2)=firstsamp(2)-1
        a(3)=firstsamp(1)-1
        b(1)=firstsamp(1)-1
        b(2)=firstsamp(2)-1
        b(3)=firstsamp(3)-1
      else
        t(1,1)=-sqrt(2./3.)
        t(1,2)=sqrt(1./6.)
        t(1,3)=sqrt(1./6.)
        t(2,1)=0.
        t(2,2)=sqrt(1./2.)
        t(2,3)=-sqrt(1./2.)
        t(3,1)=sqrt(1./3.)
        t(3,2)=sqrt(1./3.)
        t(3,3)=sqrt(1./3.)
        a(1)=firstsamp(1)-1
        a(2)=firstsamp(2)-1
        a(3)=firstsamp(3)-1
        b(1)=firstsamp(3)-1
        b(2)=firstsamp(2)-1
        b(3)=firstsamp(1)-1
      endif
      if (debug) then
        do i=1,3
          write(6, '(i6,3(2x,f10.6),i6)') b(i),(t(i,j), j=1,3),a(i)
        enddo
      endif
c----------------------------------------------------------------------
c do transformation
      do sample=1,nsamp(1)
        do i=1,3
          r(i)=0.
          do j=1,3
            r(i)=r(i)+(fdata(a(j)+sample)*t(i,j))
          enddo
        enddo
        do i=1,3
          fdata(b(i)+sample)=r(i)
        enddo
      enddo
c----------------------------------------------------------------------
c create prolog
      write(free(1), '(a)') version
      write(free(2), '(aa)') 'input file: ',infile(1:index(infile,' ')-1)
      write(free(3), '(aa)') 'output file: ',outfile(1:index(outfile,' ')-1)
      if (inx) then
        write(free(4), '(a)') 'conversion is done from x,y,z to u,v,w'
      else
        write(free(4), '(a)') 'conversion is done from u,v,w to x,y,z'
      endif
c----------------------------------------------------------------------
c modify wid2 line
      if (inx) then
        call modchan(wid2line(1), 'U')
        call modchan(wid2line(2), 'V')
        call modchan(wid2line(3), 'W')
      else
        call modchan(wid2line(1), 'Z/Z')
        call modchan(wid2line(2), 'N/Y')
        call modchan(wid2line(3), 'E/X')
      endif
c----------------------------------------------------------------------
c write file
      call sff_WOpenF(lu, outfile, free, maxfree, ierr)
      if (ierr.ne.0) stop 'ERROR: opening output file'
      last=.false.
      do trace=1,3
        if (trace.eq.3) last=.true.
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
