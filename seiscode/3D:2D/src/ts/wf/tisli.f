c this is <tisli.f>
c------------------------------------------------------------------------------
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c write TIme SLIces
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
c    28/01/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program tisli
c
      character*79 version
      parameter(version='TISLI   V1.0   write TIme SLIces')
c
      integer maxsamples,nsamp,i, maxtraces, j, ntraces
      parameter(maxsamples=4096,maxtraces=96)
c 
      real fdata(maxsamples, maxtraces)
      integer idata(maxsamples, maxtraces)
      equivalence(fdata,idata)
c 
      character*80 infile
      character*80 outfile
      character*80 outbase
      character*4 outnum
c 
      integer luin, luout
      parameter(luin=10, luout=11)
c 
      logical last
c 
      real tanf,dt,rdt,minval,maxval
      real readversion
      integer nstack,ierr,rnsamp
      real c1(maxtraces), c2(maxtraces), c3(maxtraces)
      character*1 cs
      character*14 timestamp
      character*10 code
      character*132 wid2line
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical logar
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-l/
      data opthasarg/3*.FALSE./
      data optarg/3*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: tisli infile outbase [-l] [-v]'
        print *,'   or: tisli -help'
        if (iargc().lt.1) stop 'ERROR: missing arguments'
        print *,' '
        print *,'write TIme SLIces'
        print *,' '
        print *,'infile       input file containing data from'
        print *,'             array receivers'
        print *,'outbase      basename for output files'
        print *,' '
        print *,'-v           be verbose'
        print *,'-l           output logarithmic values'
        print *,' '
        print *,'This program reads time series for receivers'
        print *,'at different spatial locations (expected to'
        print *,'cover an area in the manner of an array).'
        print *,'For each time sample a separate ASCII output file' 
        print *,'is created (with basename ''outbase'') which'
        print *,'contains instantaneous signal amplitude at the'
        print *,'given location together with horizontal coordinates'
        print *,'in an ASCII table. These files are meant to be input'
        print *,'to further steps of processing (like graphical'
        print *,'display).'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      logar=optset(3)
c 
      call getarg(1, infile)
      call getarg(2, outbase)
c
c------------------------------------------------------------------------------
c go
c 
      call sff_ROpen(luin, infile, readversion, timestamp, code, ierr)
      if (ierr.ne.0) stop 'ERROR: opening input file'
c 
      last=.false.
      ntraces=0
      do while(.not.(last))
        ntraces=ntraces+1
        if (i.gt.maxtraces) stop 'ERROR: too many traces'
        nsamp=maxsamples
        call sff_RTraceI(luin, tanf, dt,
     &    wid2line, nsamp, fdata(1,ntraces), idata(1,ntraces), code, last,
     &    cs, c1(ntraces), c2(ntraces), c3(ntraces), nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading input file'
        if (cs.ne.'C') stop 'ERROR: wrong coordinate system'
        if (j.gt.1) then
          if (dt.ne.rdt) stop 'ERROR: inconsistent sampling rates'
          if (nsamp.ne.rnsamp) stop 'ERROR: inconsistent number of samples'
        else
          rdt=dt
          rnsamp=nsamp
        endif
      enddo
c 
      minval=fdata(1,1)
      maxval=fdata(1,1)
c 
      do i=1,nsamp
        write(outnum, '(i4.4)') i
        outfile=outbase(1:index(outbase, ' ')-1)//'.'//outnum
        open(luout, file=outfile, err=99)
        do j=1,ntraces
          if (logar) fdata(i,j)=log(abs(fdata(i,j)))
          write(luout, '(3(g15.8,2x))', err=97) c1(j), c2(j), fdata(i,j)
          minval=min(minval,fdata(i,j))
          maxval=max(maxval,fdata(i,j))
        enddo
        close(luout, err=98)
      enddo
c 
      print 50,minval,maxval,(minval+maxval)*0.5,
     &         (maxval-minval)*0.1,10,(minval+maxval)*.5
c
      stop
   50 format('minimum: ',g15.8,'   maximum: ',g15.8,'   mean: ',g15.8,/
     &       '"-C',e15.10,'" -S',i3.3,' "-M',e15.10,'"')
   99 stop 'ERROR: opening input file'
   98 stop 'ERROR: closing input file'
   97 stop 'ERROR: writing input file'
      end
c
c ----- END OF tisli.f -----
