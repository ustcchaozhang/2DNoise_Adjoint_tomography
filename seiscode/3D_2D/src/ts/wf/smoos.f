c this is <smoos.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c SMOOth Seismograms by sectral extension
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
c    12/02/98   V1.0   Thomas Forbriger
c    26/06/03   V1.1   start each trace with a clean array
c
c==============================================================================
c
      program smoos
c
      character*79 version
      parameter(version=
     &   'SMOOS   V1.1   SMOOth Seismograms by sectral extension')
c
c files
      character*80 infile, outfile, nstring
      integer luin, luout
      parameter (luin=10, luout=11)
c data
      integer msamp, nsamp, i
      parameter(msamp=66000)
c calculation
      integer nfac, npow, powsamp, newpowsamp
      complex*16 spect(msamp)
      real*8 singback, singto
      parameter(singback=1.d0, singto=-1.d0)
c sff
      integer mfree, nfree, ntrace
      parameter(mfree=300)
      integer ierr, lenmax
      real libversion
      character timestamp*16, code *10
      character*80 free(mfree)*80
      character type*25, date*8, time*12, cs*1
      real c1, c2, c3
      integer idata(msamp), nstack
      real fdata(msamp), dt, tanf
      equivalence(fdata,idata) 
      logical last
      character wid2line*132
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=1)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-d/
      data opthasarg/.FALSE./
      data optarg/1h-/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: smoos infile outfile n'
      print *,'   or: smoos -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'SMOOth Seismograms by sectral extension'
        print *,' '
        stop
      endif
      if (iargc().ne.3) stop 'ERROR: wrong number of arguments'
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      call getarg(1, infile)
      call getarg(2, outfile)
      call getarg(3, nstring)
      read(nstring, *) nfac
      nfac=max(0,nfac)
c
c------------------------------------------------------------------------------
c go
c
      print *,'open file ',infile(1:index(infile,' '))
      call sff_ROpenFS(luin, infile,
     &                      libversion, timestamp, code,
     &                      nfree, free, lenmax, mfree,
     &                      type, cs, c1, c2, c3, date, time, ierr)
      if (ierr.ne.0) stop 'ERROR: opening input file'
      if (lenmax.gt.80) print *,'WARNING: ',
     &  'FREE lines read are longer than 80 characters - ',
     &  'truncated'
c 
      if (nfree.lt.mfree) then
        nfree=nfree+1
        free(nfree)=version
      endif
c 
      print *,'open file ',outfile(1:index(outfile,' '))
      if (index(code, 'S').gt.0) then
        call sff_WOpenFS(luout, outfile,
     &                      free, nfree,
     &                      type, cs, c1, c2, c3, date, time, ierr)
      else
        call sff_WOpenF(luout, outfile, free, nfree, ierr)
      endif
      if (ierr.ne.0) stop 'ERROR: opening output file'
c 
      last=.false.
      ntrace=0
      do while (.not.last)
        ntrace=ntrace+1
        nsamp=msamp
        print *,'work on trace ',ntrace
        call sff_RTraceFI(luin, tanf, dt,
     &                   wid2line, nsamp, fdata, idata, code, last,
     &                   nfree, free, mfree, lenmax,
     &                   cs, c1, c2, c3, nstack, ierr)
        if (ierr.ne.0) stop 'ERROR: reading trace'
        if (lenmax.gt.80) print *,'WARNING: ',
     &  'FREE lines read are longer than 80 characters - ',
     &  'truncated'
c
c start with a clean array
        do i=1,msamp
          spect(i)=(0.d0,0.d0)
        enddo
c 
c extend data
        npow=0
        powsamp=2**npow
        do while (powsamp.lt.nsamp)
          npow=npow+1
          powsamp=2**npow
        enddo
        newpowsamp=2**(npow+nfac)
        if (newpowsamp.gt.msamp) then
          print *,'ERROR: dataset has ',nsamp,' samples'
          print *,'ERROR: new number of samples should be ',newpowsamp
          print *,'ERROR: array size is ',msamp
          stop
        endif
        if (last) then
          print *,'extending from ',nsamp,' to ',newpowsamp,' samples'
          print *,'using ',powsamp,' samples for the first stage'
        endif
c 
        do i=1,nsamp
          spect(i)=dcmplx(fdata(i))
        enddo
        if (nsamp.gt.powsamp) then
          do i=nsamp+1,powsamp
            spect(i)=(0.d0,0.d0)
          enddo
        endif
        call tf_dfork(powsamp, spect, singback)
c        print *,'Nyquist coefficient of trace ',ntrace,':',spect(powsamp/2+1)
c        spect(powsamp/2+1)=(0.,0.)
        do i=0,powsamp/2-1
          spect(newpowsamp-i)=spect(powsamp-i)
        enddo
        do i=powsamp/2+2,newpowsamp-powsamp/2
          spect(i)=(0.d0,0.d0)
        enddo
        call tf_dfork(newpowsamp, spect, singto)
        do i=1,newpowsamp
          fdata(i)=sngl(real(spect(i)))*2.**(float(nfac)/2.)
        enddo
        nsamp=newpowsamp
        call sff_ModWid2samps(wid2line, nsamp)
        dt=dt*2.**(-nfac)
        call sff_ModWid2samprat(wid2line, 1./dt)
c write free block
        if (nfree.lt.mfree) then
          nfree=nfree+1
          write(free(nfree), 50) npow, npow+nfac
        endif
c 
        if (index(code, 'I').gt.0) then
          call sff_WTraceFI(luout, 
     &                   wid2line, nsamp, fdata, idata, last, 
     &                   nfree, free, 
     &                   cs, c1, c2, c3, nstack, ierr)
        else
          call sff_WTraceF(luout, 
     &                   wid2line, nsamp, fdata, idata, last, 
     &                   nfree, mfree, ierr)
        endif
        if (ierr.ne.0) stop 'ERROR: writing trace'
      enddo
c
      stop
   50 format('extended number of samples from 2**',i5,' to 2**',i5)
      end
c
c ----- END OF smoos.f -----
