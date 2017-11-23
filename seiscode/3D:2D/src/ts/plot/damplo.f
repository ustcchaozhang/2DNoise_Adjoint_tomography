c this is <damplo.f>
c------------------------------------------------------------------------------
c
c Copyright 2001, 2010 by Thomas Forbriger (IMGF Frankfurt)
c
c plot energy damping
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
c    07/08/2001   V1.0   Thomas Forbriger
c    04/12/2009   V1.1   use correct DIN notation for units
c    04/03/2011   V1.2   implement format selection
c
c==============================================================================
c
      program damplo
c
      character*(*) version
      parameter(version='DAMPLO   V1.2   plot energy damping')
c
c input dataset
      character*80 filename, fileformat
      integer maxtraces, totmaxsamples
      parameter(maxtraces=500, totmaxsamples=2000000)
      integer lu
      parameter(lu=12)
      real data(totmaxsamples)
      integer idata(totmaxsamples)
      equivalence(data,idata)
      real toffset(maxtraces), tracedt(maxtraces), roffset(maxtraces)
      integer innsamples(maxtraces), firstsample(maxtraces)
      integer ntraces
c processing data
      real energy(maxtraces)
      real minoff, maxoff, maxen, sfact, minen
      integer itrace,isample,ierr
c graphics
      character*80 device
      integer idevice, pgp_open, logint
      character*80 facstring
      character*10 XOPT,YOPT
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=9)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      logical rscale,escale,logrms,logoff,tscale
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-D, 2h-v, 2h-r, 2h-e, 2h-d, 2h-l,2h-L,2h-t,'-ty'/
      data opthasarg/2*.FALSE.,3*.true.,2*.false.,2*.true./
      data optarg/2*1h-,2*2h1.,3hx11,2*1h-,2h1.,'sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: damplo file [-d device] [-r f|-e f|-t f] [-l] [-L]'
        print *,'   or: damplo -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'plot energy damping'
        print *,' '
        print *,'file         SFF file name'
        print *,'-d device    pgplot output device'
        print *,'-r f         power law scaling factor'
        print *,'-e f         exponential law scaling factor'
        print *,'-t f         exponential law scaling factor (base 10)'
        print *,'             (why that?)'
        print *,'-l           logarithmic rms scale'
        print *,'-L           logarithmic offset scale'
        print *,'-ty format   select file data format'
        call sff_help_formats
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(2, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      rscale=optset(3)
      escale=optset(4)
      if (rscale) read (optarg(3), *) sfact
      if (escale) read (optarg(4), *) sfact
      device=optarg(5)
      logrms=optset(6)
      logoff=optset(7)
      tscale=optset(8)
      if (tscale) read (optarg(8), *) sfact
      fileformat=optarg(9)
c
c------------------------------------------------------------------------------
c go
      call sff_select_input_format(fileformat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting input format'
      call getarg(1, filename)
      call sffu_simpleread(lu, filename, maxtraces, totmaxsamples,
     &     data, idata, toffset, tracedt, roffset, innsamples,
     &     firstsample, ntraces, verbose)

      minoff=roffset(1)
      maxoff=roffset(1)
      maxen=0.
      do itrace=1,ntraces
        energy(itrace)=0.d0
        do isample=1,innsamples(itrace)
          energy(itrace)=energy(itrace)+data(firstsample(itrace)+isample-1)**2
        enddo
        energy(itrace)=sqrt(energy(itrace)/innsamples(itrace))
        maxen=max(maxen,energy(itrace))
        minoff=min(minoff,roffset(itrace))
        maxoff=max(maxoff,roffset(itrace))
      enddo

      if (rscale) then
        maxen=0.
        do itrace=1,ntraces
          energy(itrace)=energy(itrace)*roffset(itrace)**sfact
          maxen=max(maxen,energy(itrace))
        enddo
        write(facstring, '(a,f5.3,a)') 'scaling with factor (r/1m)\u',
     &                   sfact,'\d'
      elseif (escale) then
        maxen=0.
        do itrace=1,ntraces
          energy(itrace)=energy(itrace)*exp(roffset(itrace)*sfact)
          maxen=max(maxen,energy(itrace))
        enddo
        write(facstring, '(a,f5.3,a)') 'scaling with factor exp(',
     &                   sfact,'*r/1m)'
      elseif (tscale) then
        maxen=0.
        do itrace=1,ntraces
          energy(itrace)=energy(itrace)*10.**(roffset(itrace)*sfact)
          maxen=max(maxen,energy(itrace))
        enddo
        write(facstring, '(a,f5.3,a)') 'scaling with factor 10\u',
     &                   sfact,'*r/1m\d'
      else
        facstring='no scaling'
      endif
      minen=1.
      do itrace=1,ntraces
        energy(itrace)=energy(itrace)/maxen
        minen=min(minen,energy(itrace))
      enddo
      maxen=1.

      XOPT='AGT'
      YOPT='AGT'
      logint=0
      if (logrms) then
        logint=logint+20
        do itrace=1,ntraces
          energy(itrace)=log10(energy(itrace))
        enddo
        minen=log10(minen)
        maxen=log10(maxen)
        YOPT='AGTL'
      endif
      if (logoff) then
        logint=logint+10
        do itrace=1,ntraces
          roffset(itrace)=log10(roffset(itrace))
        enddo
        minoff=log10(minoff)
        maxoff=log10(maxoff)
        XOPT='AGTL'
      endif

      idevice=pgp_open(device)
      call pgslw(3)
      call pgenv(minoff,maxoff,minen,maxen,0,logint)
      call pgsave
      call pgslw(1)
      call pgsls(4)
      call pgbox(XOPT,1.,1,YOPT,1.,1)
      call pgunsa
      call pgslw(2)
      call pglab('offset / m','scaled relative rms',facstring)
      call pgpt(ntraces,roffset,energy, -4)
      call pgmtxt('R',2.5,0.5,0.5,filename)
      call pgclos
c
      stop
      end
c
c ----- END OF damplo.f -----
