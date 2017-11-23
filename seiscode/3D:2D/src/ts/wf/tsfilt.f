c this is <tsfilt.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c filter a time series given as a singal table
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
c    24/05/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      program tsfilt
c
      character*(*) version
      parameter(version=
     &  'TSFILT   V1.0   filter a time series given as a signal table')
c
      character*80 infile, outfile, filtfile
      integer lui,luo,luf
      parameter(lui=10,luo=11,luf=12)
      integer msamp,nsamp,i
      parameter(msamp=1000000)
      real tmin,tsec,dt
      integer tfstr_trimlen
      character*120 par,msg
      character*3 typ
      double precision x(msamp),t(msamp)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.3)) then
        print *,version
        print *,'Usage: tsfilt infile outfile filtfile'
        print *,'   or: tsfilt -help'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'infile       input signal'
        print *,'outfile      output signal'
        print *,'filtfile     seife control file (see stufi)'
        print *,' '
        print *,'infile and outfile are two-column ASCII data files.'
        print *,'The first column contains time in seconds, the'
        print *,'second column provides the respective sample values.'
        print *,'Sampling is assumed to be equidistant. This'
        print *,'assumption is not(!) checked against actual sample'
        print *,'times. The time of first sample and the sampling'
        print *,'interval are derived from the first two time'
        print *,'values.'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,filtfile)
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
c
c------------------------------------------------------------------------------
c go
      nsamp=1
      open(lui,file=infile)
    1 read(lui, *, err=99, end=2) t(nsamp),x(nsamp)
      nsamp=nsamp+1
      goto 1
    2 close(lui)
      nsamp=nsamp-1
c     
      dt=t(2)-t(1)
      tsec=t(1)
      tmin=int(tsec/60.)
      tsec=tsec-60.*tmin
c 
      open(luf,file=filtfile)
    3 read(luf, '(a120)') msg
      if (msg(1:3).eq.'end') goto 4
      typ=msg(1:3)
      par=msg(4:)
      call seife(typ, par, nsamp, dt, tmin, tsec, x, msg)
      print *,msg(1:tfstr_trimlen(msg))
      goto 3
    4 close(luf)
c 
      open(luo,file=outfile)
      do i=1,nsamp
        write(luo, 50) tmin*60.+tsec+(i-1)*dt,x(i)
      enddo
      close(luo)
c
      stop
   50 format(2(2x,g20.14))
   99 stop 'ERROR: reading data'
      end
c
c ----- END OF tsfilt.f ----- 
