c  this is file <amppg.f>
c======================================================================
c
c  AMPPG.F
c
c  Copyright (C) 1997 by Thomas Forbriger
c
c  Thomas Forbriger
c  Institut fuer Geophysik
c  Universitaet Stuttgart
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
c  plotting tf-phaseslowness/frequency-files from reamp (reflectivity)
c
c  22/02/97   V1.0   first running version (from dispg.f)
c
c  changes
c     V1.0   22/02/97   Thomas Forbriger
c     V1.1   24/02/97   thin grid
c     V1.2   11/06/97   correct version for full refmat response matrix
c     V1.3   11/12/99   changed to tf_ subroutines (formerly tflib_)
c     V1.4   20/09/03   increased size of command line argument buffer
c
c======================================================================
      program amppg
c----------------------------------------------------------------------

c  program version:
      character*79 version
      parameter(version='AMPPG V1.4   plot reflectivity amplitudes')

c  declare variables for io
      character*80 filename
      integer maxslo, maxfreq, plotslo, plotfreq, maxel
      integer dimplotslo, dimplotfreq
c dimensions of arrays
      parameter(maxslo=2100)
      parameter(maxfreq=4200)
      parameter(maxel=10)
c divisions for plot
      parameter(dimplotslo=300)
      parameter(dimplotfreq=400)
      integer nslo, nfreq
      real df
      real data(maxel, maxslo, maxfreq)
      complex indata(maxel, maxfreq)
      real plotdata(maxslo, maxfreq)
c  names of components
      character*4 compname(maxel)
c  declare internal variables
      real value, maxvalue, minvalue
      real contour(10)
      real minf, maxf, mins, maxs, fmin, umin, du
      character*120 title
      real transform(6)
c interpolation
      real fr1, fr2, slo1, slo2, frp, slp, dfre, dslo
      real val1, val2, val3, val4
      integer rfre, rslo
      logical newrect
      real tf_rectint
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=10)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
      character*80 outdevice
      logical isolines, color, resolution, smooth, grid
      real linelength, tracedx
      real tenpower
      integer comsel, setlinewidth
c   counters
      integer islo, ifreq, i
c   resolution
      real startfreq
      integer npoints
      parameter(npoints=100)
      real x(npoints), y(npoints)
c   debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-d,2h-i,2h-c,2h-r,2h-q,2h-p,2h-g,2h-s,2h-l,2h-D/
      data opthasarg/.TRUE.,2*.FALSE.,.TRUE.,.FALSE.,.TRUE.,.FALSE.,2*.TRUE.,
     &    .FALSE./
      data optarg/3hx11,2*1h-,5h1.,1.,1h-,3h2.5,1h-,2*1h1,1h-/
c matrix element names
      data compname/4hTR11,4hTR12,4hTR21,4hTR22,4hRM11,4hRM12,4hRM21,4hRM22,
     &              2htr,2hrm/

c======================================================================
c  give basic information
      print *,version
      print *,
     &'Usage: amppg filename [-d dev] [-i] [-c] [-r L,dx]'
      print *,
     &'                      [-q] [-p p] [-g] [-s comp] [-l width]'
      print *,
     &'or:    amppg -help'
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, filename)
      if (filename(1:5).eq.'-help') then
        print *,'  filename    any tf-type file'
        print *,'  -d dev      select output-device (see below for'
        print *,'              a list of possible devices) or use'
        print *,'              ''pro'' to look at data prolog'
        print *,'  -i          plot isolines'
        print *,'  -c          plot colored resolution marks and'
        print *,'              and colored grid'
        print *,'  -r L,dx     plot resolution marks according'
        print *,'              to a geophone-trace-stepwidth of'
        print *,'              dx and a total length of the'
        print *,'              geophone-line of L (in meters)'
        print *,'  -q          quick-mode does not smooth the data'
        print *,'              (this leads to bad results with'
        print *,'              isolines)'
        print *,'  -p p        the depth of the grayscale will be'
        print *,'              p powers of ten of data-value range'
        print *,'  -g          plot grid'
        print *,'  -s comp     select one component (1<=comp<=',maxel,')'
        print *,'  -l width    set line width'
        print *,' '
        print *,'selectable matrix components are:'
        do i=1,maxel
          print *,'  ',i,' : ',compname(i)
        enddo
        print *,' '
        print *,'=================================================='
        print *,'possible output devices: '
        call pgp_showdevices
        print *,' '
        print *,'=================================================='
        print *,' '
        print *,'array dimensions:'
        print *,'     maximum number of frequencies: ',maxfreq
        print *,'  maximum number of slowness steps: ', maxslo
        stop
      endif

c----------------------------------------------------------------------
c  get commandline
      outdevice='x11'
      isolines=.FALSE.
      color=.FALSE.
      resolution=.FALSE.
      smooth=.TRUE.
      tenpower=2.5
      grid=.FALSE.
      call getarg(1, filename)
c
c get commandline
c
      call tf_cmdline(2, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      if (optset(1)) outdevice=optarg(1)
      if (optset(2)) isolines=.TRUE.
      if (optset(3)) color=.TRUE.
      if (optset(4)) then
        resolution=.TRUE.
        read(optarg(4), *) linelength, tracedx
      endif
      if (optset(5)) smooth=.FALSE.
      if (optset(6)) read(optarg(6), *) tenpower
      if (optset(7)) grid=.TRUE.
      read(optarg(8), '(i10)') comsel
      read(optarg(9), '(i10)') setlinewidth
      debug=optset(10)
c  get datafile
      call ampfileread(filename, debug,
     &     maxslo, maxfreq, maxel,
     &     nslo, nfreq,
     &     df, fmin, du, umin, data, indata)
c  check dimensions for quick-mode
      print *,'dimensions of dataset read:'
      print *,'   slowness: ',nslo
      print *,'  frequency: ',nfreq

c----------------------------------------------------------------------
c  create plot-data
c  ----------------
c
      maxvalue=0.
      minf=fmin
      maxf=fmin+(nfreq-1)*df
      mins=umin
      maxs=umin+du*(nslo-1)
      if (smooth) then
c do linear interpolation
        plotslo=max(dimplotslo,nslo)
        plotfreq=max(dimplotfreq,nfreq)
c give a hint
        print *,'subdivisions of plot:'
        print *,'   slowness: ',plotslo
        print *,'  frequency: ',plotfreq
        print *,'doing linear interpolation'
c prepare some values
        dfre=(maxf-minf)/float(plotfreq-1)
        dslo=(maxs-mins)/float(plotslo-1)
c prepare surrounding ractangle
c   index
        rslo=1
c   coordinates
        slo1=mins
        slo2=umin+du
        do 12 islo=1,plotslo
          rfre=1
          fr1=minf
          fr2=fr1+df
          newrect=.TRUE.
          do 13 ifreq=1,plotfreq
c coordinates of plotpoint
            frp=minf+dfre*(ifreq-1)
            slp=mins+dslo*(islo-1)
c check rectangle
            if ((slp.gt.slo2).and.(rslo.lt.nslo)) then
              rslo=rslo+1
              slo1=umin+du*(rslo-1)
              slo2=umin+du*rslo
              newrect=.TRUE.
            endif
            if ((frp.gt.fr2).and.(rfre.lt.nfreq)) then
              rfre=rfre+1
              fr1=fr2
              fr2=fr2+df
              newrect=.TRUE.
            endif
c set values
            if (newrect) then
              val1=log10(max(1.e-20,abs(data(comsel, rslo,rfre))))
              val2=log10(max(1.e-20,abs(data(comsel, rslo,rfre+1))))
              val3=log10(max(1.e-20,abs(data(comsel, rslo+1,rfre+1))))
              val4=log10(max(1.e-20,abs(data(comsel, rslo+1,rfre))))
              newrect=.FALSE.
            endif
c interpolate
            value=tf_rectint(fr1,fr2,slo1,slo2,
     &                val1, val2, val3, val4, frp, slp)
            maxvalue=max(maxvalue,value)
            plotdata(islo, ifreq)=value
   13     continue
   12   continue
      else
c take array directly
        do 14 islo=1,nslo
          do 15 ifreq=1,nfreq
            value=log10(max(abs(data(comsel, islo,ifreq)),1.e-20))
            plotdata(islo,ifreq)=value
            maxvalue=max(maxvalue,value)
   15     continue
   14   continue
      endif
      minvalue=maxvalue-tenpower

c----------------------------------------------------------------------
c
c   plot data
c   ---------
c
      call pgp_setdevice(outdevice, 1,1)
      call pgslw(setlinewidth)
      write(title, 20) filename(1:index(filename,' ')-1),compname(comsel)
   20 format('phase-slowness/frequency - plot of ',a,'   component ',a)
      call pgenv(minf,maxf,mins,maxs,0,1)
      call pglab('frequency (Hz)',
     &           'phase-slowness (s/km)',
     &           title)
      if (smooth) then
        transform(1)=minf-dfre
        transform(3)=dfre
        transform(2)=0.
        transform(4)=mins-dslo
        transform(5)=dslo
        transform(6)=0.
        call pgupdt
        call pgsitf(0)
        call pgwedg('RG', 0.3, 3., maxvalue, 
     &    minvalue, 'log10(amplitude)')
        call pgupdt
        call pggray(plotdata, maxslo, maxfreq, 
     &              1, plotslo, 1, plotfreq,
     &              maxvalue, minvalue, transform)
      else
        transform(1)=minf-df
        transform(3)=df
        transform(2)=0.
        transform(4)=mins-(maxs-mins)/(nslo-1)
        transform(5)=(maxs-mins)/(nslo-1)
        transform(6)=0.
        call pgupdt
        call pgsitf(0)
        call pgwedg('RG', 0.3, 3., maxvalue, 
     &    minvalue, 'log10(amplitude)')
        call pgupdt
        if (debug) print *,nslo,nfreq
        call pggray(plotdata, maxslo, maxfreq, 
     &              1, nslo, 1, nfreq,
     &              maxvalue, minvalue, transform)
      endif
      call pgupdt
c----------------------------------------------------------------------
c 
c do isolines
c
      if (isolines) then
        do 21 i=1,5
          contour(i)=maxvalue*i/5
   21   continue
        if (smooth) then
          call pgcont(plotdata, maxslo, maxfreq, 
     &                1, plotslo, 1, plotfreq,
     &                contour, 5, transform, 0)
        else
          call pgcont(plotdata, maxslo, maxfreq, 
     &                1, nslo, 1, nfreq,
     &                contour, 5, transform, 0)
        endif
      endif
      call pgupdt
c----------------------------------------------------------------------
c
c do resolution borders
c
      if (resolution) then
        if (color) call pgsci(2)
c resolution
        startfreq=1/((maxs-mins)*linelength)
        x(1)=maxf
        y(1)=mins
        x(2)=0
        y(2)=mins
        x(3)=0.
        y(3)=maxs
        x(4)=startfreq
        y(4)=maxs
        do 30 ifreq=1,96
          x(ifreq+4)=(ifreq*(maxf-startfreq)/96)+startfreq
          y(ifreq+4)=mins+1/(x(ifreq+4)*linelength)
  30    continue
        call pgsfs(3)
        call pgpoly(npoints,x,y)
        call pgupdt
c plane border
        startfreq=1/((maxs-mins)*tracedx)
        do 31 ifreq=1,100
          x(ifreq)=(ifreq*(maxf-startfreq)/100)+startfreq
          y(ifreq)=1/(x(ifreq)*tracedx)
  31    continue
        call pgline(npoints,x,y)
        call pgupdt
      endif
c----------------------------------------------------------------------
c
c do grid
c
c      if (grid) then
c        if (color) then
c          call pgp_grid(2)
c        else
c          call pgp_grid(1)
c        endif
c      endif
      if (grid) then
        call pgsls(4)
        call pgslw(1)
        call pgbox('STG',0.0,0,'STG',0.0,0)
        call pgsls(1)
        call pgslw(setlinewidth)
      endif
      call pgbox('BCNST',0.0,0,'BCNST',0.0,0)
      call pgend
      stop
      end
c


c----------------------------------------------------------------------
c 
c this routine just imitates dispread
c
      subroutine ampfileread(filename, debug,
     &     maxslo, maxfreq, maxel,
     &     nslo, nfreq,
     &     df, fmin, du, umin, data, indata)
c
      character filename*(*)
      logical debug
      character*70 version, text1
      character*72 text2, text3
      integer maxslo, maxfreq, maxel
      real data(maxel, maxslo, maxfreq)
      complex indata(maxel, maxfreq)
      integer nslo, nfreq
      real df, fmin, du, umin
c 
      integer lu
      parameter(lu=20)
      real*8 zq, radius, uimin, uwil, uwir, umax, TL
      real*8 fimin, fiwil, fiwir, fimax, dt, nuref
      real*8 aljunk, bejunk, rhjunk
      real qajunk, qbjunk
      integer SL, fmi, fma
      integer i,k,l
c 
      open(lu, filename, form='unformatted', status='old', err=99)

      if (debug) print *,'start reading'
      read(lu, err=98, end=97) version
      read(lu, err=98, end=97) fmi, fma
      read(lu, err=98, end=97) dt, SL, fimin, fiwil, fiwir, fimax
      read(lu, err=98, end=97) Nslo, uimin, uwil, uwir, umax
      read(lu, err=98, end=97) ZQ, radius
      read(lu, err=98, end=97) qajunk, qbjunk, aljunk, bejunk, rhjunk
      read(lu, err=98, end=97) qajunk, qbjunk, aljunk, bejunk, rhjunk
      read(lu, err=98, end=97) text1, text2, text3
      read(lu, err=98, end=97) nuref

      TL=dt*SL
      df=1.d0/TL
      nfreq=fma-fmi+1

      if (nfreq.gt.maxfreq) stop 'ERROR: too many frequencies'
      if (nslo.gt.maxslo) stop 'ERROR: too many slownesses'

      do i=1,nslo
        if (debug) print *,'slo# ',i
        read(lu, err=98, end=97) ((indata(l,k), l=1,10), k=1,nfreq)
        do k=1,nfreq
          do l=1,10
            data(l,i,k)=abs(indata(l,k))
          enddo
        enddo
      enddo

      close(lu, err=96)

      du=(umax-umin)/(nslo-1)
      fmin=fimin
      umin=uimin

      return
   99 stop 'ERROR: opening ampfile'
   98 stop 'ERROR: reading ampfile'
   97 stop 'ERROR: reading ampfile - unexpected end'
   96 stop 'ERROR: closing ampfile'
      end
       
