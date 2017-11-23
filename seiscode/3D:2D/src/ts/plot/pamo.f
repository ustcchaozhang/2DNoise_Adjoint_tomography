c this is <pamo.f>
c
c Copyright 1996, 2010 by Thomas Forbriger
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
c quick and easy particle motion
c
c V1.0   09/12/96   Thomas Forbriger
c V1.1   10/12/96   changed look and fell completely
c V1.2   18/08/98   use simple delay counter
c V1.3   18/09/98   make axis range equal
c V1.4   26/10/99   changed tflib_cmdline to tf_cmdline
c                   introduced GIF-mode
c V1.5   14/06/02   extended title string
c V1.6   19/06/02   print time in movie
c V1.7   24/02/06   provide colour output and arrows
c V1.8   27/02/06   support component swapping
c V1.9   04/11/15   provide input format selection
c
c======================================================================
      character*80 version, title
      parameter(version=
     &  'PAMO   V1.9   particle motion (Stromboli 96 Tilte)')
c 
      integer delay
      parameter(delay=90)
c 
      real efrac, xadd,yadd
      parameter(efrac=0.03)
      integer maxsamp
      parameter(maxsamp=100000)
      real fx(maxsamp), fy(maxsamp)
      integer ix(maxsamp), iy(maxsamp)
      integer xnsamp, ynsamp
      real xmax, xmin, ymax, ymin, xrange, yrange
      character*80 filename, giftitle
      integer ierr, lu, i, j
      parameter(lu=10)
      real lver, tanf, dt
      character timestamp*20, code*10
      character wid2linex*132, wid2liney*132, device*80
      logical last
c 
      integer inctime(7), fulltime(7), temptime(7)
      character*35 timestring
      character*80 inputformat
c 
      logical debug, animate, gifmode, usecolour, plotarrows, swapcompo
      integer speed, mil, sec, reachmil, reachsec, pglwidth
      real pgchsize
      integer maxindex
      parameter(maxindex=10)
      integer listindex(maxindex)
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=12)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/'-d','-D','-a','-t','-s','-G','-c','-l','-C','-i',
     &           '-S','-f'/
      data opthasarg/.TRUE.,2*.FALSE.,2*.TRUE.,.FALSE.,2*.TRUE.,.false.,
     &               .true.,.false.,.true./
      data optarg/'x11',2*'-','PAMO','1','-','1.','1','-','0','-',
     &               'sff'/
c----------------------------------------------------------------------
      print *,version
      print *,'Usage: pamo file [ -d device ] [ -t title ] [ -a ]'
      print *,'            [-s speed] [-G] [-c v] [-l v] [-C]'
      print *,'            [-i list] [-S] [-f type]'
      print *,'   or: pamo -help'
      if (iargc().lt.1) stop 'ERROR: no parameters'
      call getarg(1, filename)
      if (filename.eq.'-help') then
        print *,'This program plots something like paticle motion'
        print *,'and is capable of animation. The first two traces'
        print *,'of file are used.'
        print *,'default is:'
        print *,'   first trace is N-component'
        print *,'  second trace is E-component'
        print *,' '
        print *,'-d device    pgplot output device (default: x11)'
        print *,'-a           animate plot'
        print *,'-t title     specify plot title (def: program version)'
        print *,'-s speed     animation speed in millisecond steps'
        print *,'             (default: 10ms)'
        print *,'-G           animate in gifmode'
        print *,'             this will produce one file per sample'
        print *,'-c v         set character height to real value ''v'''
        print *,'-l v         set line width to integer value ''v'''
        print *,'-C           produce colour graph'
        print *,'-i list      list of sample index values for which'
        print *,'             arrowheads should be plotted'
        print *,'-S           swap components'
        print *,'-f type      input files are of file format ''type'' '
        call pgp_showdevices
        print *,' '
        call sff_help_formats
        stop
      endif
c----------------------------------------------------------------------
      title=version
      speed=10
      call tf_cmdline(2, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      device=optarg(1)
      debug=optset(2)
      animate=optset(3)
      if (optset(4)) title=optarg(4)
      if (optset(5)) read(optarg(5), *) speed
      gifmode=optset(6)
      read (optarg(7), *) pgchsize
      read (optarg(8), *) pglwidth
      usecolour=optset(9)
      plotarrows=optset(10)
      if (plotarrows) then
        do i=1,maxindex
          listindex(i)=-1
        enddo
        read(optarg(10), *, end=2, err=99), (listindex(i),i=1,maxindex)
    2   continue
      endif
      swapcompo=optset(11)
      inputformat=optarg(12)
      call getarg(1, filename)
c----------------------------------------------------------------------
      call sff_select_input_format(inputformat, ierr)
      if (ierr.ne.0) 
     &  stop 'ERROR: selected input format is not supported'
      call sff_ROpen(lu, filename, lver, timestamp, code, ierr)
      if (ierr.ne.0) stop 'ERROR: opening file'
      ynsamp=maxsamp
      xnsamp=maxsamp
      call sff_RTrace(lu, tanf, dt, wid2liney, ynsamp, 
     &  fy, iy, code, last, ierr)
      if (ierr.ne.0) stop 'ERROR: reading first trace (y=N)'
      if (last) stop 'ERROR: just one trace in file'
      call sff_RTrace(lu, tanf, dt, wid2linex, xnsamp, 
     &  fx, ix, code, last, ierr)
      if (ierr.ne.0) stop 'ERROR: reading second trace (x=E)'
      if (.not.(last)) close (lu)
c 
      call time_clear(inctime)
      call time_clear(fulltime)
      call sffu_timewid2(wid2liney, fulltime)
      call sffu_dttotime(dt, inctime)
c
      call pgp_setdevice(device,1,1)
      call pgslw(pglwidth)
      call pgsch(pgchsize)
      xnsamp=min(xnsamp, ynsamp)
      if (swapcompo) then
        do i=1,xnsamp
          xmin=fx(i)
          fx(i)=fy(i)
          fy(i)=xmin
        enddo
        print *,'N: ',wid2linex(1:70)
        print *,'E: ',wid2liney(1:70)
      else
        print *,'N: ',wid2liney(1:70)
        print *,'E: ',wid2linex(1:70)
      endif
      xmin=-fx(1)
      xmax=-fx(1)
      ymin=-fy(1)
      ymax=-fy(1)
      do i=2,xnsamp
        fx(i)=-fx(i)
        fy(i)=-fy(i)
        xmin=min(xmin,fx(i))
        ymin=min(ymin,fy(i))
        ymax=max(ymax,fy(i))
        xmax=max(xmax,fx(i))
      enddo
      xrange=xmax-xmin
      xadd=xrange*0.1
      yrange=ymax-ymin
      yadd=yrange*0.1
      yadd=max(xadd,yadd)
      xadd=max(xadd,yadd)
      call pgenv(xmin-xrange*efrac, xmax+xrange*efrac+xadd, 
     &  ymin-yrange*efrac, ymax+yrange*efrac+yadd, 1, -2)
      call pglab(' ',' ',title)
      call pgarro(0.,ymin-yrange*efrac,0.,ymax+yrange*efrac+yadd)
      call pgarro(xmin-xrange*efrac,0.,xmax+xrange*efrac+xadd,0.)
      call pgtext(xmax+xadd+xrange*(efrac-0.03),0.-yrange*0.06,'E')
      call pgtext(0.-xrange*0.06,ymax+yadd+yrange*(efrac-0.03),'N')
      call pgsave
      if (usecolour) then
        call pgsci(2)
      endif
      call pgline(xnsamp, fx, fy)
      call pgunsa
      if (plotarrows) then
        call pgsave
        if (usecolour) then
          call pgsci(4)
        endif
        do i=1,maxindex
          if ((listindex(i).gt.0).and.(listindex(i).lt.xnsamp)) then
            j=listindex(i)
            call pgarro(fx(j),fy(j),fx(j+1),fy(j+1))
          endif
        enddo
        call pgunsa
      endif
      call pgupdt
      if (gifmode) then
        do i=1,xnsamp
          print *,'gifmode #',i,'/',xnsamp
c 
          call pgenv(xmin-xrange*efrac, xmax+xrange*efrac+xadd, 
     &      ymin-yrange*efrac, ymax+yrange*efrac+yadd, 1, -2)
          call pglab(' ',' ',title)
          call pgarro(0.,ymin-yrange*efrac,0.,ymax+yrange*efrac+yadd)
          call pgarro(xmin-xrange*efrac,0.,xmax+xrange*efrac+xadd,0.)
          call pgtext(xmax+xadd+xrange*(efrac-0.03),0.-yrange*0.06,'E')
          call pgtext(0.-xrange*0.06,ymax+yadd+yrange*(efrac-0.03),'N')
          call pgline(xnsamp, fx, fy)
c 
          call time_sprint(fulltime, timestring)
          giftitle=timestring(5:32)
c          write (giftitle, '(i4.4,1h/,i4.4,1h ,a)') 
c     &      i,xnsamp, timestring
          call time_add(fulltime, inctime, temptime)
          call time_copy(temptime, fulltime)
          call pgmtxt('T',1.,0.,0.,giftitle)
          call pgmove(0.,0.)
          call pgsave
          call pgsci(2)
          call pgslw(10)
          do j=1,i 
            call pgdraw(fx(j),fy(j))
          enddo
          call pgunsa
        enddo
      elseif (animate) then
        call pgmove(0.,0.)
        call pgsci(2)
        call pgslw(10)
        do i=1,xnsamp
          call pgdraw(fx(i),fy(i))
          call pgupdt
c          call tfseconds(sec, mil)
c          reachmil=mil+speed
c          reachsec=sec
c    1     if (reachmil.gt.1000) then
c            reachmil=reachmil-1000
c            reachsec=reachsec+1
c            goto 1
c          endif
c    2     call tfseconds(sec, mil)
c          if (sec.lt.reachsec) goto 2
c          if (sec.gt.reachsec) goto 3
c          if (mil.lt.reachmil) goto 2
c    3     continue
          do reachsec=1,speed
            sec=12
            mil=1
            do reachmil=1,500
              mil=mil+mil*sec
              if (mil.gt.50000) mil=1
            enddo
          enddo
        enddo
      endif
      call pgend
      stop
   99 stop 'ERROR: reading index list'
      end
