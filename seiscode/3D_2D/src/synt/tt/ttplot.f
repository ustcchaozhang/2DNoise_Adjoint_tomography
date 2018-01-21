c this is <ttplot.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
c
c plot model and travel times for dipping layers
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
c    16/09/99   V1.0   Thomas Forbriger
c    21/03/02   V1.1   get it running again
c    08/06/10   V1.2   did not check whether data was actually read before
c                      plotting data values for a non-sense number of offsets
c                      now check, whether data values were actually read
c
c==============================================================================
c
      program ttplot
c
      character*79 version
      parameter(version='TTPLOT   V1.2   plot model and travel times for dipping layers')
c
      real topdip, backoff, pgcheight,tterror
      character*80 pgpdev,file1,file2,modelfile
      logical asprat,bandw,ploteach,plotorig,readdata1,readdata2
      integer pglwidth
c 
      integer maxdata,ndata1,ndata2
      parameter(maxdata=1000)
      real xdata1(maxdata)
      real xdata2(maxdata)
      real tdata1(maxdata)
      real tdata2(maxdata)
c 
      integer maxtt
      parameter(maxtt=1000)
      real off(maxtt), t1(maxtt), t2(maxtt)
c 
      real pi
      parameter(pi=3.1415926535897931159979)
c 
      integer pgp_open
      integer pgpdevid
c 
      real xmax,xmin,zmax,zmin
      real backoffset,layphi,layd,layv
      real cmpx,cmpz,cmpphi,sl1,sl2,ti1,ti2
      real cmplength,thick1,thick2
      real polyx(4),polyz(4)
      integer nlay,i
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=13)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-D, 2h-v,2h-a,2h-d,2h-A,2h-H,2h-l,2h-W,2h-e,2h-o,2h-E,
     &           2h-h,2h-r/
      data opthasarg/2*.FALSE.,2*.TRUE.,.FALSE.,2*.TRUE.,3*.FALSE.,3*.TRUE./
      data optarg/2*1h-,2h0.,3hx11,1h-,3h1.2,1h2,3*1h-,2h2.,2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: ttplot model offset [-a angle] [-d dev]'
        print *,'              [-v] [-D] [-A] [-H height] [-l width]'
        print *,'              [-W] [-e] [-o] [-E error]'
        print *,'              [-h file] [-r file]'
        print *,'   or: ttplot -help'
        if (iargc().lt.1) stop 'ERROR: missing arguments'
        print *,' '
        print *,'plot model and travel times for dipping layers'
        print *,' '
        print *,'model        file containing model parameters'
        print *,'offset       offset between shotpoints'
        print *,'-a angle     dipping angle of free surface'
        print *,'             (default ist 0.)'
        print *,'-d dev       select output device'
        print *,'-v           be verbose'
        print *,'-D           give debug info'
        print *,'-A           aspect ratio=1'
        print *,'-H height    set character height'
        print *,'-l width     set line width'
        print *,'-W           plot black on white'
        print *,'-e           plot each refractors travel time curve'
        print *,'-o           plot original model'
        print *,'-E error     provide error bar for travel times in ms'
        print *,'-h file      read travel times for shot at (0/0)'
        print *,'-r file      read travel times for shot at (offset/0)'
        print *,' '
        print *,'Values in traveltime data files must be given as'
        print *,'follows:'
        print *,'1st line: comment'
        print *,'2nd line: number of offsets'
        print *,'3rd line: offset traveltime'
        print *,'next lines like 3rd line'
        print *,'offset and traveltime values are given in units of'
        print *,'km and s'
        print *,' '
        call pgp_showdevices
        print *,' '
        print *,'$Id$'
        stop
      endif
      if (iargc().lt.2) stop 'ERROR: missing arguments'
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      read(optarg(3), *) topdip
      pgpdev=optarg(4)
      asprat=optset(5)
      read(optarg(6), *) pgcheight
      read(optarg(7), *) pglwidth
      bandw=optset(8)
      ploteach=optset(9)
      plotorig=optset(10)
      read(optarg(11), *) tterror
      readdata1=optset(12)
      readdata2=optset(13)
      file1=optarg(12)
      file2=optarg(13)
c 
      call getarg(2, argument)
      read(argument, *) backoff
c
c------------------------------------------------------------------------------
c go
      call getarg(1, modelfile)
      call tt_readgrmod(modelfile, backoff, topdip, verbose)
      if (debug) print *,'DEBUG: read model from file'
      if (readdata1) call readtt(file1,maxdata,ndata1,xdata1,tdata1,verbose)
      if (readdata2) call readtt(file2,maxdata,ndata2,xdata2,tdata2,verbose)
c 
      if (debug) print *,'DEBUG: model and data read from files'
c 
      pgpdevid=pgp_open(pgpdev)
      if (pgpdevid.eq.0) stop 'ERROR: could not open graphics device'
      call pgsubp(1,2)
      call pgpanl(1,1)
      call pgsch(pgcheight)
      call pgslw(pglwidth)
      if (bandw) then
        call pgscr(0,1.,1.,1.)
        call pgscr(1,0.,0.,0.)
      endif
      call pgscr(2,1.,0.,0.)
      call pgscr(3,0.,0.,1.)
      call pgscr(4,0.6,.6,0.0)
c 
c PLOT MODEL
c ==========
      call pgpanl(1,1)
c 
c check how large the model plot will be
      call tt_inlay(nlay)
      nlay=nlay+1
      call tt_inbo(backoffset)
      call tt_ingrmod(nlay,layphi,layd,layv)
      zmax=(layd+backoffset*sin(max(0.,(layphi-topdip)*pi/180.)))*1.1
      zmin=-0.1*layd-backoffset*sin(max(topdip,0.)*pi/180.)
      xmin=-0.1*backoffset+min(0.,layd*sin(topdip*pi/180.))
      xmax=1.1*backoffset
c 
      if (debug) print *,'DEBUG: prepare plot window and viewport'
c prepare plot window and viewport
      if (asprat) then
        if (plotorig) then
          call pgenv(xmin,xmax,zmax,zmin,1,1)
        else
          call pgenv(xmin,xmax,zmax,zmin,1,0)
        endif
      else
        if (plotorig) then
          call pgenv(xmin,xmax,zmax,zmin,0,1)
        else
          call pgenv(xmin,xmax,zmax,zmin,0,0)
        endif
      endif
      call pgsave
      call pgsls(4)
      call pgslw(1)
      call pgbox('GTS',0.,0,'GTS',0.,0)
      call pgunsa
      call pglab('offset (m)','depth (m)',modelfile)
c 
c plot original GR-model interfaces
      if (debug) print *,'DEBUG: plot original GR-model interfaces'
      if (plotorig) then
        do i=1,nlay
          call tt_ingrmod(i,layphi,layd,layv)
c          print *,i,layphi,layd,layv
          layphi=layphi*pi/180.
          call pgmove(xmin,(layd+xmin*tan(layphi)))
          call pgdraw(xmax,(layd+xmax*tan(layphi)))
        enddo
      endif
c 
c go and convert model
      call tt_convgrmod(verbose)
      call tt_prepmod(verbose)
c 
c plot dipped layer interfaces and plumbline points
      call pgsave
      do i=1,nlay
        call tt_incmp(i,cmpx,cmpz,cmpphi)
        call tt_ingeom(i,cmplength,thick1,thick2)
c        print *,i,cmpx,cmpz,cmpphi,cmpphi*180./pi
        call pgsci(2)
        call pgmove(xmin,cmpz-(xmin-cmpx)*tan(cmpphi))
        call pgdraw(xmax,cmpz-(xmax-cmpx)*tan(cmpphi))
        call pgpt1(cmpx,cmpz,4)
        call pgsci(4)
        call pgpt1(cmpx-cmplength*cos(cmpphi),cmpz+cmplength*sin(cmpphi),4)
        call pgpt1(cmpx+cmplength*cos(cmpphi),cmpz-cmplength*sin(cmpphi),4)
        call pgsci(2)
      enddo
c 
c plot dipped plumbline at x=0
      call tt_incmp(1,cmpx,cmpz,cmpphi)
      call pgmove(sin(cmpphi)*zmin,zmin*cos(cmpphi))
      call pgdraw(sin(cmpphi)*zmax,zmax*cos(cmpphi))
c 
c plot plumbline-chain
      call pgmove(cmpx,cmpz)
      do i=2,nlay
        call tt_incmp(i,cmpx,cmpz,cmpphi)
        call pgdraw(cmpx,cmpz)
      enddo
      call pgunsa
c 
c not hatch the dipped layers areas
      call pgsave
      call pgsfs(3)
      call pgslw(1)
      call pgsci(3)
      do i=1,nlay-1
        call pgshs(45.+90.*(2*int(i/2)-i),2.,0.)
        call tt_incmp(i,cmpx,cmpz,cmpphi)
        polyx(1)=xmin
        polyx(2)=xmax
        polyx(3)=xmax
        polyx(4)=xmin
        polyz(1)=cmpz-(xmin-cmpx)*tan(cmpphi)
        polyz(2)=cmpz-(xmax-cmpx)*tan(cmpphi)
        call tt_incmp(i+1,cmpx,cmpz,cmpphi)
        polyz(3)=cmpz-(xmax-cmpx)*tan(cmpphi)
        polyz(4)=cmpz-(xmin-cmpx)*tan(cmpphi)
        call pgpoly(4,polyx,polyz)
      enddo
      call pgshs(45.+90.*(2*int(nlay/2)-nlay),2.,0.)
      call tt_incmp(nlay,cmpx,cmpz,cmpphi)
      polyx(1)=xmin
      polyx(2)=xmax
      polyx(3)=xmax
      polyx(4)=xmin
      polyz(1)=cmpz-(xmin-cmpx)*tan(cmpphi)
      polyz(2)=cmpz-(xmax-cmpx)*tan(cmpphi)
      polyz(3)=zmax
      polyz(4)=zmax
      call pgpoly(4,polyx,polyz)
      call pgunsa
c 
c plot shotpoint plumblines
      call pgsave
      call pgsci(4)
      call pgslw(1)
      do i=2,nlay
        call tt_incmp(i,cmpx,cmpz,cmpphi)
        call tt_ingeom(i,cmplength,thick1,thick2)
        cmpx=cmpx+cmplength*cos(cmpphi)
        cmpz=cmpz-cmplength*sin(cmpphi)
        call tt_ingeom(i-1,cmplength,thick1,thick2)
        call pgmove(cmpx,cmpz)
        call pgdraw(cmpx-thick2*sin(cmpphi),
     &              cmpz-thick2*cos(cmpphi))
        call pgmove(cmpx+cmplength*cos(cmpphi),cmpz-cmplength*sin(cmpphi))
c 
        call tt_incmp(i,cmpx,cmpz,cmpphi)
        call tt_ingeom(i,cmplength,thick1,thick2)
        cmpx=cmpx-cmplength*cos(cmpphi)
        cmpz=cmpz+cmplength*sin(cmpphi)
        call tt_ingeom(i-1,cmplength,thick1,thick2)
        call pgmove(cmpx,cmpz)
        call pgdraw(cmpx-thick1*sin(cmpphi),
     &              cmpz-thick1*cos(cmpphi))
        call pgmove(cmpx+cmplength*cos(cmpphi),cmpz-cmplength*sin(cmpphi))
      enddo
      call pgunsa
c 
c plot shotpoints
      call pgsave
      call tt_incmp(1,cmpx,cmpz,cmpphi)
      call pgsch(2.*pgcheight)
      call pgslw(1)
      call pgsci(2)
      call pgpt1(0.,0.,18)
      call pgpt1(2.*cmpx,2.*cmpz,18)
      call pgsci(1)
      call pgsch(1.8*pgcheight)
      call pgpt1(0.,0.,12)
      call pgpt1(2.*cmpx,2.*cmpz,12)
      call pgunsa
c 
c PLOT TRAVELTIMES
c ================
c 
      call tt_propagate(verbose)
      call pgpanl(1,2)
c 
c get traveltimes
      zmin=0.
      zmax=0.
      do i=1,maxtt
        off(i)=float(i-1)*(xmax-xmin)/float(maxtt-1)+xmin
        call tt_intt(off(i),t1(i),t2(i))
        t1(i)=1.e3*t1(i)
        t2(i)=1.e3*t2(i)
        zmax=max(zmax,t1(i),t2(i))
      enddo
      zmax=1.1*zmax
c 
c prepare plot window and viewport
      call pgpanl(1,1)
      call pgvstd
      call pgswin(xmin,xmax,zmin,zmax)
      call pgbox('BCTSN',0.,0,'BCTSNM',0.,0)
      call pgsave
      call pgsls(4)
      call pgslw(1)
      call pgbox('GTS',0.,0,'GTS',0.,0)
      call pgunsa
      call pglab('offset (m)','traveltime (ms)',' ')
      call pgmtxt('T',2.,0.,0.,file1)
      call pgmtxt('T',2.,1.,1.,file2)
c
c plot traveltimes
      call pgsave
      call pgsci(3)
      call pgline(maxtt,off,t1)
      call pgline(maxtt,off,t2)
      call pgunsa
c 
c plot single refractor lines
      if (ploteach) then
        do i=1,nlay
          call tt_inline(i,sl1,sl2,ti1,ti2)
          ti1=ti1*1.e3
          ti2=ti2*1.e3
          sl1=sl1*1.e3
          sl2=sl2*1.e3
          call pgmove(xmin, sl1*xmin+ti1)
          call pgdraw(xmax, sl1*xmax+ti1)
          call pgmove(xmin, sl2*(backoff-xmin)+ti2)
          call pgdraw(xmax, sl2*(backoff-xmax)+ti2)
        enddo
      endif
c 
c check traveltimes
      call pgsave
      call pgsci(4)
      call pgslw(1)
      call tt_intt(backoff,ti1,ti2)
      ti1=1.e3*ti1
      call pgpt1(backoff,ti1,4)
      call pgmove(xmin,ti1)
      call pgdraw(xmax,ti1)
      call tt_intt(0.,ti1,ti2)
      ti2=1.e3*ti2
      call pgpt1(0.,ti2,4)
      call pgmove(xmin,ti2)
      call pgdraw(xmax,ti2)
      call pgunsa
c 
c plot shotpoints
      call pgsave
      call pgsch(2.*pgcheight)
      call pgslw(1)
      call pgsci(2)
      call pgpt1(0.,0.,18)
      call pgpt1(backoff,0.,18)
      call pgsci(1)
      call pgsch(1.8*pgcheight)
      call pgpt1(0.,0.,12)
      call pgpt1(backoff,0.,12)
      call pgunsa
c 
c plot data
      call pgsave
      call pgslw(1)
      call pgsci(2)
      if (readdata1) then
      do i=1,ndata1
        call pgpt1(xdata1(i),tdata1(i),14)
        call pgerr1(6,xdata1(i),tdata1(i),tterror,1.)
      enddo
      endif
      if (readdata2) then
      do i=1,ndata2
        call pgpt1(backoff-xdata2(i),tdata2(i),14)
        call pgerr1(6,backoff-xdata2(i),tdata2(i),tterror,1.)
      enddo
      endif
      call pgunsa
c 
      call pgclos 
c
      stop
      end
c
c
c======================================================================
c
      subroutine readtt(filename,nmax,n,x,t,verb)
c 
c read traveltime picks from file to pickset iset
c
      integer nmax,n
      real x(nmax),t(nmax)
      character filename*(*)
      logical verb
c 
      integer lu, i
      parameter(lu=10)
c 
      if (verb) print *,'reading picks from ',filename(1:index(filename,' '))
c 
      open(lu, file=filename, status='old', err=99)
      read(lu, '(/,i10)', err=98, end=97) n
c 
      if (n.gt.nmax) stop 'ERROR: too many samples'
c 
      do i=1,n
        read(lu, *, err=98, end=97) x(i), t(i)
        x(i)=1.e3*x(i)
        t(i)=1.e3*t(i)
      enddo
c 
      close(lu, err=96)
      if (verb) print *,'file read and closed'
c 
      return
   50 format('input filename for traveltimes:')
   51 format(a)
   99 stop 'ERROR: opening travel time file'
   98 stop 'ERROR: reading travel time file'
   97 stop 'ERROR: reading travel time file - unexpected end'
   96 stop 'ERROR: closing travel time file'
      end
c 
c ----- END OF ttplot.f -----
