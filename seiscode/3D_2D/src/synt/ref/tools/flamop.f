c this is <flamop.f> by Thomas Forbriger 14/01/97
c
c Copyright (C) 1997 by Thomas Forbriger
c
c plot flat earth model
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
c V1.0   14/01/97   based von gemmodpg.f
c V1.1   11/02/97   reading new model format
c V1.2   22/02/97   writing ascii file
c
c----------------------------------------------------------------------
      character*70 version
      parameter(version='FLAMOP   V1.2   plot flat earth model')
c model
      integer maxlayer, nlayer,ilay
      parameter(maxlayer=1000)
      double precision alpha(maxlayer), beta(maxlayer), zt, zmin, zmax
      double precision rho(maxlayer), qa(maxlayer), qb(maxlayer)
      double precision zb(maxlayer), rb(maxlayer), R, dz(maxlayer)
      character text*72
c 
      character*80 filename, device, title
      logical debug
      integer fin, lu
      parameter(lu=10)
c output
      logical ascout
      character outfile*80
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-d,2h-D,2h-z,2h-o/
      data opthasarg/.TRUE.,.FALSE.,2*.TRUE./
      data optarg/3hx11,1h-,7h0.,1.e5,10hflamop.out/
c----------------------------------------------------------------------
      print *,version
      print *,'Usage: flamop [-d device] [-z zmin,zmax] [-o file] file'
      print *,'   or: flamop -help'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, filename)
      if (filename.eq.'-help') then
        print *,'Plots a flat earth model created by chopmod.'
        print *,' '
        print *,'-d device    any pgplot device'
        print *,'-z zmin,zmax defines a depth range to be plotted'
        print *,'-o file      write values to ascii-file'
        print *,' '
        call pgp_showdevices
        stop
      endif
c----------------------------------------------------------------------
c set options
      call tf_cmdline(1, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      device=optarg(1)
      debug=optset(2)
      read(optarg(3), *) zmin, zmax
      ascout=optset(4)
      outfile=optarg(4)
      if (iargc().eq.(lastarg)) stop 'ERROR: no file'
      call getarg(iargc(), filename)
      fin=index(filename,' ')
c----------------------------------------------------------------------
c read file
      if (debug) print *,'DEBUG: filename ',filename(1:fin)
      open(lu, file=filename, status='old', err=99)
      read(lu, '(a72)', err=98, end=97) text
      read(lu, '(/30x,i10/30x,f10.3///)', err=98, end=97) nlayer, R
      print 51,'Zb','alpha','beta','rho','Qa','Qb','Rb'
      do ilay=1,nlayer
        read(lu, fmt=50, err=98, end=97) zb(ilay), alpha(ilay),
     &    beta(ilay), rho(ilay), qa(ilay), qb(ilay), rb(ilay)
        print 50, zb(ilay), alpha(ilay), beta(ilay), rho(ilay),
     &    qa(ilay), qb(ilay), rb(ilay)
      enddo
      close(lu, err=96)
c----------------------------------------------------------------------
c prepare dz
      zt=0.d0
      do ilay=1,nlayer
        dz(ilay)=zb(ilay)-zt
        zt=zb(ilay)
      enddo
c----------------------------------------------------------------------
c set limits
      zmin=max(zmin,0.d0)
      zmax=min(zmax,zb(nlayer))
c----------------------------------------------------------------------
c start plot
      call pgp_setdevice(device, 3, 2)
c      call pgsch(2.3)
c plot velocities
      title='P-velocity [km/s]'
      call plotcurve(maxlayer, nlayer, zmin, zmax, zb, alpha, title, debug)
      call pgmtxt('T',1.,0.,0.,text)
      title='S-velocity [km/s]'
      call plotcurve(maxlayer, nlayer, zmin, zmax, zb, beta, title, debug)
      title='density [g/cm^3]'
      call plotcurve(maxlayer, nlayer, zmin, zmax, zb, rho, title, debug)
      title='Qalpha'
      call plotcurve(maxlayer, nlayer, zmin, zmax, zb, qa, title, debug)
      title='Qbeta'
      call plotcurve(maxlayer, nlayer, zmin, zmax, zb, qb, title, debug)
      title='dZ [km]'
      call plotcurve(maxlayer, nlayer, zmin, zmax, zb, dz, title, debug)
      call pgend
   50 format(f10.3,6(1x,f10.3))
   51 format(a10,6(1x,a10)/77(1h-))
c 
c do output
      if (ascout) then
        open(lu, file=outfile, err=95)
        do ilay=1,nlayer
          if (ilay.eq.1) then
            write(lu, '(7(e10.3,1x))', err=94) 0., alpha(ilay), beta(ilay), 
     &        rho(ilay), qa(ilay), qb(ilay), rb(ilay)
          else
            write(lu, '(7(e10.3,1x))', err=94) zb(ilay-1), 
     &        alpha(ilay), beta(ilay), 
     &        rho(ilay), qa(ilay), qb(ilay), rb(ilay)
          endif
          write(lu, '(7(e10.3,1x))', err=94) zb(ilay), alpha(ilay), beta(ilay), 
     &      rho(ilay), qa(ilay), qb(ilay), rb(ilay)
        enddo
        close(lu, err=93)
      endif
      stop
c
   99 stop 'ERROR: opening input file'
   98 stop 'ERROR: reading input file'
   97 stop 'ERROR: reading input file - unexpected end of file'
   96 stop 'ERROR: closing input file'
   95 stop 'ERROR: opening output file'
   94 stop 'ERROR: writing output file'
   93 stop 'ERROR: closing output file'
      end
c----------------------------------------------------------------------
      subroutine plotcurve(maxlayer, nlayer, zmin, zmax, zb, par, title, debug)
c
      integer maxlayer, nlayer
      logical debug
      double precision zb(maxlayer), par(maxlayer), zmin, zmax
      character title*(*)
c
      integer nsteps, i, ilay
      parameter(nsteps=300)
      real xmax, ymax, xmin, ymin
      real x(nsteps), y(nsteps)
c 
      if (nsteps.lt.nlayer) print *,'WARNING: number of layers: ',nlayer
      if (nsteps.lt.nlayer) print *,'WARNING: number of plot steps: ',nsteps
      ymin=sngl(zmin)
      ymax=sngl(zmax)
      if (debug) print *,'DEBUG: create array'
      ilay=0
    1 continue
        ilay=ilay+1
        if (zb(ilay).lt.zmin) goto 1
      do i=1,nsteps
        y(i)=ymin+(ymax-ymin)*float(i-1)/float(nsteps-1)
    2   if ((zb(ilay).lt.dble(y(i))).and.(y(i).lt.ymax)) then
          ilay=ilay+1
          if (debug) print *,'DEBUG: ilay,i,zb,y,ymin,ymax ',
     &      ilay,i,zb(ilay),y(i),ymin,ymax
          goto 2
        endif
        x(i)=sngl(par(ilay))
      enddo
      if (debug) print *,'DEBUG: array created'
      xmin=x(1)
      xmax=x(1)
      do i=1,nsteps
        xmin=min(xmin,x(i))
        xmax=max(xmax,x(i))
      enddo
      if (debug) print *,'DEBUG: go plot xmin/xmax',xmin,xmax
      call pgslw(1)
      call pgenv(xmin, xmax, ymax, ymin, 0, 2)
      call pglab(title, 'depth [km]', ' ')
      call pgslw(4)
      call pgline(nsteps, x, y)
      call pgslw(1)
      return
      end
