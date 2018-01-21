c this is <flamops.f> by Thomas Forbriger 14/01/97
c
c plot flat earth model
c
c Copyright (c) 1997 by Thomas Forbriger (BFO Schiltach) 
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
c V1.0   14/01/97   based von flamop.f
c V1.1   11/02/97   reding new model format
c V1.2   12/09/03   now uses tf_cmdline (not tflib_cmdline)
c
c----------------------------------------------------------------------
      character*70 version
      parameter(version=
     &  'FLAMOPS   V1.2   plot flat earth model in spherical geometry')
c model
      integer maxlayer, nlayer,ilay
      parameter(maxlayer=1000)
      double precision alpha(maxlayer), beta(maxlayer), zt, zmin, zmax
      double precision rho(maxlayer), qa(maxlayer), qb(maxlayer), rmin, rmax
      double precision zb(maxlayer), rb(maxlayer), R, dr(maxlayer)
      character text*72
      double precision efa_r, efa_z
c 
      character*80 filename, device, title
      logical debug
      integer fin, lu
      parameter(lu=10)
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=3)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-d,2h-D,2h-r/
      data opthasarg/.TRUE.,.FALSE.,.TRUE./
      data optarg/3hx11,1h-,7h0.,1.e4/
c----------------------------------------------------------------------
      print *,version
      print *,'Usage: flamops [ -d device ] [ -z zmin,zmax ] file'
      print *,'   or: flamops -help'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, filename)
      if (filename.eq.'-help') then
        print *,'Plots a flat earth model created by chopmod. The model'
        print *,'will be transformed back to a spherical geometry.'
        print *,' '
        print *,'-d device    any pgplot device'
        print *,'-r rmin,rmax defines a radius range to be plotted'
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
      read(optarg(3), *) rmin, rmax
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
        dr(ilay)=efa_r(R, zt)-efa_r(R, zb(ilay))
        zt=zb(ilay)
      enddo
c----------------------------------------------------------------------
c set limits
      rmin=max(rmin, efa_r(R, zb(nlayer)))
      zmin=max(efa_z(R, rmax), 0.d0)
      zmax=min(efa_z(R, rmin), zb(nlayer))
c----------------------------------------------------------------------
c start plot
      call pgp_setdevice(device, 3, 2)
c      call pgsch(2.3)
c plot velocities
      title='P-velocity [km/s]'
      call plotcurve(maxlayer, nlayer, r, zmin, zmax, zb, alpha, title, 
     &  debug, 1)
      call pgmtxt('T',1.,0.,0.,text)
      title='S-velocity [km/s]'
      call plotcurve(maxlayer, nlayer, r, zmin, zmax, zb, beta, title, debug, 1)
      title='density [g/cm^3]'
      call plotcurve(maxlayer, nlayer, r, zmin, zmax, zb, rho, title, debug, 2)
      title='Qalpha'
      call plotcurve(maxlayer, nlayer, r, zmin, zmax, zb, qa, title, debug, 3)
      title='Qbeta'
      call plotcurve(maxlayer, nlayer, r, zmin, zmax, zb, qb, title, debug, 4)
      title='dR [km]'
      call plotcurve(maxlayer, nlayer, r, zmin, zmax, zb, dr, title, debug, 4)
      call pgend
      stop
   50 format(f10.3,6(1x,f10.3))
   51 format(a10,6(1x,a10)/77(1h-))
   99 stop 'ERROR: opening input file'
   98 stop 'ERROR: reading input file'
   97 stop 'ERROR: reading input file - unexpected end of file'
   96 stop 'ERROR: closing input file'
      end
c----------------------------------------------------------------------
      subroutine plotcurve(maxlayer, nlayer, r, zmin, zmax, zb, par, title, 
     &  debug, parmode)
c
      integer maxlayer, nlayer, parmode
      logical debug
      double precision zb(maxlayer), par(maxlayer), zmin, zmax, r
      character title*(*)
c
      integer nsteps, i, ilay
      parameter(nsteps=300)
      real xmax, ymax, xmin, ymin
      real x(nsteps), y(nsteps)
      double precision efa_denback, efa_velback, efa_r, efa_z, z
c 
      if (nsteps.lt.nlayer) print *,'WARNING: number of layers: ',nlayer
      if (nsteps.lt.nlayer) print *,'WARNING: number of plot steps: ',nsteps
      ymax=sngl(efa_r(R, zmin))
      ymin=sngl(efa_r(R, zmax))
      if (debug) print *,'DEBUG: create array ymin, ymax ',ymin,ymax
      ilay=0
    1 continue
        ilay=ilay+1
        if (zb(ilay).lt.zmin) goto 1
      do i=1,nsteps
        y(i)=ymax-(ymax-ymin)*float(i-1)/float(nsteps-1)
        z=efa_z(R, dble(y(i)))
    2   if ((zb(ilay).lt.z).and.(y(i).gt.ymin)) then
          ilay=ilay+1
          if (debug) print *,'DEBUG: ilay,i,zb,z,y,ymin,ymax ',
     &      ilay,i,zb(ilay),z,y(i),ymin,ymax
          goto 2
        endif
        if (parmode.eq.1) then
c velocity
          x(i)=sngl(efa_velback(R, z, par(ilay)))
        elseif (parmode.eq.2) then
c density
          x(i)=sngl(efa_denback(R, z, par(ilay)))
        elseif (parmode.eq.3) then
c Qalpha
c is no problem as Qalpha is not affected by the transformation from 
c flat to spherical and there is no way to resolve Qkappa here
c without velocity information
          x(i)=sngl(par(ilay))
        elseif (parmode.eq.4) then
c no transform
          x(i)=sngl(par(ilay))
        else
          stop 'ERROR: unknown parmode'
        endif
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
      call pgenv(xmin, xmax, ymin, ymax, 0, 2)
      call pglab(title, 'radius [km]', ' ')
      call pgslw(4)
      call pgline(nsteps, x, y)
      call pgslw(1)
      return
      end
