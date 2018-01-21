c this is <gemmodpg.f>
c
c Copyright (c) 1995 by Jörg Dalkolmo
c Copyright (c) 1997 by Thomas Forbriger

c Some parts of the source code are copied from gemini by Jörg Dalkolmo.
c
c plot GEMINI earth model 
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
c V1.0   10/01/97   Thomas Forbriger
c V1.1   15/01/97   define radius range
c V1.2   14/04/00   use up-to-date commandline function
c V1.3   16/07/2005 plot files defining transverse isotropy
c
c----------------------------------------------------------------------
      character*70 version
      parameter(version='GEMMODPG   V1.3   plot gemini earth model')
c gemini
      integer maxlayer, nlayer
      parameter(maxlayer=30)
      double precision alpha(maxlayer, 4), beta(maxlayer, 4)
      double precision alphah(maxlayer, 4), betah(maxlayer, 4)
      double precision rho(maxlayer, 4), qm(maxlayer), qk(maxlayer)
      double precision eta(maxlayer, 4)
      double precision rb(0:maxlayer)
      integer iflso(maxlayer), nco(maxlayer)
      character text*72
c 
      character*80 filename, device, title
      logical debug, optani
      integer fin, lu
      parameter(lu=10)
      real rmin, rmax
      integer i,j
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-d,2h-D,2h-r,2h-t/
      data opthasarg/.TRUE.,.FALSE.,.TRUE.,.false./
      data optarg/3hx11,1h-,7h0.,1.e4,1h-/
c----------------------------------------------------------------------
      print *,version
      print *,'Usage: gemmodpg [-t] [ -d device ] [ -r rmin,rmax ] file'
      print *,'   or: gemmodpg -help'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, filename)
      if (filename.eq.'-help') then
        print *,'Plots a gemini earth model'
        print *,' '
        print *,'-d device    any pgplot device'
        print *,'-r rmin,rmax plot range from radius rmin to radius rmax'
        print *,'-t           earth model has transverse isotropy'
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
      read(optarg(3), *) rmin,rmax
      optani=optset(4)
      if (iargc().eq.(lastarg)) stop 'ERROR: no file'
      call getarg(iargc(), filename)
      fin=index(filename,' ')
c----------------------------------------------------------------------
c read file
      if (debug) print *,'DEBUG: filename ',filename(1:fin)
      if (optani) then
        call gemini_getani(filename, lu, maxlayer, eta,
     &    rb, qm, qk, rho, alpha, alphah, beta, betah,
     &    nlayer, iflso, nco, text)
      else
        call gemini_getmod(filename, lu, maxlayer, 
     &    rb, qm, qk, rho, alpha, beta, nlayer, iflso, nco, text)
      endif
c----------------------------------------------------------------------
      if (debug) then
        do i=1,nlayer
          print *,rb(i-1)
          print *,alpha(i,1),alphah(i,1),beta(i,1),
     &            betah(i,1),eta(i,1),rho(i,1),qm(i),qk(i)
          do j=2,nco(i)
            print *,alpha(i,j),alphah(i,j),beta(i,j),
     &              betah(i,j),eta(i,j),rho(i,j)
          enddo
          print *,' '
        enddo
        print *,rb(nlayer)
      endif
c----------------------------------------------------------------------
c check ranges
      rmin=max(rmin,sngl(rb(0)))
      rmax=min(rmax,sngl(rb(nlayer)))
c----------------------------------------------------------------------
c start plot
      if (optani) then
        call pgp_setdevice(device, 4, 2)
      else
        call pgp_setdevice(device, 5, 0)
      endif
      call pgsch(2.3)
c plot velocities
      if (optani) then
        title='Pv-velocity (km/s)'
        call plotcurve(maxlayer, nlayer, rb, alpha, nco, iflso, rmin, rmax,
     &    title, debug)
        call pgmtxt('T',1.,0.,0.,text)
        title='Ph-velocity (km/s)'
        call plotcurve(maxlayer, nlayer, rb, alphah, nco, iflso, rmin, rmax,
     &    title, debug)
        title='Sv-velocity (km/s)'
        call plotcurve(maxlayer, nlayer, rb, beta, nco, iflso, rmin, rmax,
     &    title, debug)
        title='Sh-velocity (km/s)'
        call plotcurve(maxlayer, nlayer, rb, betah, nco, iflso, rmin, rmax,
     &    title, debug)
        title='eta'
        call plotcurve(maxlayer, nlayer, rb, eta, nco, iflso, rmin, rmax,
     &    title, debug)
      else
        title='P-velocity (km/s)'
        call plotcurve(maxlayer, nlayer, rb, alpha, nco, iflso, rmin, rmax,
     &    title, debug)
        call pgmtxt('T',1.,0.,0.,text)
        title='S-velocity (km/s)'
        call plotcurve(maxlayer, nlayer, rb, beta, nco, iflso, rmin, rmax,
     &    title, debug)
      endif
      title='density (g/cm^3)'
      call plotcurve(maxlayer, nlayer, rb, rho, nco, iflso, rmin, rmax,
     &  title, debug)
      title='Qmu'
      call plotcurveb(maxlayer, nlayer, rb, qm, nco, iflso, rmin, rmax,
     &  title, debug)
      title='Qkappa'
      call plotcurveb(maxlayer, nlayer, rb, qk, nco, iflso, rmin, rmax,
     &  title, debug)
      call pgend
      stop
      end
c----------------------------------------------------------------------
      subroutine plotcurve(maxlayer, nlayer, rb, par, nco, iflso, rmin, rmax,
     &  title, debug)
c
      integer maxlayer, nlayer
      logical debug
      real rmin, rmax
      double precision rb(0:maxlayer), par(maxlayer, 4)
      integer nco(maxlayer), iflso(maxlayer)
      character title*(*)
c
      integer nsteps, i
      parameter(nsteps=400)
      real xmax, ymax, xmin, ymin,dx
      real x(nsteps), y(nsteps)
      double precision gemini_par
c 
      ymin=rmin
      ymax=rmax
      if (debug) print *,'DEBUG: create array'
      do i=1,nsteps
        y(i)=ymin+(ymax-ymin)*float(i-1)/float(nsteps-1)
        x(i)=gemini_par(par, rb, dble(y(i)), maxlayer, nlayer)
      enddo
      if (debug) print *,'DEBUG: array created'
      xmin=x(1)
      xmax=x(1)
      do i=1,nsteps
        xmin=min(xmin,x(i))
        xmax=max(xmax,x(i))
      enddo
      if (xmin.eq.xmax) xmin=0.99*xmax
      dx=xmax-xmin
      xmax=xmax+0.05*dx
      xmin=xmin-0.05*dx
      if (debug) print *,'DEBUG: go plot xmin/xmax',xmin,xmax
      call pgslw(1)
      call pgenv(xmin, xmax, ymin, ymax, 0, 2)
      call pglab(title, 'radius (km)', ' ')
      call pgslw(4)
      call pgline(nsteps, x, y)
      call pgslw(1)
      return
      end
c----------------------------------------------------------------------
      subroutine plotcurveb(maxlayer, nlayer, rb, par, nco, iflso, rmin, rmax,
     &  title, debug)
c
      integer maxlayer, nlayer
      logical debug
      real rmin, rmax
      double precision rb(0:maxlayer), par(maxlayer)
      integer nco(maxlayer), iflso(maxlayer)
      character title*(*)
c
      integer nsteps, i, gemini_layer, nl
      parameter(nsteps=400)
      real xmax, ymax, xmin, ymin,dx
      real x(nsteps), y(nsteps)
c 
      ymin=rmin
      ymax=rmax
      if (debug) print *,'DEBUG: create array'
      do i=1,nsteps
        y(i)=ymin+(ymax-ymin)*float(i-1)/float(nsteps-1)
        nl=gemini_layer(rb, dble(y(i)), maxlayer, nlayer)
        x(i)=sngl(par(nl))
      enddo
      if (debug) print *,'DEBUG: array created'
      xmin=x(1)
      xmax=x(1)
      do i=1,nsteps
        xmin=min(xmin,x(i))
        xmax=max(xmax,x(i))
      enddo
      if (xmin.eq.xmax) xmin=0.99*xmax
      dx=xmax-xmin
      xmax=xmax+0.05*dx
      xmin=xmin-0.05*dx
      if (debug) print *,'DEBUG: go plot xmin/xmax',xmin,xmax
      call pgslw(1)
      call pgenv(xmin, xmax, ymin, ymax, 0, 2)
      call pglab(title, 'radius (km)', ' ')
      call pgslw(4)
      call pgline(nsteps, x, y)
      call pgslw(1)
      return
      end

