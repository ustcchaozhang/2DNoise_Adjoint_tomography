c this is <modeinterference.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
c
c demonstrate the effect on Fourier phase by interference of surface wave modes
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
c This program was copied from $HOME/doz/ofw/interfer/interfer.f
c
c REVISIONS and CHANGES
c    13/08/2013   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program modeinterference
c
      character*(*) version
      parameter(version='MODEINTERFERENCE   V1.0   '//
     &   'demonstrate the effect on Fourier phase')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=8)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c 
      real xmin,xmax
      integer nx,idevice,pgp_open,i
      real c1,c2,f,a1,a2,x,rat,dx
      character*80 device
      real w1x,w1y,w2x,w2y,wgx,wgy,cx,cy
      real w1x0,w1y0,w2x0,w2y0,wgx0,wgy0,phase
      complex v1,v2,v10,v20,vg,vg0
      real pi2
      parameter(pi2=2.*3.141592653589)
      integer col1,col2,tlw
      parameter(col1=2,col2=4,tlw=5)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-D, 2h-v, 2h-x, 2h-n ,2h-c,2h-a,2h-d,2h-f/
      data opthasarg/2*.FALSE.,6*.TRUE./
      data optarg/2*1h-,'0.,10.','6','150.,340.','1.,1.2','x11','15.'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.0)) then
        print *,version
        print *,'Usage: modeinterference [-v] [-d dev] [-D]'
        print *,'              [-c c1,c2] [-a a1,a1] [-n N] [-x x1,x2]'      
        print *,'              [-f f]'
        print *,'   or: modeinterference -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'If modes with different amplitude and phase velocity'
        print *,'interfere, the resulting Fourier phase no longer'
        print *,'is k*r, where k=omega/c is wavenumber, omegae is'
        print *,'angular frequency, c is phase velocity, and r is offset'
        print *,' '
        print *,'-v        be verbose'
        print *,'-D        debug mode'
        print *,'-d dev    plot to PGPLOT device dev'
        print *,'-f f      plot for frequency f in Hz'
        print *,'-c c1,c2  plot for phase velocities c1 and c2'
        print *,'-a a1,a2  plot for amplitudes a1 and a2'
        print *,'-x x1,x2  plot for offsets x1 to x2'
        print *,'-n N      plot at N offsets'
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
      read(optarg(3), *) xmin,xmax
      read(optarg(4), *) nx
      read(optarg(5), *) c1,c2
      read(optarg(6), *) a1,a2
      device=optarg(7)
      read(optarg(8), *) f
c
      print *,'xmin,xmax: ',xmin,xmax
      print *,'c1,c2: ',c1,c2
      print *,'a1,a2: ',a1,a2
      print *,'f: ',f
c
c------------------------------------------------------------------------------
c go
      dx=(xmax-xmin)/(nx-1)
      rat=0.45*dx/(a1+a2)
c 
      idevice=pgp_open(device)
      call pgsvp(0.01,0.99,0.01,0.99)
      call pgenv(xmin-0.6*dx,xmax+0.6*dx,0.,3.2*dx,1,-2)
      call pgslw(1)
      call pgbox('BN',0.0,0,'',0.0,0)
      call pgsls(1)
      call pglab('offset / m','','')
      call pgsfs(2)
      call pgsah(2,30.,1.)
      call pgsch(0.5)
c
      do i=1,nx
        x=(i-1)*(xmax-xmin)/(nx-1)+xmin
        v1=cexp(-(0.,1.)*f*pi2*x/c1)*a1*rat
        v2=cexp(-(0.,1.)*f*pi2*x/c2)*a2*rat
        vg=v1+v2
        w1x=real(v1)
        w2x=real(v2)
        wgx=real(vg)
        w1y=aimag(v1)
        w2y=aimag(v2)
        wgy=aimag(vg)
        if (i.eq.1) then
          vg0=vg
          v10=v1
          v20=v2
          w1x0=w1x
          w2x0=w2x
          wgx0=wgx
          w1y0=w1y
          w2y0=w2y
          wgy0=wgy
        endif
        cx=x

        cy=2.6*dx
        call pgcirc(cx,cy,abs(v10))
        call pgarro(cx,cy,cx+w1x0,cy+w1y0)
        call pgsci(col1)
        call pgslw(tlw)
        call pgarro(cx,cy,cx+w1x,cy+w1y)
        call pgsci(1)
        call pgslw(1)

        cy=1.6*dx
        call pgcirc(cx,cy,abs(v20))
        call pgarro(cx,cy,cx+w2x0,cy+w2y0)
        call pgsci(col2)
        call pgslw(tlw)
        call pgarro(cx,cy,cx+w2x,cy+w2y)
        call pgsci(1)
        call pgslw(1)

        cy=0.6*dx
        call pgcirc(cx,cy,abs(vg0))
        call pgarro(cx,cy,cx+wgx0,cy+wgy0)
        call pgslw(tlw)
        call pgarro(cx,cy,cx+wgx,cy+wgy)
        call pgsci(col1)
        call pgarro(cx,cy,cx+w1x,cy+w1y)
        call pgsci(col2)
        call pgarro(cx+w1x,cy+w1y,cx+wgx,cy+wgy)
        call pgsci(1)
        call pgslw(1)

      enddo
c
      call pgclos
c
      stop
      end
c
c ----- END OF modeinterference.f ----- 
