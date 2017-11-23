c  this is file <pgp.f>    
c======================================================================
c
c  PGP.F - Library
c
c Copyright (c) 1995 by Thomas Forbriger (IfG Stuttgart)
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
c  these are output-subroutines to prepare different devices for
c  pgplot-output
c
c  requires the pgplot library
c
c  revision
c     V1.0   27/10/95   first running version
c     V1.1   22/12/95   new soubroutine for simple plotting
c     V1.2   29/3/96    added grid plotting
c     V1.3   14/11/96   added pgplot device information
c     V1.4   10/12/96   changed device character length to any
c     V1.5   15/01/98   support pgplot V5.2 pgopen
c
c======================================================================
cS
c show devices
      subroutine pgp_showdevices
cE
      print *,'To select an ouput device you may use one of the'
      print *,'following short cuts:'
      print *,'  pgp      choose your own pglot-device'
      print *,'  def      use default (set by PGPLOT_DEV)'
      print *,'  x11      X-Windows Server'
      print *,'  lp       postscript to stdout'
      print *,'  ps       postscript to file pgp.ps'
      print *,'  tek      Tektronix-Terminal'
      print *,'  lpls     postscript to stdout'
      print *,'  psls     postscript to file pgp.ps landscape'
      print *,' '
      print *,'Or choose one of the official PGPLOT device names:'
      print *,' '
      call pgldev
      return
      end
c----------------------------------------------------------------------
cS
c set devices
      integer function pgp_open(device)
      character device*(*)
cE
      integer ier, pgopen
c 
      if (device.eq.'x11') then
        device='/xserve'
      else if (device.eq.'def') then
        device=' '
      else if (device.eq.'lp') then
        device='-/vps'
      else if (device.eq.'ps') then
        device='pgp.ps/vps'
      else if (device.eq.'tek') then
        device='/tk4100'
      else if (device.eq.'lpls') then
        device='-/ps'
      else if (device.eq.'psls') then
        device='pgp.ps/ps'
      endif
      ier=pgopen(device)
      if (ier.lt.1) stop 'ERROR: unknown device-type'
      pgp_open=ier
      return
      end
c----------------------------------------------------------------------
cS
c set devices
      subroutine pgp_setdevice(device,nxsub,nysub)
      integer nxsub,nysub
      character device*(*)
cE
      integer ier, pgbeg
c 
      if (device.eq.'x11') then
        device='/xserve'
      else if (device.eq.'def') then
        device=' '
      else if (device.eq.'lp') then
        device='-/vps'
      else if (device.eq.'ps') then
        device='pgp.ps/vps'
      else if (device.eq.'tek') then
        device='/tk4100'
      else if (device.eq.'lpls') then
        device='-/ps'
      else if (device.eq.'psls') then
        device='pgp.ps/ps'
      endif
      ier=pgbeg(0,device,nxsub,nysub)
      if (ier.ne.1) stop 'ERROR: unknown device-type'
      return
      end
c----------------------------------------------------------------------
cS
c
c pgp_setenv
c
c just like pgenv but with:
c   lmarg= left margin (0<= lmarg <1)
c   rmarg= right margin
c   tmarg= top margin
c   bmarg= bottom margin
c
      subroutine pgp_setenv(xmin, xmax, ymin, ymax, just, axis,
     &  lmarg, rmarg, tmarg, bmarg)
c
c declare parameters
      real xmin, xmax, ymin, ymax, lmarg, rmarg, tmarg, bmarg
      integer just, axis
c
cE
c declare variables
      INTEGER      L
      LOGICAL      PGNOTO
      CHARACTER*10 XOPTS, YOPTS, ENVOPT, TEMP
C
      IF (PGNOTO('pgp_setenv')) RETURN
c
      call pgpage
      call pgsvp(lmarg, (1-rmarg), bmarg, (1-tmarg))
C
C If invalid arguments are specified, issue warning and leave window
C unchanged.
C
      IF (XMIN.EQ.XMAX) THEN
        CALL GRWARN('invalid x limits in pgp_setenv: XMIN = XMAX.')
        RETURN
      ELSE IF (YMIN.EQ.YMAX) THEN
        CALL GRWARN('invalid y limits in pgp_setenv: YMIN = YMAX.')
        RETURN
      END IF
C
C Call PGSWIN to define the window.
C If equal-scales requested, adjust viewport.
C
      IF (JUST.EQ.1) THEN
        CALL PGWNAD(XMIN,XMAX,YMIN,YMAX)
      ELSE
        CALL PGSWIN(XMIN,XMAX,YMIN,YMAX)
      END IF
C
C Call PGBOX to draw and label frame around viewport.
C
      YOPTS = '*'
      IF (AXIS.EQ.-2) THEN
        XOPTS = ' '
      ELSE IF (AXIS.EQ.-1) THEN
        XOPTS = 'BC'
      ELSE IF (AXIS.EQ.0) THEN
        XOPTS = 'BCNST'
      ELSE IF (AXIS.EQ.1) THEN
        XOPTS = 'ABCNST'
      ELSE IF (AXIS.EQ.2) THEN
        XOPTS = 'ABCGNST'
      ELSE IF (AXIS.EQ.10) THEN
        XOPTS = 'BCNSTL'
        YOPTS = 'BCNST'
      ELSE IF (AXIS.EQ.20) THEN
        XOPTS = 'BCNST'
        YOPTS = 'BCNSTL'
      ELSE IF (AXIS.EQ.30) THEN
        XOPTS = 'BCNSTL'
        YOPTS = 'BCNSTL'
      ELSE
        CALL GRWARN('pgp_setenv: illegal AXIS argument.')
        XOPTS = 'BCNST'
      END IF
      IF (YOPTS.EQ.'*') YOPTS = XOPTS
C
C Additional PGBOX options from PGPLOT_ENVOPT.
C
      CALL GRGENV('ENVOPT', ENVOPT, L)
      IF (L.GT.0 .AND. AXIS.GE.0) THEN
        TEMP = XOPTS
        XOPTS = ENVOPT(1:L)//TEMP
        TEMP = YOPTS
        YOPTS = ENVOPT(1:L)//TEMP
      END IF
      CALL PGBOX(XOPTS, 0.0, 0, YOPTS, 0.0, 0)
      return
      end
c----------------------------------------------------------------------
cS
c
c pgp_grid
c
c plot a grid
c
      subroutine pgp_grid(color)
c declare parameters
      integer color
cE
c delcare variables
      integer colind
c go
      call pgqci(colind)
      call pgsci(color)
      call pgbox('BCGTS',0.0,0,'BCGTS',0.0,0)
      call pgsci(colind)
      return
      end
c----------------------------------------------------------------------
cS
c
c pgp_shade
c
c prepare are colour-shading table
c 
c known type are:
c   rwb 
c   rainbow
c   gray
c 
      subroutine pgp_shade(type, cimin, cimax)
c 
c declare parameters
c 
      character*(*) type
      integer cimin, cimax
cE
c declare variables
      integer ci, cidmin, cidmax, ni, ni2, i
      real cr, cg, cb
      integer cavmin, cavmax
c check capabilities
      call pgqcol(cavmin, cavmax)
      if (cavmax.lt.cimin)
     &  stop 'pgp_shade: ERROR can''t allocate enough colors'
      if (cavmax.lt.cimax) then
        print *,'pgp_shade: WARNING max index requested: ',cimax
        print *,'pgp_shade: WARNING max index available: ',cavmax
      endif
      if (cimin.ge.cimax)
     &stop 'pgp_shade: ERROR max index must be greater than min index'
      cidmin=max(cavmin,cimin)
      cidmax=min(cavmax,cimax)
      ni2=int((cidmax-cidmin+1)/2)
      ni=ni2*2
      cidmax=cidmin+ni-1
      call pgscir(cidmin, cidmax)
c go through different coloring types
      if (type(1:3).eq.'rwb') then
        do i=0, ni2-1
          ci=i+cidmin
          cr=1.
          cg=float(i)/float(ni2)
          cb=cg
          call pgscr(ci,cr,cg,cb)
        enddo
        do i=ni2,ni-1
          ci=i+cidmin
          cr=1.-((float(i)-ni2)/(ni2-1.))
          cg=cr
          cb=1.
          call pgscr(ci,cr,cg,cb)
        enddo
      elseif (type(1:7).eq.'rainbow') then
        do i=0,ni-1
          ci=i+cidmin
          cr=120.+240.*(float(i)/float(ni-1))
          call pgshls(ci,cr,0.5,1.)
        enddo
      elseif (type(1:4).eq.'gray') then
        do i=0,ni-1
          ci=i+cidmin
          cr=float(i)/(float(ni-1))
          call pgscr(ci,cr,cr,cr)
        enddo
      else
        print *,'pgp_shade: WARNING type unknown: ',type
        print *,'pgp_shade: WARNING creating grayscale'
        do i=0,ni-1
          ci=i+cidmin
          cr=float(i)/(float(ni-1))
          call pgscr(ci,cr,cr,cr)
        enddo
      endif
      return
      end
