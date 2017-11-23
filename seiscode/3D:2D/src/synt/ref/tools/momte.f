c this is <momte.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
c
c calculate MOMent TEnsor from strike, rake and dip (Aki & Richards Box 4.4)
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
c    16/04/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program momte
c
      character*79 version
      parameter(version='MOMTE   V1.0   calculate MOMent TEnsor from strike, rake and dip (Aki & Richards Box 4.4)')
c
      double precision strike,rake,dip,mxx,mxy,mxz,myy,myz,mzz,pi
      parameter(pi= 3.141592653589793115997963d0)
      character*20 sstrike,srake,sdip
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
      if ((argument(1:5).eq.'-help').or.(iargc().ne.3)) then
        print *,version
        print *,'Usage: momte strike rake dip'
        print *,'   or: momte -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'calculate MOMent TEnsor from strike, rake and dip',
     &          ' (Aki & Richards Box 4.4)'
        print *,' '
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(4, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      call getarg(1, sstrike)
      call getarg(2, srake)
      call getarg(3, sdip)
      read (sstrike, *, err=99) strike
      read (srake, *, err=99) rake
      read (sdip, *, err=99) dip
c
c------------------------------------------------------------------------------
c go
      strike=strike*pi/180.
      rake=rake*pi/180.
      dip=dip*pi/180.
      mxx=-1.*(sin(dip)*cos(rake)*sin(2.*strike)+
     &  sin(2.*dip)*sin(rake)*sin(strike)*sin(strike))
      mxy=sin(dip)*cos(rake)*cos(2.*strike)+
     &  0.5*sin(2.*dip)*sin(rake)*sin(2.*strike)
      mxz=-1.*(cos(dip)*cos(rake)*cos(strike)+
     &  cos(2.*dip)*sin(rake)*sin(strike))
      myy=sin(dip)*cos(rake)*sin(2.*strike)-
     &  sin(2.*dip)*sin(rake)*cos(strike)*cos(strike)
      myz=-1.*(cos(dip)*cos(rake)*sin(strike)-
     &  cos(2.*dip)*sin(rake)*cos(strike))
      mzz=sin(2.*dip)*sin(rake)
c 
      print 50,strike*180./pi,rake*180./pi,dip*180./pi
      print 51,mxx,mxy,mxz
      print 51,mxy,myy,myz
      print 51,mxz,myz,mzz
c
      stop
   50 format('strike: ',f8.4,'°'/,
     &       'rake:   ',f8.4,'°'/,
     &       'dip:    ',f8.4,'°'/,
     &       /'tensor:')
   51 format(3(2x,f8.4))
   99 stop 'ERROR: commandline argument'
      end
c
c ----- END OF momte.f -----
