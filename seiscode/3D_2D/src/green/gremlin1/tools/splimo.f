c this is <splimo.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c Introduce a new section into a polynomial model at a specified depth.
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
c    15/04/98   V1.0   Thomas Forbriger
c    14/01/99   V1.1   model definition changed (see glq_model.inc)
c                      - depth means now bottom of section
c                      - follow flag handling has changed
c                      - parameter value calculation has changed
c    17/11/10   V1.2   use correct include path
c
c==============================================================================
c
      program splimo
c
      character*79 version
      parameter(version='SPLIMO   V1.2   SPLIt MOdel section')
c
      character*80 infile, outfile, sdepth
      real depth
      double precision au1, au2, au3, al1, al2, al3
      double precision zt, zb, zr, zt1, zb1, zr1, zt2, zb2, zr2
      double precision x1, x2
      integer upper, lower
      integer isec, mod_isec, thissec, ipar, ipol
      logical dofollow
c 
      include '../libs/glq_dim.inc'
      include '../libs/glq_model.inc'
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c here are the keys to our commandline options
      data optid/2h-d,2h-f/
      data opthasarg/2*.FALSE./
      data optarg/2*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
      print *,'Usage: splimo infile outfile depth [-f]'
      print *,'   or: splimo -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,
     &   'Introduce a new section into a polynomial model at a specified depth.'
        print *,' '
        print *,'infile       input model'
        print *,'outfile      output model'
        print *,'depth        depth in meter at which a new section should'
        print *,'             be introduced'
        print *,'-f           switch follow mode on'
        print *,'             in follow mode the new section will follow'
        print *,'             its parent section in any case'
        stop
      endif
      if (iargc().lt.3) stop 'ERROR: too few arguments'
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(4, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      dofollow=optset(2)
      if (dofollow) print *,'follow mode'
c 
      call getarg(1,infile)
      call getarg(2,outfile)
      call getarg(3,sdepth)
      read(sdepth, *, err=99, end=99) depth
c
c------------------------------------------------------------------------------
c go
c
      call mod_read(infile, 1)
c 
      print *,'split at ',depth,'m'
      thissec=mod_isec(depth, 1)
      print *,'this is in section ',thissec
      print *,'move sections ',thissec,' to ',glqm_nsec
c space for another section available?
      if (glqm_nsec.eq.glqm_msec) stop 'ERROR: too many sections'
c go and move lower sections
      do isec=glqm_nsec,thissec,-1
        mdepth(isec+1, 1)=mdepth(isec, 1)
        do ipar=1,glqm_mpar        
          glqm_npol(isec+1, ipar)=glqm_npol(isec, ipar)
          glqm_follow(isec+1, ipar)=glqm_follow(isec, ipar)
          do ipol=1,glqm_mpol
            model(ipol, isec+1, ipar, 1)=model(ipol, isec, ipar, 1)
          enddo
        enddo
      enddo
c get another section
      glqm_nsec=glqm_nsec+1
      lower=thissec+1
      upper=thissec
      print *,'calculate new parameters for sections',upper,' and ',lower
c 
c calculate values
c 
c index:
c     1: new upper section (above new interface)
c     2: new lower section (below new interface)
c  none: old section which will be split
c
c  zt: depth of top of section
c  zb: depth of bottom of section
c  zr: depth of reference of section
c 
c  x1: reference coordinate of new upper section relative to old section
c      reference depth
c  x2: reference coordinate of new lower section relative to old section
c      reference depth
c 
      mdepth(upper, 1)=depth
c crop marks for new sections and old section (which is splitted)
      zt2=mdepth(upper, 1)
      zb2=mdepth(lower, 1)
      zb1=zt2
      if (upper.gt.1) then
        zt1=mdepth(upper-1, 1)
      else
        zt1=0.d0
      endif
      zb=zb2
      zt=zt1
      zr=0.5d0*(zt+zb)
      zr1=0.5d0*(zt1+zb1)
      zr2=0.5d0*(zt2+zb2)
      x1=zr1-zr
      x2=zr2-zr
      print *,'old reference depth was ',zr,' m'
      print *,'reference depth of upper section is ',zr1,' m'
      print *,'reference depth of lower section is ',zr2,' m'
      do ipar=1,glqm_mpar
c new coefficients for polynomials relative to new references
        au1=model(1,thissec,ipar,1)+
     &      model(2,thissec,ipar,1)*x1+
     &      model(3,thissec,ipar,1)*x1*x1
        au2=model(2,thissec,ipar,1)+
     &      model(3,thissec,ipar,1)*x1*2.d0
        au3=model(3,thissec,ipar,1)
        al1=model(1,thissec,ipar,1)+
     &      model(2,thissec,ipar,1)*x2+
     &      model(3,thissec,ipar,1)*x2*x2
        al2=model(2,thissec,ipar,1)+
     &      model(3,thissec,ipar,1)*x2*2.d0
        al3=model(3,thissec,ipar,1)
        model(1,upper,ipar,1)=au1
        model(2,upper,ipar,1)=au2
        model(3,upper,ipar,1)=au3
        model(1,lower,ipar,1)=al1
        model(2,lower,ipar,1)=al2
        model(3,lower,ipar,1)=al3
c set or inherit follow flag 
        if (dofollow) then
          glqm_follow(lower, ipar)=.true.
        else
          glqm_follow(lower, ipar)=glqm_follow(upper, ipar)
        endif
      enddo
c 
      call mod_follow(1)
      print *,'finished'
c 
      call mod_save(outfile, 1, .true., version)
c 

c 
      stop
   99 stop 'ERROR: reading depth'
      end
c
c ----- END OF splimo.f -----
