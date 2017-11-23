c this is <mop.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1997, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c plot ploynomial model
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
c    03/12/97   V1.0   Thomas Forbriger
c    14/01/99   V1.1   model definition changed (see glq_model.inc)
c                      there is no more halfspace parameter
c    25/01/99   V1.2   some more options and title plotting
c    01/12/99   V1.3   introduced chopping master
c    17/11/10   V1.4   use correct include path
c    12/12/12   V1.5   support color selection
c
c==============================================================================
c
      program mop
c
      character*79 version
      parameter(version='MOP   V1.5   plot polynomial model')
c 
      include '../libs/glq_dim.inc'
      include '../libs/glq_model.inc'
      include '../libs/glq_para.inc'
      include '../libs/glq_pgpara.inc'
c
      integer narg, arg,i
      real csfactor
      logical plotti
      character*100 title
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=12)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug
c options
      character*40 device
      real step
      logical poly
c here are the keys to our commandline options
      data optid/2h-D,2h-d,2h-h,2h-s,2h-p,2h-T,2h-H,2h-l,2h-c,2h-m,
     &           '-a','-b'/
      data opthasarg/.FALSE.,3*.TRUE.,2*.FALSE.,6*.TRUE./
      data optarg/1h-,3hx11,3h40.,3h.01,2*1h-,2h1.,2*1h1,1h2,
     &            '1.,0.,0.','0.,0.,1.'/
c
c------------------------------------------------------------------------------
c basic information
c
      print *,version
c      print *,'Usage: mop filename [-d dev] [-h depth] [-s step] [-p]'
      print *,'Usage: mop [-d dev] [-s step] [-p] [-H cs] [-l lw]'
      print *,'           [-a r,g,b] [-b r,g,b]'
      print *,'           [-c clw] [-T] [-m master] files...'
      print *,'   or: mop -help'
c
      if (iargc().lt.1) stop 'ERROR: missing arguments'
      call getarg(1, argument)
      if (argument(1:5).eq.'-help') then
        print *,' '
        print *,'plot polynomial model'
        print *,' '
        print *,'-d dev         plot device (default: x11)'
c        print *,'-h depth       depth of top of halfspace'
        print *,'-s step        maximum relative step for parameter in'
        print *,'               discrete model'
        print *,'-p             plot polynomial model'
        print *,'-T             plot title telling filename'
        print *,'-H cs          scaling factor for character size'
        print *,'-l lw          line width'
        print *,'-l clw         line width for curves'
        print *,'-a r,g,b       set r,g,b color values for P-wave curves'
        print *,'-b r,g,b       set r,g,b color values for S-wave curves'
        print *,'-m master      define master parameter for chopping'
        print *,'               default is ''',optarg(10)(1:3),''''
        print *,'files...       files to read models from'
        print *,'               each file will be plotted on a separate page'
        print *,' '
        call pgp_showdevices
        print *,' '
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(1, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
c 
      call par_setdefpg
      call par_setdefverb
c 
      device=optarg(2)
c      read(optarg(3), *) depth
      read(optarg(4), *) step
      poly=optset(5)
      plotti=optset(6)
      read(optarg(7), *) csfactor
      read(optarg(8), *) pg_lw
      read(optarg(9), *) pg_clw
      read(optarg(10), *) chop_master
      read(optarg(11), *)  (pg_rgbtable(i,pg_alphacol),i=1,3)
      read(optarg(12), *)  (pg_rgbtable(i,pg_betacol),i=1,3)
c 
      pg_ch=csfactor
      pg_bch=1.6*csfactor
      pg_lch=1.3*csfactor
      pg_alpharcol=pg_alphacol
      pg_betarcol=pg_betacol
      pg_rcolind=pg_colind
c
c------------------------------------------------------------------------------
c go
      print *,'relative parameter step: ',step
c open pgplot
      call pgp_setdevice(device, 1, 1)
      call par_pgapply
      call pgask(.false.)
c 
      call pgscr(0,1.,1.,1.)
      call pgscr(1,0.,0.,0.)
c 
      narg=iargc()
      do arg=lastarg+1,narg
        call getarg(arg, argument)
c read model file
        call mod_read(argument, 1)
c chop it
c      print *,'halfspace depth: ',depth,'m'
c      call par_chop(.true., step, depth)
        call par_chop(.true., step)
        call mod_chop(1)
        if (poly) then
          call pg_mod(1)
        else
          call pg_mod(0)
        endif
c title
        if (plotti) then
          call pg_selvp(13)
          call pgsch(pg_lch)
          write(title, 50) argument(1:index(argument,' ')-1)
          call pgmtxt('T',-1.,0.5,0.5,title)
        endif
        if (arg.lt.narg) call pgpage
      enddo
      call pgend
c
      stop
   50 format('polynomial model from file "',a,1h")
      end
c
c ----- END OF mop.f -----
