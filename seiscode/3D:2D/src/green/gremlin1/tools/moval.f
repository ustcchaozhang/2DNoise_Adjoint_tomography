c this is <moval.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1999, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c MOdel parameter VALue at given depth
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
c    03/03/99   V1.0   Thomas Forbriger
c    24/03/99   V1.1   tell vp/vs
c    11/02/10   V1.2   added note regarding derivatives
c    17/11/10   V1.3   use correct include path
c
c==============================================================================
c
      program moval
c
      character*79 version
      parameter(version=
     &  'MOVAL   V1.3   MOdel parameter VALue at given depth')
c 
      include '../libs/glq_dim.inc'
c
      logical doeval, doconvert
      real depth, a(glqm_mpol), pval(glqm_mpol, glqm_mpar)
      character*80 infile, outfile
      integer i, pindex, lu
      parameter(lu=10)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=4)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-D, 2h-v, 2h-d, 2h-o/
      data opthasarg/2*.FALSE.,2*.TRUE./
      data optarg/2*1h-,2h1.,9hmoval.out/
c 
c------------------------------------------------------------------------------
c basic information
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.3)) then
        print *,version
        print *,'Usage: moval model   -d depth | -o file'
        print *,'   or: moval -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'MOdel parameter VALue at given depth'
        print *,' '
        print *,'model        name of file containing polynomial model'
        print *,'-d depth     output model coefficients at a given depth'
        print *,'-o file      reconvert to OLD format'
        print *,' '
        print *,'The program returns the value of the parameter as well'
        print *,'as the first and second derivative of the polynomial'
        print *,'defining the variation of the parameters with depth.'
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
      doeval=optset(3)
      doconvert=(optset(4).and.(.not.(doeval)))
      read(optarg(3), *) depth
      outfile=optarg(4)
c
      call getarg(1, infile)
c
c------------------------------------------------------------------------------
c go
c
      call mod_read(infile, 1)
      call mod_follow(1)
      if (doeval) then
        do pindex=1,glqm_mpar
          call mod_eval(1, pindex, depth, a)
          do i=1,glqm_mpol
            pval(i,pindex)=a(i)
          enddo
        enddo
        print 50, depth
        do i=1,glqm_mpol
          print 51, (pval(i,pindex), pindex=1,glqm_mpar)
        enddo
        print 52, 'vp/vs:',pval(1,1)/pval(1,2)
      elseif (doconvert) then
        open(lu, file=outfile, status='new', err=99)
        call mod_writeold(lu, 1, version)
        close(lu,err=98)
      else
        stop 'ERROR: select any action'
      endif
c 
      stop
   50 format('model parameters at ',f6.2,'m depth:'/
     &        t5,'Vp',t20,'Vs',t35,'density',t50,'Qp',t65,'Qs')
   51 format(3(f12.9,3x),2(f12.4,3x))
   52 format(/a12,3x,f12.9)
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: closing output file'
      end
c
c ----- END OF moval.f -----
