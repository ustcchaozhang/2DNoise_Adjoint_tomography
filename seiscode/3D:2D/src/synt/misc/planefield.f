c this is <planefield.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c 09/02/2001 by Thomas Forbriger (IMGF Frankfurt)
c
c create field of harmonic plane waves
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
c    09/02/2001   V1.0   Thomas Forbriger
c    29/01/2012   V1.1   added online help
c
c
c==============================================================================
c
      program planefield
c
      character*(*) version
      parameter(version='PLANEFIELD   V1.1   create field of harmonic plane waves')
c
      logical last
      character*80 filename
      integer nmax,i,ntrace,n,j,lu
      parameter(nmax=10000,lu=10)
      real x(nmax)
      real xmin,xmax,vel,tmax,fre,pi2,dt,r,t
      parameter(pi2=2.* 3.1415926535)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=8)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-x, 2h-c, 2h-t, 2h-n, 2h-f, 2h-N/
      data opthasarg/2*.FALSE.,6*.TRUE./
      data optarg/2*1h-,'2.,50.','50.','1.','1024','30.', '25'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: planefield file [-d] [-v] [-x min.max]'
        print *,'                  [-c v] [-t max] [-n n] [-f f]'
        print *,'                  [-N N]'
        print *,'   or: planefield -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'create field of harmonic plane waves'
        print *,' '
        print *,'file         name of output file (SFF)'
        print *,'-d           produce debug output'
        print *,'-v           be verbose'
        print *,'-x min,max   set minimum and maximum offset'
        print *,'-c v         use phase velocity v'
        print *,'-t max       produce time series of duration max'
        print *,'-n n         produce time series with n samples'
        print *,'-N N         produce time series for N receivers'
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call getarg(1, filename)
      call tf_cmdline(2, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      read(optarg(3), *) xmin, xmax
      read(optarg(4), *) vel
      read(optarg(5), *) tmax
      read(optarg(6), *) n
      read(optarg(7), *) fre
      read(optarg(8), *) ntrace
c
c------------------------------------------------------------------------------
c go
      dt=tmax/float(n)
      if (n.gt.nmax) stop 'ERROR: too many samples'
      call sffu_simpleopen(lu, filename)
      do i=1,ntrace
        r=(i-1)*(xmax-xmin)/float(ntrace-1)+xmin
        last=.false.
        if (i.eq.ntrace) last=.true.
        do j=1,n
          t=(j-1)*tmax/float(n-1)
          x(j)=sin(pi2*fre*(t-r/vel))
        enddo
        call sffu_simplewrite(lu, last, x, n, dt, r)
      enddo
c
      stop
      end
c
c ----- END OF planefield.f -----
