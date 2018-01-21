c this is <dispfield.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 2001, 2010 by Thomas Forbriger (IMGF Frankfurt)
c
c dispersed wavefield
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
c    17.12.2010   V1.1   provide online help, adjusted default
c                        parameters
c    27.12.2010   V1.2   use libfapidxx 
c
c==============================================================================
c
      program dispfield
c
      character*(*) version
      parameter(version=
     &  'DISPFIELD   V1.2   create field of harmonic plane waves')
c
      logical last
      character*80 filename, oformat
      integer nmax,i,ntrace,n,j,lu,nexp
      parameter(nmax=10000,lu=10)
      real x(nmax)
      complex cx(nmax),ime
      parameter(ime=(0.,1.))
      real xmin,xmax,cmin,cmax,tmax,fmin,fmax,expo,tap,pi2,dt,r,f
      real hin,df,c,a,fny,m,c0,fwil,fwir,tf_costap
      integer ifmin, ifmax, ifwil, ifwir
      parameter(pi2=2.* 3.1415926535, hin=1.)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=11)
      character*3 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/'-d', '-v', '-x', '-c', '-t', '-n', '-f', '-N', '-T',
     &           '-e', '-ty'/
      data opthasarg/2*.FALSE.,9*.TRUE./
      data optarg/2*'-','1.,50.','70.,4000.','1.5','13','0.,80.', 
     &            '50', '0.2', '0.01', 'sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if (argument(1:6).eq.'-xhelp') then
        call sff_help_formats
      endif
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: dispfield outfile'
        print *,'                 [-x min,max] [-c min,max] [-t T]'
        print *,'                 [-n exp] [-f min,max] [-N n] [-T t]'
        print *,'                 [-e exp] [-ty f]'
        print *,'   or: dispfield -help'
        print *,'   or: dispfield -xhelp'
        if (argument(1:5).ne.'-help') 
     &    stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'create dispersed wavefield'
        print *,' '
        print *,'outfile      output file'
        print *,' '
        print *,'-ty f        select output file format'
        print *,'-v           be verbose'
        print *,'-x min,max   offset range'
        print *,'-c min,max   phase velocity range'
        print *,'-t T         duration of time series'
        print *,'-n exp       number of samples given as a power of two'
        print *,'-f min,max   frequency range'
        print *,'-N n         number of traces'
        print *,'-T t         taper'
        print *,'-e exp       exponent of frequency for dispersion'
        print *,'             i.e. phase velocity increases with a'
        print *,'             given exponent of freqnecy'
        print *,' '
        call sff_help_details
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
      read(optarg(4), *) cmin,cmax
      read(optarg(5), *) tmax
      read(optarg(6), *) nexp
      read(optarg(7), *) fmin,fmax
      read(optarg(8), *) ntrace
      read(optarg(9), *) tap
      read(optarg(10), *) expo
      oformat=optarg(11)
c
c------------------------------------------------------------------------------
c go
      n=2**nexp
      if (n.gt.nmax) stop 'ERROR: too many samples'
      dt=tmax/float(n)
      df=1./tmax
      fny=0.5/dt
c      print *,'n,dt,df,fny:',n,dt,df,fny
      if (fmax.gt.fny) stop 'ERROR: frequency too large'
      m=(cmax-cmin)/(fmin**expo-fmax**expo)
      c0=cmax-m*fmin**expo
      print *,'c0,m,expo: ',c0,m,expo
      fwil=fmin+tap*(fmax-fmin)
      fwir=fmax-tap*(fmax-fmin)
      ifmin=fmin/df+1
      ifmax=fmax/df+1
      ifwil=fwil/df+1
      ifwir=fwir/df+1

      call sff_select_format(oformat, i)
      if (i.ne.0) stop 'ERROR: selecting output format'
      call sff_New(lu, filename, i)
      call sffu_simpleopen(lu, filename)

      do i=1,ntrace
        r=(i-1)*(xmax-xmin)/float(ntrace-1)+xmin
        last=.false.
        if (i.eq.ntrace) last=.true.
c        if (i.eq.2) then
c          print *,'r,ime,pi2,hin:',r,ime,pi2,hin
c        endif

        do j=1,n/2
          f=(j-1)*df
          c=c0+m*f**expo
          a=tf_costap(j,ifmin,ifwil,ifwir,ifmax)
          cx(j)=exp(-ime*pi2*f*r/c)*a
          cx(n-j+1)=conjg(cx(j))
c          if (i.eq.2) then
c            print *,'f,c,a:',f,c,a,cx(j)
c          endif
        enddo

        call tf_fork(n,cx,hin)

        do j=1,n
          x(j)=real(cx(j))
        enddo

        call sffu_simplewrite(lu, last, x, n, dt, r)
      enddo
c
      stop
      end
c
c ----- END OF dispfield.f -----
