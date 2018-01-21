c this is <kette.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c Copyright 2001, 2010 by Thomas Forbriger (IMGF Frankfurt)
c
c Masse-Feder-Kette
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
c    16/11/2001   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program kette
c
      character*(*) version
      parameter(version='KETTE   V1.0   Masse-Feder-Kette')
      character*(*) KETTE_CVS_ID
      parameter(KETTE_CVS_ID='$Id$')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=9)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c 
      real delay,tlen,dt,lowpass,hipass
      integer lpord,hpord
      real pi
      double complex ime
      parameter(pi= 3.14159265358979)
      parameter(ime=(0.d0,1.d0))
c 
      real dom,df,fny,omny,fre,om,lpom,hpom
c 
      integer i,j,k,l
c 
      character*80 parfile, outfile
      integer plu,olu
      parameter (plu=10,olu=11)
      logical last
c
      integer npar,mpar
      parameter(mpar=200)
      double precision m(mpar)
      double precision d(mpar)
      double precision e(mpar)
      double precision dmodulus, emodulus
      double complex modulus(mpar)
      double complex mat(mpar,mpar)
      double complex r(mpar)
      integer ipiv(mpar)
      integer info
c  
      integer nsamp,msamp
      parameter(msamp=4100*16)
      double complex spec(msamp,mpar)
      double complex strans(msamp)
      real data(msamp)
c here are the keys to our commandline options
      data optid/2h-D, 2h-v, 2h-d,2h-T,2h-i,2h-l,2h-o,2h-h,2h-O/
      data opthasarg/2*.FALSE.,7*.TRUE./
      data optarg/2*1h-,2h0.,2h5.,5h1.e-3,3h0.2,1h2,3h1.0,1h2/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.2)) then
        print *,version
        print *,'Usage: kette parfile outfile [-v] [-d t] [-T T] [-i i]'
        print *,'             [-l l] [-o o] [-h h] [-O O]'
        print *,'   or: kette -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'Masse-Feder-Kette'
        print *,' '
        print *,'Calculate response of a mass-spring-chain to delta'
        print *,'impulse force.'
        print *,' '
        print *,'parfile      parameter file'
        print *,'             1st row: N'
        print *,'                      N: number of masses'
        print *,'                      (number of springs: N-1)'
        print *,'             2nd to Nth row: m, D, e'
        print *,'                      m: mass'
        print *,'                      D: natural preiod of mass-spring pair'
        print *,'                         (in s)'
        print *,'                      e: relaxation time (Maxwell body)'
        print *,'                         as a fraction of D'
        print *,'                         (in s)'
        print *,'             N+1st row: m'
        print *,'outfile      SFF data'
        print *,'             N traces representing the displacement of the'
        print *,'             masses in response to a step impulse' 
        print *,'             transfer on mass 1 with amplitude 1Ns'
        print *,' '
        print *,'-d t         delay force pulse by t seconds'
        print *,'             (default: ',optarg(3)(1:5),')'
        print *,'-T T         length of seismograms in seconds'
        print *,'             (default: ',optarg(4)(1:5),')'
        print *,'-i i         sampling interval in seconds'
        print *,'             (default: ',optarg(5)(1:5),')'
        print *,'-l l         lowpass frequency as a fraction of nyquist'
        print *,'             (default: ',optarg(6)(1:5),')'
        print *,'-o o         order of butterworth lowpass'
        print *,'             (default: ',optarg(7)(1:5),')'
        print *,'-h h         highpass period as a fraction of signal length'
        print *,'             (default: ',optarg(6)(1:5),')'
        print *,'-O O         order of butterworth highpass'
        print *,'             (default: ',optarg(7)(1:5),')'
        print *,'-v           be verbose'
        print *,' '
        print *,' '
        print *,KETTE_CVS_ID
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(3, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      read(optarg(3), *) delay
      read(optarg(4), *) tlen
      read(optarg(5), *) dt
      read(optarg(6), *) lowpass
      read(optarg(7), *) lpord
      read(optarg(8), *) hipass
      read(optarg(9), *) hpord

      call getarg(1, parfile)
      call getarg(2, outfile)
c
c------------------------------------------------------------------------------
c go
      open(plu, file=parfile)
      read(plu, *) npar
      if (npar.gt.mpar) stop 'ERROR: too many elements'
      do i=1,npar-1
        read(plu, *) m(i),d(i),e(i)
      enddo
      read(plu, *) m(npar)
      close(plu)
c 
      nsamp=1
      do while ((nsamp*dt).lt.tlen)
        nsamp=nsamp*2
      enddo
      if (nsamp.gt.msamp) stop 'ERROR: too many samples'
c 
      tlen=nsamp*dt
      df=1./tlen
      fny=df*(nsamp/2)
      omny=fny*2.*pi
      dom=df*2.*pi
      lpom=omny*lowpass
      hpom=2.*pi/(hipass*tlen)
c
      do i=1,nsamp
        do k=1,npar
          spec(i,k)=(0.d0,0.d0)
        enddo
      enddo
c 
      do i=2,nsamp/2
        fre=df*(i-1)
        om=2.*pi*fre
c 
        do j=1,npar
c 
          r(j)=(0.d0,0.d0)
          do k=1,npar
            mat(j,k)=(0.d0,0.d0)
          enddo
        enddo
c 
c time delay
        r(1)=exp(-ime*om*delay)
c butterworth lowpass
        do k=1,lpord
          r(1)=r(1)*(-ime)/((om/lpom)-exp(ime*pi*(2.*k-1)/(2.*lpord)))
        enddo
c butterworth hipass
        do k=1,hpord
          r(1)=r(1)*(om/hpom)/((om/hpom)-exp(ime*pi*(2.*k-1)/(2.*hpord)))
        enddo
c 
c build system matrix
        do k=1,npar-1
          dmodulus=m(k)*(2.*pi/d(k))**2
          emodulus=dmodulus*d(k)*e(k)/(2.*pi)
          modulus(k)=1./((1./dmodulus)-(ime/(om*emodulus)))
        enddo
        mat(1,1)=modulus(1)/m(1)
        mat(1,2)=-modulus(1)/m(1)
        if (npar.gt.2) then
          do k=2,npar-1
            mat(k,k-1)=-modulus(k-1)/m(k)
            mat(k,k)=modulus(k-1)/m(k)+modulus(k)/m(k)
            mat(k,k+1)=-modulus(k)/m(k)
          enddo
        endif
        mat(npar,npar-1)=-modulus(npar-1)/m(npar)
        mat(npar,npar)=modulus(npar-1)/m(npar)
        do k=1,npar
          mat(k,k)=mat(k,k)-om**2
        enddo
c 
c solve system
        call zgesv(npar, 1, mat, mpar, ipiv, r, mpar, info)
        if (info.lt.0) then
          print *,'zgesv: argument #',info,' has an illegal value'
          stop 'ERROR'
        endif
        if (info.gt.0) then
          print *,'zgesv: matrix is singular'
          stop 'ERROR'
        endif
c 
c add to spectra
        do k=1,npar
          spec(i,k)=r(k)
        enddo
      enddo
c
      call sffu_simpleopen(olu, outfile)
c calculate time-domain signal and write to output file
      if (verbose) print *,'calculate time series'
      do i=1,npar
        if (verbose) print *,'  mass ',i
        last=.false.
        if (i.eq.npar) last=.true.
        do k=1,nsamp
          strans(k)=(0.d0,0.d0)
        enddo
        do k=1,nsamp/2
          strans(k)=spec(k,i)*sqrt(float(nsamp))*df
        enddo
        do k=2,nsamp/2-1
          strans(nsamp+2-k)=dconjg(strans(k))
        enddo
        call tf_dfork(nsamp,strans,1.d0)
        do k=1,nsamp
          data(k)=sngl(dreal(strans(k)))
        enddo
        call sffu_simplewrite(olu, last, data, nsamp, dt, float(i))
      enddo
c
      stop
      end
c
c ----- END OF kette.f -----
