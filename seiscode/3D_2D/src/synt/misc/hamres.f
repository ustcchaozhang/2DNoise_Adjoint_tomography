c this is <hamres.f>
c------------------------------------------------------------------------------
c   ($Id$)
c
c 25/10/2001 by Thomas Forbriger (IMGF Frankfurt)
c
c HAMmer RESponse
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
c    25/10/2001   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program hamres
c
      character*(*) version
      parameter(version='HAMRES   V1.0   HAMmer RESponse')
      character*(*) HAMRES_CVS_ID
      parameter(HAMRES_CVS_ID='$Id$')
c
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=14)
      character*4 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c options
      logical overwrite, nod1, nod2, noe, nodelta
      double precision m,d1,d2,e,i,tlen,dt,ood2
      integer nexpo
      character*80 filename
c calculation
      integer nsamples, maxsamples
      parameter(maxsamples=100000)
      double complex force(maxsamples)
      double complex response(maxsamples), ime, msls
      real data(maxsamples)
      double precision pi, totime, tofourier
      parameter(pi=3.1415926535897d0)
      parameter(totime=1.d0,tofourier=-1.d0, ime=(0.d0,1.d0))
      double precision df,t,f,f0
      integer lu, k
      parameter(lu=10)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d, 2h-v, 2h-o, 2h-I, 3h-D1, 3h-D2, 2h-e, 2h-m,
     &           4h-nD1, 4h-nD2, 3h-ne, 2h-T, 2h-n, 2h-d/
      data opthasarg/3*.FALSE.,5*.TRUE.,3*.FALSE.,3*.TRUE./
      data optarg/3*1h-,4h112.,4h5.e6,4h2.e7,4h2.e4,3h30.,3*1h-,
     &            2h1.,1h9,5h2.e-4/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: hamres filename [-o] [-v]'
        print *,'              [-I I] [-D1 D1] [-D2 D2] [-e e] [-m m]'
        print *,'              [-T T] [-nD1] [-nD2] [-ne] [-n n] [-d d]'
        print *,'   or: hamres -help'
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'HAMmer RESponse'
        print *,' '
        print *,'Calculate transient force transmission of a mass'
        print *,'suspended by a standard linear solid and excited by'
        print *,'a defined force.'
        print *,' '
        print *,'The output signal is in Newton.'
        print *,' '
        print *,'filename     name of SFF output file'
        print *,' '
        print *,'-o           overwrite existing output file'
        print *,'-v           be verbose'
        print *,'-I I         impulse transfer in Ns'
        print *,'             (default value is ',optarg(4)(1:4),'Ns)'
        print *,'             A value of 112Ns is appropriate for a'
        print *,'             free falling 8kg sledge-hammer from a hight'
        print *,'             of 2.5m and purely elastic impulse transfer.'
        print *,'-D1 D1       modulus of parallel spring D1 in N/m'
        print *,'             (default value is ',optarg(5)(1:4),'N/m)'
        print *,'-D2 D2       modulus of serial spring D2 in N/m'
        print *,'             (default value is ',optarg(6)(1:4),'N/m)'
        print *,'-e e         modulus of dashpot e in Ns/m'
        print *,'             (default value is ',optarg(7)(1:4),'Ns/m)'
        print *,'-m m         mass in kg'
        print *,'             (default value is ',optarg(8)(1:4),'kg)'
        print *,'             A mass of 30kg is that of a box of 30cm x'
        print *,'             30cm x 20cm filled with a material of'
        print *,'             1.8 g/ccm mass dnesity.'
        print *,'-T T         duration of exciting force pulse in s'
        print *,'             (default value is delta pulse)'
        print *,'-n n         set number of sample to 2**n'
        print *,'             (default value is ',optarg(13)(1:4),')'
        print *,'-d d         set sampling interval to d s'
        print *,'             (default value is ',optarg(14)(1:5),'s)'
        print *,'-nD1         switch of spring D1 (D1=0)'
        print *,'-nD2         switch of spring D2 (1/D2=0)'
        print *,'-ne          switch of dashpot e (e=0)'
        print *,' '
        print *,HAMRES_CVS_ID
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
      overwrite=optset(3)
      read(optarg(4), *) i
      read(optarg(5), *) d1
      read(optarg(6), *) d2
      read(optarg(7), *) e
      read(optarg(8), *) m
      nod1=optset(9)
      nod2=optset(10)
      noe=optset(11)
      read(optarg(12), *) tlen
      nodelta=optset(12)
      read(optarg(13), *) nexpo
      read(optarg(14), *) dt

      ood2=1./d2
      if (nod2) ood2=0.
      if (nod1) d1=0.
      if (noe) e=0.

      nsamples=2**nexpo
      if (nsamples.gt.maxsamples) stop 'ERROR: too many samples'
      if (verbose) print *,'create ',nsamples,' samples'
      df=1.d0/(dt*float(nsamples))

      if (verbose) then
        print 52,'spring D1 modulus: ',d1,'N/m'
        if (nod2) then
          print 51,'spring 1/D2 modulus: ','infinity'
        else
          print 52,'spring D2 modulus: ',d2,'m/N'
        endif
        print 52,'dashpot e modulus: ',e,'Ns/m'
        print 50,'mass m: ',m,'kg'
        print 50,'stress relaxation time:',e*ood2,'s'
        if (nod1) then
          print 51,'strain relaxation time:','infinity'
          print 51,'SLS relaxation time:','infinity'
        else
          print 50,'strain relaxation time:',e*(ood2+1./d1),'s'
          print 50,'SLS relaxation time:',
     &              e*sqrt(ood2*(ood2+1./d1)),'s'
        endif
        if (nod2) then
          print 50,'system period:',0.,'s'
        else
          print 50,'system period:',sqrt(m/(d1+d2))*2.*pi,'s'
        endif
      endif

c
c------------------------------------------------------------------------------
c go
c
c calculate force spectrum
      if (nodelta) then
        if (verbose) print *,'create force pulse of ',tlen,'s'
        do k=1,nsamples
          t=(k-1)*dt
          if (t.lt.tlen) then
            force(k)=sin(pi*t/tlen)*sqrt(float(nsamples))*dt
          else
            force(k)=(0.d0,0.d0)
          endif
        enddo
        call tf_dfork(nsamples, force, tofourier)
        f0=i*pi*0.5/tlen
      else
        if (verbose) print *,'create delta force impulse'
        do k=1,nsamples
          force(k)=(1.d0,0.d0)
        enddo
        f0=i
      endif

c calculate modulus spectrum
      if (verbose) print *,'create response spectrum'
      if (nod1) then
        do k=1,nsamples/2
          f=(k-1)*df
          response(k)=force(k)*ime*e/
     &                (ime*e-2.*pi*f*m*(1.+ime*2.*pi*f*e*ood2))
          response(k)=response(k)/(sqrt(float(nsamples))*dt)
        enddo
      else
        do k=1,nsamples/2+1
          f=(k-1)*df
          msls=(d1+ime*2.*pi*f*e*(1.+d1*ood2))/
     &         (1.+ime*2.*pi*f*e*ood2)         
          response(k)=force(k)*f0*msls/(msls-m*(2.*pi*f)**2)
          response(k)=response(k)/(sqrt(float(nsamples))*dt)
        enddo
      endif
      do k=2,nsamples/2-1
        response(nsamples+2-k)=conjg(response(k))
      enddo

c extract time series
      if (verbose) print *,'create response time series'
      call tf_dfork(nsamples,response,totime)
      do k=1,nsamples
        data(k)=real(response(k))
      enddo

      if (overwrite) then
        call sff_New(lu,filename,k)
        if (k.ne.0) stop 'ERROR: deleting output file'
      endif
      call sffu_simpleopen(lu,filename)
      call sffu_simplewrite(lu, .true., data, nsamples, sngl(dt), 0.)

      stop
   50 format(a30,1x,f12.3,1x,a)
   51 format(a30,1x,a)
   52 format(a30,1x,g12.3,1x,a)
      end
c
c ----- END OF hamres.f -----
