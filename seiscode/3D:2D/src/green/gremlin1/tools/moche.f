c this is <moche.f>
c------------------------------------------------------------------------------
c ($Id$)
c
c Copyright 2000, 2011 by Thomas Forbriger (IfG Stuttgart)
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
c read a refmet file and calculate viscoelastic rheological parameters
c
c REVISIONS and CHANGES
c    25/05/2000   V1.0   Thomas Forbriger
c    04/06/2000   V1.1   - read tabulated model created by motab
c                        - corrected error in vpoisson
c    16/10/2001   V1.2   wrong deifnition of Qp and Qs in distel!
c    11/02/2003   V1.3   new parameter definition :-)
c
c==============================================================================
c
      program moche
c
      character moche_id*(*)
      parameter (moche_id=
     &  '$Id$')
c
      character*79 version
      parameter(version='MOCHE   V1.3   MOdel CHEck')
c 
      character*80 infile,comment
c 
      integer n, ms
      parameter(ms=200)
      real*8 radius, z(0:ms), alpha(0:ms)
      real*8 beta(0:ms), rho(0:ms), nuref
      real qa(0:ms), qb(0:ms)
      integer cl_vlevel, lev2
c
      double precision phialpha, phibeta
      double complex calpha, cbeta, ckappa, cmmod, cmu
      double precision kappa, mu, phikappa, phimu, Qkappa, Qmu
      double precision vpoisson, mpoisson, rpoisson
      double complex ime
      parameter(ime=(0.d0,1.d0))
      double precision pi
      parameter(pi= 3.14159265358979311599796)
c 
      integer i, lu
      parameter(lu=20)
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=5)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c
      logical motabfile, altdef, olddef
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/2h-d,2h-v,2h-t,2h-D,2h-O/
      data opthasarg/4*.FALSE.,.true./
      data optarg/5*1h-/
c
c------------------------------------------------------------------------------
c basic information
c
c
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: moche infile [-v] [-t] [-D] [-O]'
        print *,'   or: moche -help'
        print *,' '
        if (iargc().lt.1) stop 'ERROR: missing arguments'
        print *,'MOCHE reads a refmet style model and calculates a set'
        print *,'of viscoelastic rheology parameters'
        print *,' '
        print *,'infile       file to read as input model'
        print *,'             the default is refmet format'
        print *,' '
        print *,'-v     be verbose (i.e. explain output)'
        print *,'-d     debug (prints intermediate results)'
        print *,'-t     model file is a table craeted by motab'
        print *,'-D     use alternate definition of complex velocity'
        print *,'-O     use old definition of complex velocity'
        print *,' '
        print *,'Mv is the modulus corresponding to velocity v.'
        print *,'  Mp=lambda+2*mu'
        print *,'  Ms=mu'
        print *,' '
        print *,'The complex velocity'
        print *,'  v=sqrt(Mv/rho)'
        print *,'from the complex modulus Mv'
        print *,' '
        print *,'There are three definitions in this program'//
     &  'relating velocity'
        print *,'Q to the complex velocity. The usual one'//
     &  '(in reflectivity'
        print *,'code), which follows a physical definition of quality,'
        print *,'is the default in this program. Here Qv matches'//
     &  'the quality'
        print *,'of the modulus itself and tabulated velocity is'
        print *,'related to the real part of the modulus.'
        print *,' '
        print *,'  1/Qv=Im(Mv)/Re(Mv)'
        print *,'and'
        print *,'  Mv=real(Mv)*(1+i/Qv)'
        print *,' '
        print *,'And from this'
        print *,'  '
        print *,'  v=c*sqrt(1+i/Qv)'
        print *,'with'
        print *,'  c=sqrt(real(Mv)/rho)'
        print *,' '
        print *,'The alternate definition (to be selected with -O) is'
        print *,'----------------------------------------------------'
        print *,' '
        print *,'  1/Qv=Im(Mv)/Re(Mv)'
        print *,'and'
        print *,'  Mv=abs(Mv)*exp(i*arctan(1/Qv))'
        print *,' '
        print *,'And from this'
        print *,'  '
        print *,'  v=c*exp(i*arctan(1/Qv)/2)'
        print *,'with'
        print *,'  c=sqrt(abs(Mv)/rho)'
        print *,' '
        print *,'The alternate definition (to be selected with -D) is'
        print *,'----------------------------------------------------'
        print *,' '
        print *,'  1/Qv=Imv(v)/Re(v)'
        print *,'which leads to'
        print *,'  v=c*exp(i*arctan(1/Qv))'
        print *,'with'
        print *,'  c=sqrt(abs(Mv)/rho)'
        print *,' '
        call refmet_modinf
        print *,' '
        print *,moche_id
        stop
      endif
c 
      if (iargc().lt.1) stop 'ERROR: wrong number of arguments'
      call getarg(1, infile)
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(2, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)
      motabfile=optset(3)
      altdef=optset(4)
      olddef=optset(5)
c 
      if (DEBUG) print *,'DEBUG: debug is on'
c
c------------------------------------------------------------------------------
c go
c
c read model
      if (motabfile) then
        open(lu, file=infile, status='old', err=99)
        n=0
        do while (n.lt.ms)
          read(lu,*,err=98,end=1)
     &      z(n+1),alpha(n+1),beta(n+1),rho(n+1),qa(n+1),qb(n+1)
          n=n+1
          z(n)=1.e-3*z(n)
        enddo
    1   close(lu,err=97)
      else
        if (verbose) then
          lev2=1
          cl_vlevel=4
        else
          lev2=1
          cl_vlevel=0
        endif
        call refmet_rmod(infile, comment, ms, nuref,
     &    radius, n, z, alpha, beta, rho, qa, qb, 
     &    cl_vlevel, debug, lev2)
        if (nuref.ne.0.d0) 
     &    print *,'NOTICE: reference frequency ',
     &      nuref,' will be ignored'
        if (radius.gt.0.) 
     &    print *,'NOTICE: model has radius ',radius,
     &           ' - will be regarded flat'
      endif
c calc and output
      if (verbose) then
        print 51,'#','z','kappa','phika','Qka','mu','phimu','Qmu',
     &           'vnu','mnu','rnu','ka/mu'
      endif
      do i=1,n
        if ((olddef).or.(altdef)) then
          if (altdef) then
            phibeta=atan2(1.,qb(i))
            phialpha=atan2(1.,qa(i))
          else
            phibeta=atan2(1.,qb(i))/2.
            phialpha=atan2(1.,qa(i))/2.
          endif
          calpha=alpha(i)*exp(ime*phialpha)
          cbeta=beta(i)*exp(ime*phibeta)
          cmu=cbeta*cbeta*rho(i)
          cmmod=calpha*calpha*rho(i)
          ckappa=cmmod-(4.d0/3.d0)*cmu
        else
          cmu=beta(i)**2*rho(i)*(1.d0+ime/qb(i))
          cmmod=alpha(i)**2*rho(i)*(1.d0+ime/qa(i))
          ckappa=cmmod-(4.d0/3.d0)*cmu
        endif
        if (debug) then
          print 53,'alpha',alpha(i),'Qalpha',qa(i),'phialpha',phialpha,
     &             'beta',beta(i),'Qbeta',qb(i),'phibeta',phibeta
          print 54,'calpha',calpha,'cbeta',cbeta,'cmu',cmu,
     &             'cmmod',cmmod,'ckappa',ckappa
        endif
        kappa=abs(ckappa)
        mu=abs(cmu)
        phikappa=atan2(imag(ckappa),real(ckappa))
        phimu=atan2(imag(cmu),real(cmu))
        qkappa=real(ckappa)/imag(ckappa)
        qmu=real(cmu)/imag(cmu)
        vpoisson=(0.5d0*alpha(i)*alpha(i)-beta(i)*beta(i))/
     &                 (alpha(i)*alpha(i)-beta(i)*beta(i))
        mpoisson=(1.5d0*kappa-mu)/(3.d0*kappa+mu)
        rpoisson=(1.5d0*real(ckappa)-real(cmu))/
     &    (3.d0*real(ckappa)+real(cmu))
        print 50, i,1.d3*z(i),
     &            kappa,phikappa*180.d0/pi,qkappa,
     &            mu,phimu*180.d0/pi,qmu,
     &            vpoisson,mpoisson,rpoisson,kappa/mu
      enddo
      if (verbose) then
        print *,' '
        print 52,'#',' ','layer index'
        print 52,'z','[m]','depth of top interface of layer'
        print 52,'kappa','[GPa]','amplitude of complex bulk modulus'
        print 52,'phika','[°]','phase angle of complex bulk modulus'
        print 52,'Qka',' ','Q value of complex bulk modulus'
        print 52,'mu','[GPa]','amplitude of complex shear modulus'
        print 52,'phimu','[°]','phase angle of complex shear modulus'
        print 52,'Qmu',' ','Q value of complex shear modulus'
        print 52,'vnu',' ','poisson ratio calculated from velocities'
        print 52,'mnu',' ','poisson ratio calculated from moduls'
        print 52,'rnu',' ',
     &   'poisson ratio calculated from real part of moduls'
        print 52,'ka/mu',' ','ratio kappa/mu'
      endif
      print *,' '
      if ((altdef).or.(olddef)) then
        if (altdef) then
          print *,'Used 1/Qv=Im(v)/Re(v) for the velocity quality Qv,'
          print *,'where v is the complex velocity v=sqrt(Mv/rho) and'
          print *,'Mv is the complex modulus.'
        else
          print *,'Used 1/Qv=Im(Mv)/Re(Mv) for the velocity quality Qv,'
          print *,'where Mv is the complex modulus and v=sqrt(Mv/rho)'
          print *,'the complex velocity.'
        endif
        print *,'The tabulated real velocity c=abs(v) is the amplitude'
        print *,'of the complex velocity v.'
      else
        print *,'The tabulated real velocity c=sqrt(real(v**2)) is'
        print *,'related to the real part of the complex modulus'
        print *,'Mv=v**2*rho=c**2*rho*(1+i/Qv).'
      endif
c 
      stop
   99 stop 'ERROR: opening input file'
   98 stop 'ERROR: reading input file'
   97 stop 'ERROR: closeing input file'
   50 format(i3,1x,f6.2,2(1x,f6.3,1x,f6.2,1x,f6.1),3(1x,f6.3),1x,f5.2)
   51 format(/'rheology:',/,a3,1x,a6,6(1x,a6),3(1x,a6),1x,a5)
   52 format(1x,a6,1x,a5': ',a)
   53 format((3x,3(1x,a8,':',1x,f7.3,9x)))
   54 format((3x,2(1x,a8,':',1x,'(',f7.2,f7.3,')')))
      end
c
c 
c ----- END OF moche.f -----
