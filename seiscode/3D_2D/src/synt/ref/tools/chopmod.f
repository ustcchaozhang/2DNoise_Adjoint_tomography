c this is <chopmod.f>
c------------------------------------------------------------------------------
c
c Copyright (C) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c Build flat earth model of discrete homogeneous layers from a
c spherical earth model given by polynomal coefficients.
c Quality factors are transformed from Qkappa and Qmu to Qalpha and Qbeta.
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
c V1.0   13/01/97   Thomas Forbriger
c V1.1   16/01/97   add top halfspace to output model
c V1.2   18/01/97   end of layer stack is indicated by 'halfspace'
c V1.3   29/01/97   no-EFA-modus implemented
c V1.4   31/01/97   removed ugly bug (program got caught in top section)
c                   invented flag insection
c V1.5   09/02/97   introduced velocity reference frequency 
c V1.6   14/02/97   introduced transform of velocties to dispersed velocities
c                   for a dominant period
c V1.7   17/03/99   provide different density transformations
c V1.8   22/12/11   migrated to gfortran
c V1.9   22/12/11   support input of files with transverse isotropic model 
c
      program chopmod
      character*70 version
      parameter(version='CHOPMOD   V1.9   spherical --> flat earth')
c----------------------------------------------------------------------
c 
c STRATEGY
c
c Some words about the strategy how this alogrithm works. As an input
c model it expects a model given in polynomals up to order four. This
c model may be given separated into several sections with one polinomal 
c each. 
c
c The algorithm starts at the top (surface of the earth with
c radius=earthradius and depth z=0). From there it walk down into the
c earth in little steps of given stepsize. All calculations with
c parameters at a certain depth will be done with the transformed
c parameters. This means that the model is first transformed and 
c chopped afterwords! If NO-EFO-mode is selected no transformation
c will be done.
c
c The basic idea of the algorithm is to replace a smooth but finite
c change in parameters by a sharp step, which should have the same effect.
c In the first step of the algorithm we start from a given depth
c and look for a depthrange with acceptable relative parameter change.
c A smaller depth will be taken if the sign of the parameter gradient
c changes or if the bottom of the actual section of the original model
c is reached. This is done for all three parameters (alpha, beta and rho).
c From these three ranges we find the smallest of them to be taken for all.
c In a second step we will search a depth within the found depthrange
c where to place the sharp parameter step to get an unchanged mean
c parameter value over the depthrange. From these three stepdepth we
c select a mean one for the border of two layers of the flat model.
c The next step is to go on with a new depthrange at the bottom of
c the previous one.
c
c After defining all layer boundaries in the described manner (there
c are no parameter values yet). The algorithm goes again through the
c whole set of layers in the flat model and calculates mean parameter
c values for each layer that will be the same as the ones in the 
c original transformed model for this layer depth range.
c
c----------------------------------------------------------------------
c configuration
      double precision stepsize, minradius, scanstep
c functions
      double precision efa_r, efa_z
      integer gemini_layer
c 
      integer densexp
      double precision fexp, fdc
c new model
      integer maxlayer, nlayer, layer
      parameter(maxlayer=1000)
      double precision falpha(maxlayer), fbeta(maxlayer), frho(maxlayer)
      double precision zu(maxlayer), radius(maxlayer), qalpha(maxlayer)
      double precision qbeta(maxlayer)
      real*8 nuref
c gemini
      logical optani
      integer maxsection, nsection, nl
      parameter(maxsection=30)
      double precision alpha(maxsection, 4), beta(maxsection, 4)
      double precision eta(maxlayer, 4)
      double precision alphah(maxsection, 4), betah(maxsection, 4)
      double precision rho(maxsection, 4), qm(maxsection), qk(maxsection)
      double precision rb(0:maxsection), R
      integer iflso(maxsection), nco(maxsection)
      character text*72
c general 
      character*80 filename, outfile
      logical debug, replace, noefa, insection
      integer fin, iargc, lu, isec
      parameter(lu=10)
      double precision rrr
      double precision pi
      parameter (pi=3.14159265358979d0)
c dispersion
      logical disperse
      real*8 domper, q, nureforig
c calculations
      double precision zos, zus, zzo, zzu, zzm, rrm
      double precision zualpha, zubeta, zurho, zsalpha, zsbeta, zsrho
c commandline
      integer maxopt, lastarg 
      parameter(maxopt=10)
      character*2 optid(maxopt)
      character*40 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c here are the keys to our commandline options
      data optid/2h-d,2h-s,2h-r,2h-o,2h-S,2h-n,2h-f,2h-p,2h-N,'-t'/
      data opthasarg/.FALSE.,2*.TRUE.,.FALSE.,.TRUE.,.FALSE.,3*.TRUE.,
     &  .false./
      data optarg/1h-,4h0.02,5h1000.,1h-,2h1.,1h-,2h0.,2h0.,2h-1,'-'/
c----------------------------------------------------------------------
      print *,version
      print *,'Usage: chopmod [-s stepsize] [-r minradius] [-o] [-f freq]'
      print *,'               [-p per] [-S scanning] [-n] [-N n] [-t]'
      print *,'               infile outfile'
      print *,'   or: chopmod -help'
      if (iargc().lt.1) stop 'ERROR: missing parameters'
      call getarg(1, filename)
      if (filename.eq.'-help') then
        print *,' '
        print *,'Transform spherical earth model to flat earth consisting of'
        print *,'discrete homogeneous layers. The EFA (earth flattening'
        print *,'approximation) given by G. Mueller in his tutorial for'
        print *,'the reflectivity method is used. Quality factors are'
        print *,'transformed from elastic moduli to seismic layer'
        print *,'velocities.'
        print *,' '
        print *,'-s stepsize  Gives the maximum size of parameter function'
        print *,'             steps as a fraction of the absolute value.'
        print *,'             Default is 0.02 which means that every step'
        print *,'             caused by discrete homogeneus layering will not'
        print *,'             be larger than 2%'
        print *,'-r minradius Sets the minimum radius for the resulting'
        print *,'             earth model. The flat earth transformation'
        print *,'             does not support parameter function down'
        print *,'             to the earth''s center. Default is a limiting'
        print *,'             radius auf 1000km. Units are km.'
        print *,'-o           Replace existing output file.'
        print *,'-S scanning  Stepsize when scanning earth model.'
        print *,'             Default is 1km. Units are km.'
        print *,'-n           NO-EFA-modus: model will be chopped but no'
        print *,'             earth flattening approximation will be'
        print *,'             applied.'
        print *,'-f freq      The frequency (in Hz) will be included as the'
        print *,'             reference frequency for Q-values and velocities.'
        print *,'             The default is 0.Hz which means values are'
        print *,'             frequency independent.'
        print *,'-p per       If you set a dominant period per (in sec). The'
        print *,'             simple constant-Q velocity dispersion will be'
        print *,'             assumed to transform the model velocities to a'
        print *,'             none dispersive model for this dominant period.'
        print *,'-N n         Provides different density transformations as'
        print *,'             defined by'
        print *,'             G. Mueller, 1977, J. Geophys., 42, 429-436'
        print *,'             preferable values are'
        print *,'             n=-1 for body waves (Mueller)'
        print *,'             n=1  for the acoustic case (Helmberger)'
        print *,'             n=-2 for Rayleigh waves (Biswas)'
        print *,'             n=-5 for Love waves (Biswas and Knopoff)'
        print *,'             (default n=',optarg(9)(1:3),')'
        print *,'-t           earth model has transverse isotropy'
        print *,'             (h-velocities and eta are ignored)'
        print *,' '
        print *,'infile       GEMINI model file for spherical earth'
        print *,'outfile      Output containing model for flat earth'
        print *,' '
        print *,'Program is compiled for:'
        print *,'  maximum number of input model sections: ',maxsection
        print *,'  maximum number of output model layers: ',maxlayer
        stop
      endif
c----------------------------------------------------------------------
c set options
      call tf_cmdline(1, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      debug=optset(1)
      read(optarg(2), *) stepsize
      read(optarg(3), *) minradius
      replace=optset(4)
      read(optarg(5), *) scanstep
      noefa=optset(6)
      read(optarg(7), *) nuref
      disperse=optset(8)
      read(optarg(9), *) densexp
      optani=optset(10)
      if (disperse) then
        if (nuref.le.0.d0) stop 'ERROR: You must set a reference frequency'
        read(optarg(8), *) domper
      endif
      if (scanstep.lt.1.d-2) stop 'ERROR: scanning stepsize too small'
      if (stepsize.lt.1.d-5) stop 'ERROR: stepsize too small'
      if (minradius.lt.1.d0) stop 'ERROR: minimum radius too small'
      if (iargc().le.(lastarg+1)) stop 'ERROR: filenames?'
      if (iargc().gt.(lastarg+2)) 
     &  print *,'WARNING: additional parameters are ignored'
      call getarg((lastarg+1), filename)
      call getarg((lastarg+2), outfile)
      fin=index(filename,' ')
      print *,'stepsize set to: ',stepsize
      print *,'minradius set to: ',minradius,'km'
      print *,'scanning stepsize set to: ',scanstep,'km'
      print *,'refrence frequency is: ',nuref,'Hz'
      print *,'density transformation for: n=',densexp
c----------------------------------------------------------------------
c read file
      if (debug) print *,'DEBUG: filename ',filename(1:fin)
      if (optani) then
        call gemini_getani(filename, lu, maxsection, eta,
     &    rb, qm, qk, rho, alpha, alphah, beta, betah,
     &    nsection, iflso, nco, text)
      else
        call gemini_getmod(filename, lu, maxsection, 
     &    rb, qm, qk, rho, alpha, beta, nsection, iflso, nco, text)
      endif
c      call gemini_getmod(filename, lu, maxsection, 
c     &  rb, qm, qk, rho, alpha, beta, nsection, iflso, nco, text)
      R=rb(nsection)
c----------------------------------------------------------------------
c go through all sections of polynomal model and chop model
      if (debug) print *,'DEBUG: got to chop'
      if (debug) print *,'DEBUG: depth range minz,minr,minrt',
     &  efa_z(R, minradius),minradius,efa_r(R, efa_z(R, minradius))
      nlayer=0
      isec=nsection
      do while (isec.ge.1)
        insection=.true.
c top of section
        if (noefa) then
          zos=R-rb(isec)
        else
          zos=efa_z(R, rb(isec))
        endif
        if (isec.gt.1) then
c bottom of section
          if (noefa) then
            zus=R-rb(isec-1)
          else
            zus=efa_z(R, rb(isec-1))
          endif
        else
c bottom is earth center
          if (noefa) then
            zus=R-minradius
          else
            zus=efa_z(R, minradius)
          endif
        endif
        if (debug) print *,'DEBUG: section, zos,zus,iflso ',
     &    isec,zos,zus,iflso(isec)
        zzu=zos
    1   continue
          nlayer=nlayer+1
          zzo=zzu
          if (nlayer.gt.maxlayer) stop 'ERROR: too many layers'
          if (debug) print *,'DEBUG: layer ',nlayer,' zzo ',zzo
c search max depth step for each parameter
          call nextz(R, zzo, stepsize, zualpha, zus, scanstep, alpha, isec, 
     &      maxsection, .false., debug, noefa, densexp)
          if (iflso(isec).eq.1) then
            zubeta=zualpha
          else
            call nextz(R, zzo, stepsize, zubeta, zus, scanstep, beta, isec, 
     &        maxsection, .false., .false., noefa, densexp)
          endif
          call nextz(R, zzo, stepsize, zurho, zus, scanstep, rho, isec, 
     &      maxsection, .true., .false., noefa, densexp)
          zzu=min(zualpha,zubeta,zurho)
          if (debug) print *,'DEBUG: zualpha,zubeta,zurho,zzu ',
     &      zualpha,zubeta,zurho,zzu
          if (zus.gt.0) then
            if (zzu.gt.zus) insection=.false.
            zzu=min(zzu,zus)
          endif
c get stepdepth
          call stepz(R, zzo, zzu, zsalpha, scanstep, alpha, isec, 
     &      maxsection, .false., debug, noefa, densexp)
          if (iflso(isec).eq.1) then
            zsbeta=zsalpha
            if (debug) print *,'DEBUG: fluid'
          else
            call stepz(R, zzo, zzu, zsbeta, scanstep, beta, isec, 
     &        maxsection, .false., debug, noefa, densexp)
          endif
          call stepz(R, zzo, zzu, zsrho, scanstep, rho, isec, 
     &      maxsection, .true., debug, noefa, densexp)
          zu(nlayer)=(zsalpha+zsbeta+zsrho)/3.d0
          if (debug) 
     &      print *,'DEBUG: zus, zzu, zsalpha, zsbeta, zsrho, zu(nlayer)',
     &      zus, zzu, zsalpha, zsbeta, zsrho, zu(nlayer)
c reached section or radius limit?
        if (noefa) then
          rrr=R-zu(nlayer)
        else
          rrr=efa_r(R, zu(nlayer))
        endif
        if ((insection).and.(rrr.gt.minradius)) goto 1
        nlayer=nlayer+1
        if (nlayer.gt.maxlayer) stop 'ERROR: too many layers'
        if (debug) print *,'DEBUG: section closed zu=zzu, zus ',zzu,zus
        zu(nlayer)=zzu
c reached radius limit?
        if (noefa) then
          rrr=R-zu(nlayer)
        else
          rrr=efa_r(R, zu(nlayer))
        endif
        if (rrr.lt.minradius) isec=1
        isec=isec-1
      enddo
c----------------------------------------------------------------------
c go through all layers and evaluate parameter values
      if (debug) print *,'DEBUG: calc parameters'
      zzu=0.d0
      do layer=1,nlayer
        zzo=zzu
        zzu=zu(layer)
        zzm=(zzu+zzo)/2.d0
        if (noefa) then
          rrm=R-zzm
        else
          rrm=efa_r(R, zzm)
        endif
        nl=gemini_layer(rb, rrm, maxsection, nsection)
        if (noefa) then
          radius(layer)=R-zu(layer)
        else
          radius(layer)=efa_r(R, zu(layer))
        endif
        if (debug) print *,'DEBUG: layer,section,radius,zzu,zzm,rrm ',
     &    layer,nl,radius(layer),zzu,zzm,rrm
c let parameter values be mean values in layer
        call meanv(R, zzo, zzu, falpha(layer), scanstep, alpha, nl, 
     &    maxsection, .false., noefa, densexp)
        call meanv(R, zzo, zzu, fbeta(layer), scanstep, beta, nl, 
     &    maxsection, .false., noefa, densexp)
        call meanv(R, zzo, zzu, frho(layer), scanstep, rho, nl, 
     &    maxsection, .true., noefa, densexp)
c transform quality factors according to Mueller tutorial
c 
c The quality factors are not affected by the EFA as only the quotient
c of beta and alpha is used (which remains the same). This may be understood
c as follows: As the length of the ray-path increases with depth under the
c EFA the wavelength increases at the same time which leeds to same number
c of wavelength in the full path an therefor the same damping under a given
c quality factor.
c
        qbeta(layer)=qm(nl)
        qalpha(layer)=(1.d0/qm(nl))
     &                  *4.d0*fbeta(layer)**2/(3.d0*falpha(layer)**2)+
     &                (1.d0/qk(nl))
     &                  *(1.d0-(4.d0*fbeta(layer)**2/(3.d0*falpha(layer)**2)))
        qalpha(layer)=(1.d0/qalpha(layer))
      enddo
c----------------------------------------------------------------------
c do velocity dispersion
      if (disperse) then
        do layer=1,nlayer
          q=max(100.d0,qalpha(layer))
          falpha(layer)=falpha(layer)*(1.d0-1.d0/pi/q*log(domper*nuref))
          q=max(100.d0,qbeta(layer))
          fbeta(layer)=fbeta(layer)*(1.d0-1.d0/pi/q*log(domper*nuref))
        enddo
        nureforig=nuref
        nuref=0.d0
      endif
c----------------------------------------------------------------------
c open output
      print *,'open output file: ',outfile(1:index(outfile,' ')-1)
      if (replace) then
        open(lu, file=outfile, err=99)
      else
        open(lu, file=outfile, status='new', err=99)
      endif
      write(lu, '(a72)', err=98) text
c----------------------------------------------------------------------
c write model
      if (noefa) then
        write(lu, 55, err=98) 
     &    'number of layers:',nlayer,'earth radius (km):-1*',-R,
     &    'reference frequency (Hz):', nuref
      else
        write(lu, 55, err=98) 
     &    'number of layers:',nlayer,'earth radius (km):',R,
     &    'reference frequency (Hz):', nuref
      endif
      write(lu, 50, err=98) 'Zb','alpha','beta','rho','Qalpha','Qbeta','Rb'
c write top halfspace (atmosphere) needed by refmet.f
      write(lu, 51, err=98) 0.d0, 0.3318d0, 0.d0, 0.0013d0, 1000.d0, -1.d0, R
      do layer=1,nlayer-1
        write(lu, 51, err=98) zu(layer), falpha(layer), fbeta(layer), 
     &                frho(layer), qalpha(layer), qbeta(layer), radius(layer)
      enddo
      write(lu, 54, err=98) 'halfspace:', falpha(nlayer), fbeta(nlayer), 
     &              frho(nlayer), qalpha(nlayer), qbeta(nlayer), radius(nlayer)
c----------------------------------------------------------------------
c output additional comments
      write(lu, '(1x)', err=98)
      write(lu, 52, err=98) 'par','units','description'
      write(lu, '(76(1h-))', err=98) 
      write(lu, 52, err=98) 'Zb','km','bottom-of-layer depth'
      write(lu, 52, err=98) 'alpha','km/s','P-wave layer velocity'
      write(lu, 52, err=98) 'beta','km/s','S-wave layer velocity'
      write(lu, 52, err=98) 'rho','g/cm^3','layer density'
      write(lu, 52, err=98) 'Qalpha',' ','quality factor related to alpha'
      write(lu, 52, err=98) 'Qbeta',' ','quality factor related to beta'
      write(lu, 52, err=98) 'Rb','km','radius corresponding to bottom of layer'
c 
      write(lu, 58) 'source depth','transform. exp.','explosion corr.',
     &              'double-couple corr.'
      call efa_ampfac(R,100.d0,-1,fexp,fdc)
      write(lu, 57, err=98) 100., -1, fexp, fdc
      call efa_ampfac(R,100.d0,densexp,fexp,fdc)
      write(lu, 57, err=98) 100., densexp, fexp, fdc
      call efa_ampfac(R,600.d0,-1,fexp,fdc)
      write(lu, 57, err=98) 600., -1, fexp, fdc
      call efa_ampfac(R,600.d0,densexp,fexp,fdc)
      write(lu, 57, err=98) 600., densexp, fexp, fdc
c 
      write(lu, '(//"This file was generated by:"/a70/)', err=98) version
      write(lu, '("Minimum allowed relative parameter stepsize: "f6.4)',
     &  err=98) stepsize
      write(lu, '("Minimum processed radius: "f8.3"km")', err=98) minradius
      write(lu, '("Scanning stepsize: "f8.3"km")', err=98) scanstep
      write(lu, '(/a)', err=98) 
     &  'A reference frequency of 0Hz means values are frequency independent.'
      if (noefa) then
        write(lu, '(/a)', err=98) 
     &    'No earth flattening approximation was applied!'
      endif
      if (disperse) write(lu, 56, err=98) domper, nureforig
      stop
   50 format(a10,6(1x,a10)/77(1h-))
   51 format(f10.3,2(1x,f10.5),1x,f10.6,3(1x,f10.3))
   52 format(a8,a8,2x,a)
   53 format(18x,a)
   54 format(a10,2(1x,f10.5),1x,f10.6,3(1x,f10.3))
   55 format(/a30,i10/a30,f10.3,a30,f10.3/)
   56 format(/'transformed velocities according to a simples constant-Q',
     &       /'dispersion law to period ',f10.4,'sec for a reference',
     &       /'freuqency of ',f10.5,'Hz')
   57 format(2x,f10.0,'km',i17,f20.5,f22.5)
   58 format(/'amplitude correction factors depending on source depth are',
     &       /2x,a12,a17,a20,a22)
   99 stop 'ERROR: opening output file'
   98 stop 'ERROR: writing output file'
   97 stop 'ERROR: closing output file'
      end
c======================================================================
c subroutines
      subroutine nextz(R, zo, ss, zu, zus, dz, a, nl, maxsection, isdensity, 
     &  debug, noefa, densexp)
c 
c look up lower range border for given parameter
c 
c 29/01/97 implemented zus check
c 
c R: earth radius
c zo: top of range
c ss: desired maximum stepsize factor
c zu: found bottom of range
c zus: bottom of search section
c dz: scanning stepwidth
c a: polynomal coefficients
c nl: number of actual section in spherical model
c maxsection: array dimension
c isdensity: true is parameter should be treated as density
c noefa: true is no Earth Flattening Approximation should be applied
c
      integer maxsection, nl, densexp
      logical isdensity, debug, noefa
      double precision R, zo, ss, zu, dz, a(maxsection, 4), zus
c 
      double precision vo, vu, va
c get reference at top 
      if (debug) print *,'DEBUG: subroutine nextz',r,zo,ss,dz,nl
      if (isdensity) then
        call iefa_den(R, zo, vo, a, nl, maxsection, noefa, densexp)
      else
        call iefa_vel(R, zo, vo, a, nl, maxsection, noefa)
      endif
      zu=zo
      vu=vo
    1 continue
c        if (debug) print *,'DEBUG: zu,dz ',zu,dz
        zu=zu+dz
        va=vu
        if (isdensity) then
          call iefa_den(R, zu, vu, a, nl, maxsection, noefa, densexp)
        else
          call iefa_vel(R, zu, vu, a, nl, maxsection, noefa)
        endif
c        if (debug) print *,'DEBUG: zu,zus,dz,vu ',zu,zus,dz,vu
        if (abs(vu-vo).gt.(ss*vo)) goto 2
        if (zu.gt.(zo+dz)) then
          if ((sign(1.d0,va-vo)*sign(1.d0,vu-va)).lt.0) goto 2
        endif
        if (zu.gt.zus) goto 3
        goto 1
    2 continue
      zu=zu-dz
    3 continue
      if (debug) print *,'DEBUG: set zu to ',zu,' vu,vo: ',vu,vo
      return
      end
c----------------------------------------------------------------------
      subroutine stepz(R, zo, zu, zs, dz, a, nl, maxsection, isdensity, 
     &  debug, noefa, densexp)
c 
c look up step depth for given parameter
c 
c R: earth radius
c zo: top of range
c zu: bottom of range
c zs: stepping depth
c dz: scanning stepwidth
c a: polynomal coefficients
c nl: number of actual section in spherical model
c maxsection: array dimension
c isdensity: true is parameter should be treated as density
c
      integer maxsection, nl, densexp
      logical isdensity, debug, noefa
      double precision R, zo, zs, zu, dz, a(maxsection, 4)
c 
      double precision vo, vu, vm
c get reference at top 
      if (isdensity) then
        call iefa_den(R, zo, vo, a, nl, maxsection, noefa, densexp)
      else
        call iefa_vel(R, zo, vo, a, nl, maxsection, noefa)
      endif
c get reference at bottom 
      if (isdensity) then
        call iefa_den(R, zu, vu, a, nl, maxsection, noefa, densexp)
      else
        call iefa_vel(R, zu, vu, a, nl, maxsection, noefa)
      endif
c get mean value
      call meanv(R, zo, zu, vm, dz, a, nl, maxsection, isdensity, noefa,
     &           densexp)
      if (debug) print *,'DEBUG: stepz   zo, zu, vm ', zo,zu,vm
c a step of size (vu-vo) should lead to the same mean value
      if (vo.eq.vu) then
        zs=(zu+zo)/2.d0
      else
        zs=((vm-vu)*zu+(vo-vm)*zo)/(vo-vu)
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine meanv(R, zo, zu, vm, dz, a, nl, maxsection, isdensity,
     &  noefa, densexp)
c 
c calculate mean parameter value
c 
c R: earth radius
c zo: top of range
c zu: bottom of range
c vm: mean value
c dz: scanning stepwidth
c a: polynomal coefficients
c nl: number of actual section in spherical model
c maxsection: array dimension
c isdensity: true is parameter should be treated as density
c
      integer maxsection, nl, densexp
      logical isdensity, noefa
      double precision R, zo, zu, vm, dz, a(maxsection, 4)
c 
      double precision va, ddz, zz
      integer n, i
c calc mean
      vm=0.d0
      n=int((zu-zo)/dz)
      n=max(2,n)
      ddz=(zu-zo)/dble(n)
      zz=zo-(ddz/2.d0)
      do i=1,n
        zz=zz+ddz
        if (isdensity) then
          call iefa_den(R, zz, va, a, nl, maxsection, noefa, densexp)
        else
          call iefa_vel(R, zz, va, a, nl, maxsection, noefa)
        endif
        vm=vm+va
      enddo
      vm=vm/dble(n)
      return
      end
c----------------------------------------------------------------------
c 
c efa-calling subroutines
c
      subroutine iefa_den(R, z, v, a, nl, maxsections, noefa, n)
c 
      integer maxsections, nl, n
      double precision R, z, v, a(maxsections, 4)
      logical noefa
c 
      double precision x
      integer i
c
      if (noefa) then
        x=1.d0-(z/R)
        v=a(nl, 4)
        do i=3,1,-1
          v=a(nl,i)+x*v
        enddo
      else
        call efa_denn(R, z, n, v, a, nl, maxsections)
c        call efa_den(R, z, v, a, nl, maxsections)
      endif
      return
      end
c----------------------------------------------------------------------
      subroutine iefa_vel(R, z, v, a, nl, maxsections, noefa)
c 
      integer maxsections, nl
      double precision R, z, v, a(maxsections, 4)
      logical noefa
c 
      double precision x
      integer i
c
      if (noefa) then
        x=1.d0-(z/R)
        v=a(nl, 4)
        do i=3,1,-1
          v=a(nl,i)+x*v
        enddo
      else
        call efa_vel(R, z, v, a, nl, maxsections)
      endif
      return
      end

c
c ----- END OF chopmod.f -----
