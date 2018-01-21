c this is <lazek.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
c
c calculate travel time datasets from refmet model files
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
c    21/11/97   V1.0   Thomas Forbriger
c    15/10/01   V1.1   new tflib call
c
c==============================================================================
c
      character*79 version
      parameter(version=
     &  'LAZEK   V1.1   travel time datasets from refmet models')
c
      integer nlay, mlay, nsamp, model
      parameter(mlay=40)
      double precision z(0:mlay), vel(0:mlay,2)
      double precision rho(0:mlay), nuref, radius
      real qa(0:mlay), qb(0:mlay)
      real ti(mlay), d(mlay), cross(mlay)
      logical visible(mlay)
      integer firstcross(mlay)
c receiver info to read
      integer nrcv, mrcv
      parameter(mrcv=300)
      double precision vred, tli, tre, r(mrcv), phi(mrcv)
c travel time curve
      real t(mrcv), x(mrcv)
      real xcross
c 
      integer lu
      parameter (lu=20)
c 
      character*80 comment, modelfile, filename, aorb, receiverfile
c 
      integer i, j, thislay
      real vref
c commandline
      integer maxopt, lastarg, iargc
      parameter(maxopt=2)
      character*2 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c options
      logical fixedpos, debug
c here are the keys to our commandline options
      data optid/2h-r,2h-d/
      data opthasarg/.TRUE.,.FALSE./
      data optarg/2*1h-/
c 
c go
      print *,version
      print *,'Usage: lazek modelfile filename a|b [-r receiverfile]'
      print *,'   or: lazek -help'
      comment=' '
      if (iargc().eq.1) call getarg(1, comment)
      if (comment(1:6).eq.'-help ') then
        print *,' '
        print *,'modelfile        file to take refmet model from'
        print *,'filename         file to write travel times to'
        print *,'a|b              a=Vp-model   b=Vs-model'
        print *,'-r receiverfile  evaluate travel times at fixed positions'
        print *,'                 (file must have refmet format)'
        print *,' '
        print *,'$Id$'
        stop
      endif
      if (iargc().lt.3) stop 'ERROR: wrong number of arguments'
c 
c first read the commandline
      call tf_cmdline(4, lastarg,
     &     maxopt, optid, optarg, optset, opthasarg)
      fixedpos=optset(1)
      receiverfile=optarg(1)
      debug=optset(2)
c get more command line parameters
      call getarg(1, modelfile)
      call getarg(2, filename)
      call getarg(3, aorb)
c
      if (aorb(1:2).eq.'a ') then
        model=1
      elseif (aorb(1:2).eq.'b ') then
        model=2
      else
        stop 'ERROR: choose a or b for velocity model to take'
      endif
c 
c go model
      call refmet_rmod(modelfile, comment, mlay, 
     &   nuref, radius, nlay,
     &   z, vel(0,1), vel(0,2), rho, qa, qb, 2, .false., 1)
c 
      print *,comment
c
      if (radius.gt.0.d0) stop 'ERROR: only flat models!'
      if (nuref.gt.0.d0) stop 'ERROR: no dispersion!'
c
c calculate layer thickness
      do i=1,nlay-1
        d(i)=z(i+1)-z(i)
      enddo
      d(nlay)=-1.
c 
c calculate intercept times
      ti(1)=0.
      do i=2,nlay
        ti(i)=0.
        do j=1,i-1
          ti(i)=ti(i)+((d(j)/vel(j,model))*
     &                sqrt(1.-(vel(j,model)/vel(i,model))**2))
        enddo
        ti(i)=2.*ti(i)
      enddo
c 
c check refraction visibility
      vref=0.
      do i=1,nlay
        if (vel(i,model).gt.vref) then
          vref=vel(i,model)
          visible(i)=.true.
        else
          visible(i)=.false.
        endif
      enddo
c 
c check whether to take fixed positions or to take real curve
      if (fixedpos) then
        print *,' '
        print *,'use fixed positions'
        call refmet_rrcv(receiverfile, comment, vred, tli, tre,
     &    nrcv, mrcv, r, phi, radius, 2, 1, .false.)
        nsamp=nrcv
        do i=1,nrcv
          x(i)=r(i)
          t(i)=x(i)/vel(1, model)+ti(1)
          do j=2,nlay
            t(i)=min((x(i)/vel(j, model)+ti(j)), t(i))
          enddo
        enddo
      else
c
c select positions by curve crossings
c
      print *,' '
      print *,'find cross-overs'
c find all first crossings
        do i=1,nlay-1
          if (debug) print *,'DEBUG: find cross-overs for layer ',i
          firstcross(i)=-1
          cross(i)=-1.
          do j=i+1,nlay
            if (visible(j)) then
              xcross=((ti(j)-ti(i))/
     &          (1./vel(i,model)-1./vel(j,model)))
              if (debug) print *,'DEBUG: xcross: ',xcross
              if ((firstcross(i).lt.0).or.(cross(i).gt.xcross)) then
                if (debug) print *,'DEBUG: found cross-overs with layer ',j
                cross(i)=xcross
                firstcross(i)=j
              endif
            endif
          enddo
        enddo
c read out all cross-overs
        thislay=1        
        nsamp=0
        x(1)=0.
        do while ((thislay.lt.nlay).and.(firstcross(thislay).gt.0))
          nsamp=nsamp+1
          if (debug) print *,'DEBUG: cross-overs between ',thislay,
     &      firstcross(thislay)
          x(nsamp)=((ti(firstcross(thislay))-ti(thislay))/
     &      (1./vel(thislay,model)-1./vel(firstcross(thislay),model)))
          t(nsamp)=x(nsamp)/vel(thislay, model)+ti(thislay)
          thislay=firstcross(thislay)
        enddo
c append a section of the last refractor
        nsamp=nsamp+1
        if (nsamp.eq.1) then
          x(nsamp)=10.
        else
          x(nsamp)=1.3*x(nsamp-1)
        endif
        t(nsamp)=x(nsamp)/vel(thislay, model)+ti(thislay)
      endif
c 
c write file
      print *,' '
      print *,'open output file ',filename(1:index(filename,' '))
      open(lu, file=filename, err=98)
      write(lu, '(a)') version
      write(lu, '(i5)') nsamp
      write(lu, '(2f12.7)') (x(i), t(i), i=1,nsamp)
      close(lu, err=96)
      print *,'file written...'
c 
      stop
   98 stop 'ERROR: opening travel time file'
   97 stop 'ERROR: writing travel time file'
   96 stop 'ERROR: closing travel time file'
      end
c
c ----- END OF lazek.f -----
