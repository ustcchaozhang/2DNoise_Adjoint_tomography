c this is <phadi.f>
c------------------------------------------------------------------------------
c
c Copyright 2001,2010 by Thomas Forbriger (IMGF Frankfurt)
c
c calculate dispersion relation from phase differences
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
c    22/02/2001   V1.0   Thomas Forbriger
c    23/02/2001   V1.1   seperated phase test
c    26/02/2001   V1.2   activated phase VELOCITY flag
c    09/03/2001   V1.3   extra fit result plot options
c    24/08/2001   V1.4   force positive phase step in phase analysis too
c                        via phthresh
c                        and introduced frequency limit for threshold
c    25/07/2010   V1.5   Zacharias found a bug: There was a parameter
c                        missing in a call to pgpt - I wonder how this
c                        went unnoticed over so many years...
c    27/07/2010   V1.6   correct plot labels
c    30.12.2010   V1.7   implemented libfapidxx
c
c==============================================================================
c
      program phadi
c
      character*(*) version
      parameter(version=
     & 'PHADI   V1.7   calculate dispersion relation from phase differences')
c input dataset
      character*80 filename, informat
      integer maxtraces, totmaxsamples
      parameter(maxtraces=300, totmaxsamples=2000000)
      integer lu, ierr
      parameter(lu=12)
      real data(totmaxsamples)
      integer idata(totmaxsamples)
      equivalence(data,idata)
      real toffset(maxtraces), tracedt(maxtraces), roffset(maxtraces)
      integer innsamples(maxtraces), firstsample(maxtraces)
      integer ntraces
c processing dataset
      integer maxsamples, maxnsamples, nsamples, nfny
      integer maxvalues
      parameter(maxsamples=10000,maxvalues=1000)
      complex spectra(maxsamples, maxtraces)
      integer chain(maxtraces), first
      real x(maxtraces), dt
      real df
      real vx(maxtraces,maxvalues)
      real vy(maxtraces,maxvalues)
      real vel(maxsamples), fre(maxsamples)
c graphics
      character*80 device
      integer idevice, pgp_open
c 
c phasor test
      logical phasor,phasorout
      character*80 phasorfile
      real phasorf1, phasorf2
c
c phase test
      logical phasetest,phaseout,phasetime
      character*80 phasefile
      real phasef1, phasef2
c
c dispersion analysis
      logical dodisan,disanout,disanvelocity,disandothresh
      character*80 disanfile
      real disanf1, disanf2,disanthresh,disanthreshlim
      logical disanreduce,fitplot,errorplot
      real disaneps
      integer disanminn, disannl, disannr
c
      integer i,j,k
c constants
      complex ime
      real pi2,hin
      parameter(hin=1.,pi2=2.*3.14159265358979,ime=(0.,1.))
c commandline
      integer maxopt, lastarg, iargc
      character*80 argument
      parameter(maxopt=23)
      character*3 optid(maxopt)
      character*80 optarg(maxopt)
      logical optset(maxopt), opthasarg(maxopt)
c debugging
      logical debug, verbose
c here are the keys to our commandline options
      data optid/3h-DE, 2h-v, '-P', '-Pf', '-Po', '-d', '-p', '-pf', '-po',
     &           '-pt', '-D', '-Df', '-Do', '-Dv', '-f', '-Dt',
     &           '-DO', '-Dn', '-Dr', '-Dp','-De', '-Dl', '-ty'/
      data opthasarg/3*.FALSE.,3*.TRUE.,.FALSE.,2*.TRUE.,2*.FALSE.,
     &               2*.TRUE.,.FALSE.,5*.TRUE.,2*.FALSE.,2*.true./
      data optarg/3*1h-,'10.,20.','phasor.out','x11','-','10.,20.',
     &           'phase.out',2*'-','10.,20.','disan.out','-','10.,20.',
     &           '0.', '1.e-4', '10', '1,1',2*'-','0.','sff'/
c
c------------------------------------------------------------------------------
c basic information
c
c
      argument=' '
      if (iargc().eq.1) call getarg(1, argument)
      if ((argument(1:5).eq.'-help').or.(iargc().lt.1)) then
        print *,version
        print *,'Usage: phadi datafile [-v] [-DE] [-d device] [-f f1,f2]'
        print *,'                      [-P] [-Pf f1,f2] [-Po file]'
        print *,'                      [-p] [-pf f1,f2] [-po file] [-pt]'
        print *,'                      [-D] [-Df f1,f2] [-Do file] [-Dv]'
        print *,'                      [-Dt val] [-DO eps] [-Dn N]'
        print *,'                      [-Dr Nl,Nr] [-Dp] [-De] [-Dl f]'
        print *,'                      [-ty format]'
        print *,'   or: phadi -help'
        print *,'   or: phadi -xhelp'
        if (argument(1:6).eq.'-xhelp') then 
           call sff_help_details 
           stop 
        endif 
        if (argument(1:5).ne.'-help') stop 'ERROR: wrong number of arguments'
        print *,' '
        print *,'calculate dispersion relation from phase differences'
        print *,' '
        print *,'datafile     SFF seismogram file'
        print *,' '
        print *,'-v           be verbose'
        print *,'-DE          print debuggin output'
        print *,'-d device    PGPLOT output device'
        print *,'-f f1,f2     default frequency interval'
        print *,'-ty format   select file format (see list below)'
        print *,' '
        print *,'-P           execute phasor test'
        print *,'-Pf f1,f2    frequency interval for phasor test'
        print *,'-Po file     output file for phasor walkout'
        print *,' '
        print *,'-p           execute phase analysis'
        print *,'-pf f1,f2    frequency interval for phase analysis'
        print *,'-po file     output file for phase curves'
        print *,'-pt          calculate phase time'
        print *,' '
        print *,'-D           calculate disperion relation'
        print *,'-Df f1,f2    frequency interval for dispersion analysis'
        print *,'-Do file     output file for dispersion curves'
        print *,'-Dv          calculate phase velocity rather than slowness'
        print *,'-Dt val      if selected 2pi will be added to the'
        print *,'             phasor-phase if it is less than val (val'
        print *,'             must be given in fraction of 2pi)'
        print *,'             this also applies to the phase analysis.'
        print *,'-Dl f        apply threshold selected with -Dt only at'
        print *,'             frequencies greater than ''f'' '
        print *,'-DO eps      take away data points from both ends of the'
        print *,'             profile to improve fit quality, until'
        print *,'             quality can not improved by more than eps of'
        print *,'             the rms error'
        print *,'-Dn N        at least N data points must remain for fit'
        print *,'-Dr Nl,Nr    reduce until eps-criterion is fullfilled for'
        print *,'             at least Nl datapoints at small offsets and'
        print *,'             Nr datapoints at large offsets'
        print *,'-Dp          plot fit results'
        print *,'-De          plot fit error'
        print *,' '
        call pgp_showdevices
        print *,' '
        call sff_help_formats
        stop
      endif
c
c------------------------------------------------------------------------------
c read command line arguments
c
      call tf_cmdline(2, lastarg, maxopt, optid,
     &                optarg, optset, opthasarg)
      debug=optset(1)
      verbose=optset(2)

      device=optarg(6)

      read(optarg(15), *) phasorf1, phasorf2
      read(optarg(15), *) phasef1, phasef2
      read(optarg(15), *) disanf1, disanf2

      phasor=optset(3)
      if (optset(4)) read(optarg(4), *) phasorf1, phasorf2
      phasorfile=optarg(5)
      phasorout=optset(5)

      phasetest=optset(7)
      if (optset(8)) read(optarg(8), *) phasef1, phasef2
      phasefile=optarg(9)
      phaseout=optset(9)
      phasetime=optset(10)

      dodisan=optset(11)
      if (optset(12)) read(optarg(12), *) disanf1, disanf2
      disanfile=optarg(13)
      disanout=optset(13)
      disanvelocity=optset(14)
      disandothresh=optset(16)
      read(optarg(16), *) disanthresh
      disanreduce=optset(17)
      read(optarg(17), *) disaneps
      read(optarg(18), *) disanminn
      read(optarg(19), *) disannl, disannr
      fitplot=optset(20)
      errorplot=optset(21)
      read(optarg(22), *) disanthreshlim
      informat=optarg(23)
      
      call getarg(1, filename)
c
c------------------------------------------------------------------------------
c go
      call sff_select_input_format(informat, ierr)
      if (ierr.ne.0) stop 'ERROR: selecting input file format'
      call sffu_simpleread(lu, filename, maxtraces, totmaxsamples, data, idata,
     &    toffset, tracedt, roffset, innsamples, firstsample, ntraces, verbose)
c
c consistency check and copy
      if (ntraces.lt.2) stop 'ERROR: too few traces'
      maxnsamples=innsamples(1)
      dt=tracedt(1)
      do i=1,ntraces-1
        if (abs(1.-tracedt(i)/tracedt(i+1)).gt.1.e-4) then
          print *,'ERROR: trace ',i,' and ',i+1,' have inconsistent'
          print *,'       sampling intervals'
          stop 'can''t handle that!'
        endif
        maxnsamples=max(maxnsamples,innsamples(i+1))
        if (innsamples(i).ne.innsamples(i+1)) then
          print *,'NOTICE: trace ',i,' and ',i+1,' have inconsistent'
          print *,'        numbers of samples'
        endif
      enddo
c
      nsamples=1
      do while (nsamples.lt.maxnsamples)
        nsamples=nsamples*2
      enddo
      if (nsamples.gt.maxsamples) stop 'ERROR: too many samples'
      if (verbose) print *,'going to use ',nsamples,' samples'
      df=1./(float(nsamples)*dt)
      nfny=nsamples/2
c 
      call tf_rchain(roffset, chain, ntraces, first, 1)
      k=first
      do j=1,ntraces
        do i=1,nsamples
          spectra(i,j)=(0.,0.)
        enddo
        do i=1,innsamples(k)
          spectra(i,j)=cmplx(data(i+firstsample(k)-1))
        enddo
        x(j)=roffset(k)
        call tf_fork(nsamples,spectra(1,j),hin)
c normalize spectral coefficients (we are just looking for phases and we
c ignore interference effects)
        do i=1,nfny
          spectra(i,j)=spectra(i,j)/abs(spectra(i,j))
        enddo
        if ((toffset(k)/dt).gt.1.e-4) then
        do i=1,nfny
          spectra(i,j)=exp(-ime*df*pi2*float(i-1)*toffset(k))*spectra(i,j)
        enddo
        endif
        k=chain(k)
      enddo
c
      idevice=pgp_open(device)
      call pgask(.FALSE.)
c
c----------------------------------------------------------------------
c
c phasor test
      if (phasor) call phasortest(maxtraces, maxsamples, ntraces,
     &            nsamples, spectra,
     &            x, df, phasorf1, phasorf2, phasorout, phasorfile,
     &            maxvalues, vx,vy,verbose)
c
c phasor test
      if (phasetest) call phaseanal(maxtraces, maxsamples, ntraces,
     &            nsamples, spectra,
     &            x, df, phasef1, phasef2, phaseout, phasefile,
     &            maxvalues, vx,vy,verbose,phasetime,
     &            disandothresh,disanthresh,disanthreshlim)
c
c dispersion relation
      if (dodisan) call dispersion(maxtraces, maxsamples, ntraces,
     &            nsamples, spectra,
     &            x, df, disanf1, disanf2, disanout, disanfile,
     &            maxvalues, vx,vy,verbose,disanvelocity,vel,fre,
     &            version,disandothresh,disanthresh,disanreduce,
     &            disaneps, disanminn,disannl, disannr,debug,
     &            fitplot, errorplot,disanthreshlim)
c 
      call pgclos
c
      stop
      end
c
c======================================================================
c
c phasor test
      subroutine phasortest(mt,ms,nt,ns,s,x,df,f1,f2,fout,
     &   filename,mv,vx,vy,v)
c
      integer mt,ms,nt,ns,mv
      complex s(ms,mt)
      real x(mt),vx(mt,mv),vy(mt,mv)
      real f1,f2,df
      logical fout,v
      character*(*) filename
c 
      integer if1,if2,nf,i,j,k
      real xmax, ymax, xmin, ymin, dx, pi2
      parameter(pi2=2.*3.1415926535897931)
      complex phasor
      if1=nint(f1/df)
      if2=nint(f2/df)
      nf=if2-if1+1
      if (v) print *,'phasor test from ',f1,'Hz to ',f2,'Hz'
      if (v) print *,'  ',nf,' frequencies'
      if (nf.gt.mv) stop 'ERROR: selected too many values'
c 
c check dx consistency
      dx=x(2)-x(1)
      do i=1,nt-1
        if (abs((x(i+1)-x(i))/dx-1.).gt.1.e-4) then
          print *,'WARNING: inconsistent offset interval'
          print *,'         from trace ',i,' at ',x(i)
          print *,'           to trace ',i+1,' at ',x(i+1)
          print *,'         standard interval: ',dx
        endif
      enddo
c 
c 
      do i=if1,if2
        k=i-if1+1
        do j=1,nt-1
          phasor=s(i,j+1)/s(i,j)
          vx(j,k)=real(phasor)
          vy(j,k)=aimag(phasor)
          if (j.gt.1) then
            vx(j,k)=vx(j,k)+vx(j-1,k)
            vy(j,k)=vy(j,k)+vy(j-1,k)
          endif
        enddo
      enddo
c
      xmax=0.
      ymax=0.
      xmin=0.
      ymin=0.
c 
      do k=1,nf
        do j=1,nt-1
          xmax=max(xmax,vx(j,k))
          ymax=max(ymax,vy(j,k))
          xmin=min(xmin,vx(j,k))
          ymin=min(ymin,vy(j,k))
        enddo
      enddo
c 
      call pgenv(xmin,xmax,ymin,ymax,0,2)
      call pglab('real part','imaginary part','Phasor walkout')
      do i=1,nf
        call pgline(nt-1,vx(1,i),vy(1,i))
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------

c
c phase analysis
      subroutine phaseanal(mt,ms,nt,ns,s,x,df,f1,f2,fout,
     &   filename,mv,vx,vy,v,ftime,dothresh,phthresh,threshlim)
c
      integer mt,ms,nt,ns,mv
      complex s(ms,mt)
      real x(mt),vx(mt,mv),vy(mt,mv)
      real f1,f2,df,phthresh,threshlim
      logical fout,v,ftime,dothresh
      character*(*) filename
c 
      integer if1,if2,nf,i,j,k
      real xmax, ymax, xmin, ymin, pi2, fac
      parameter(pi2=2.*3.1415926535897931)
      complex phasor
      if1=nint(f1/df)
      if2=nint(f2/df)
      nf=if2-if1+1
      if (v) print *,'phase test from ',f1,'Hz to ',f2,'Hz'
      if (v) print *,'  ',nf,' frequencies'
      if (nf.gt.mv) stop 'ERROR: selected too many values'
c 
      do i=if1,if2
        k=i-if1+1
        if (ftime) then
          fac=1./(pi2*float(i-1)*df)
        else
          fac=1./pi2
        endif
        do j=1,nt-1
          phasor=s(i,j+1)/s(i,j)
          vx(j,k)=x(j)
          vy(j,k)=aimag(log(phasor))
          if ((dothresh).and.(vy(j,k).lt.(phthresh*pi2)).and.
     &        (threshlim.lt.(float(i-1)*df))) vy(j,k)=vy(j,k)+pi2
          vy(j,k)=vy(j,k)*fac
          if (j.gt.1) then
            vy(j,k)=vy(j,k)+vy(j-1,k)
          endif
        enddo
      enddo
c 
c
      xmax=0.
      ymax=0.
      xmin=0.
      ymin=0.
c 
      do k=1,nf
        do j=1,nt-1
          xmax=max(xmax,vx(j,k))
          ymax=max(ymax,vy(j,k))
          xmin=min(xmin,vx(j,k))
          ymin=min(ymin,vy(j,k))
        enddo
      enddo
c 
      call pgenv(xmin,xmax,ymin,ymax,0,2)
      if (ftime) then
        call pglab('Offset (m)','Phase traveltime (s)','Phase')
      else
        call pglab('Offset (m)','Phase (2\gp)','Phase')
      endif
      do i=1,nf
        call pgline(nt-1,vx(1,i),vy(1,i))
      enddo
c 
      return
      end

c----------------------------------------------------------------------

c
c dispersion analysis
      subroutine dispersion(mt,ms,nt,ns,s,x,df,f1,f2,fout,
     &   filename,mv,vx,vy,v,fvel,vel,fre,version,dothresh,phthresh,
     &   freduce,eps,minn,nl,nr,dbg,fitplot,errorplot,threshlim)
c
      integer mt,ms,nt,ns,mv
      integer minn,nl,nr
      real eps
      complex s(ms,mt)
      real x(mt),vx(mt,mv),vy(mt,mv),vel(ms),fre(ms)
      real f1,f2,df,phthresh,threshlim
      logical fout,v,fvel,dothresh,freduce,dbg,fitplot,errorplot
      character*(*) filename,version
c 
      integer if1,if2,nf,i,j,k
      real xmax, ymax, xmin, ymin, pi2, fac
      logical hot,redleft
      parameter(pi2=2.*3.1415926535897931)
      complex phasor
      integer ix1,ix2,nx,lu,nred,nrl,nrr
      parameter(lu=10)
      real f0,m,esqr,erms,cc,bb,cb,bc,ab,ac,ermsold
      character*100 title
c 
      real pg_ch
      integer pg_slw,pg_clw,pg_flw,pg_fci
      parameter(pg_ch=1.3,pg_slw=3,pg_clw=4,pg_flw=10,pg_fci=2)
c
      if1=nint(f1/df)
      if2=nint(f2/df)
      nf=if2-if1+1
      if (v) print *,'dispersion analysis from ',f1,'Hz to ',f2,'Hz'
      if (v) print *,'  ',nf,' frequencies'
      if (nf.gt.mv) stop 'ERROR: selected too many values'
c 
      do i=if1,if2
        k=i-if1+1
c calculate phase curve
        fac=1./(pi2*float(i-1)*df)
        do j=1,nt-1
          phasor=s(i,j+1)/s(i,j)
          vx(j,1)=x(j)
          vy(j,1)=aimag(log(phasor))
          if ((dothresh).and.(vy(j,1).lt.(phthresh*pi2)).and.
     &        (threshlim.lt.(float(i-1)*df))) vy(j,1)=vy(j,1)+pi2
          vy(j,1)=vy(j,1)*fac
          if (j.gt.1) then
            vy(j,1)=vy(j,1)+vy(j-1,1)
          endif
        enddo
c calculate fitting ceofficients (linear regression)
        fre(k)=float(i-1)*df
        if (v) print *,'  working on frequency ',float(i-1)*df
        ix1=1
        ix2=nt-1
        hot=.true.
        redleft=.true.
        nred=0
        nrl=nl
        nrr=nr
        do while (hot)
          nx=ix2-ix1+1
c calculate linear parameters f0 and m
          bb=0.
          cc=0.
          cb=0.
          bc=0.
          ab=0.
          ac=0.
          bb=float(nx)
          do j=ix1,ix2
            cc=cc+vx(j,1)**2
            cb=cb+vx(j,1)
            ab=ab+vy(j,1)
            ac=ac+vy(j,1)*vx(j,1)
          enddo
          bc=cb
          f0=(ab*cc-ac*cb)/(bb*cc-cb*bc)
          m=(ac*bb-ab*bc)/(cc*bb-cb*bc)
c calculate error
          esqr=0.
          do j=ix1,ix2
            vy(j,2)=f0+m*vx(j,1)
            esqr=esqr+abs(vy(j,1)-vy(j,2))**2
          enddo
          erms=sqrt(esqr/float(nx))
          if (freduce) then
            if (dbg) print *,'still reducing '
            if (nred.gt.0) then
              if (dbg) print *,'  ermsold: ',ermsold,' erms: ',erms
              if (nx.gt.minn) then
                if (dbg) print *,'  nx: ',nx, ' > minn ',minn
                nred=nred+1
                if (dbg) print *,'  (ermsold/erms-1.): ',(ermsold/erms-1.),
     &                           ' eps: ',eps
                if (dbg) print *,'  redleft: ',redleft,' nrl,nrr: ',nrl,nrr
                if ((ermsold/erms-1.).gt.eps) then
                  if (redleft) then
                    nrl=nl
                  else
                    nrr=nr
                  endif
                else
                  if (redleft) then
                    nrl=nrl-1
                  else
                    nrr=nrr-1
                  endif
                endif
                if (dbg) print *,'  nrl,nrr: ',nrl,nrr,' ix1,ix2: ',ix1,ix2
                if (redleft) then
                  if (nrr.gt.0) then
                    redleft=.false.
                    ix2=ix2-1
                  elseif (nrl.gt.0) then
                    ix1=ix1+1
                  else
                    hot=.false.
                  endif
                else
                  if (nrl.gt.0) then
                    redleft=.true.
                    ix1=ix1+1
                  elseif (nrr.gt.0) then
                    ix2=ix2-1
                  else
                    hot=.false.
                  endif
                endif
                if (dbg) print *,'  redleft: ',redleft,' ix1,ix2: ',ix1,ix2
              else
                hot=.false.
              endif
            endif
            ermsold=erms
            nred=nred+1
          else
            hot=.false.
          endif
        enddo
c tell something about our results
        if (fitplot) then
          call pgsave
          write(title, 51) float(i-1)*df, m*1.e3
          xmax=0.
          ymax=0.
          xmin=0.
          ymin=0.
          do j=ix1,ix2
            ymax=max(ymax,vy(j,1),vy(j,2))
            ymin=min(ymin,vy(j,1),vy(j,2))
          enddo
          do j=1,nt-1
            xmax=max(xmax,vx(j,1))
            ymax=max(ymax,vy(j,1))
            xmin=min(xmin,vx(j,1))
            ymin=min(ymin,vy(j,1))
          enddo
          call pgslw(pg_slw)
          call pgsch(pg_ch)
          call pgenv(xmin,xmax,ymin,ymax,0,1)
          call pgslw(1)
          call pgsls(4)
          call pgbox('BCGTS',0.0,0,'BCGTS',0.0,0)
          call pgslw(pg_slw)
          call pgsls(1)
          call pglab('Offset (m)', 'Phase traveltime (s)', title)
          call pgslw(pg_clw)
          call pgline(nt-1,vx(1,1),vy(1,1))
          call pgsci(pg_fci)
          call pgslw(pg_flw)
          call pgline(nx,vx(ix1,1),vy(ix1,2))
          call pgsci(1)
          call pgunsa
        endif
        if (errorplot) then
          write(title, 52) float(i-1)*df, m*1.e3
          call pgsave
          xmax=0.
          ymax=0.
          xmin=0.
          ymin=0.
          do j=ix1,ix2
            vy(j,2)=(vy(j,2)-f0-m*vx(j,1))*1.e3
          enddo
          do j=1,nt-1
            vy(j,1)=(vy(j,1)-f0-m*vx(j,1))*1.e3
          enddo
          do j=ix1,ix2
            ymax=max(ymax,vy(j,1),vy(j,2))
            ymin=min(ymin,vy(j,1),vy(j,2))
          enddo
          do j=1,nt-1
            xmax=max(xmax,vx(j,1))
            ymax=max(ymax,vy(j,1))
            xmin=min(xmin,vx(j,1))
            ymin=min(ymin,vy(j,1))
          enddo
          call pgslw(pg_slw)
          call pgsch(pg_ch)
          call pgenv(xmin,xmax,ymin,ymax,0,1)
          call pgslw(1)
          call pgsls(4)
          call pgbox('BCGTS',0.0,0,'BCGTS',0.0,0)
          call pgslw(pg_slw)
          call pgsls(1)
          call pglab('Offset (m)', 'phase traveltime residual (ms)', title)
          call pgslw(pg_clw)
          call pgline(nt-1,vx(1,1),vy(1,1))
          call pgsci(pg_fci)
          call pgslw(pg_flw)
          call pgline(nx,vx(ix1,1),vy(ix1,2))
          call pgslw(pg_clw)
          call pgsls(4)
          call pgmove(vx(ix1,1),ymin)
          call pgdraw(vx(ix1,1),ymax)
          call pgmove(vx(ix2,1),ymin)
          call pgdraw(vx(ix2,1),ymax)
          call pgsls(1)
          call pgsci(1)
          call pgunsa
        endif
        if (v) print 50,nx,vx(ix1,1),vx(ix2,1),f0,m,erms
        if (fvel) then
          vel(k)=1.e-6/m
        else
          vel(k)=m
        endif
      enddo
c 
c
      xmax=fre(1)
      ymax=vel(1)*1.e3
      xmin=fre(1)
      ymin=vel(1)*1.e3
c 
      do k=1,nf
        vel(k)=1.e3*vel(k)
        xmax=max(xmax,fre(k))
        ymax=max(ymax,vel(k))
        xmin=min(xmin,fre(k))
        ymin=min(ymin,vel(k))
      enddo
c 
      call pgenv(xmin,xmax,ymin,ymax,0,2)
      if (fvel) then
        call pglab('frequency (Hz)','phase velocity (km/s)','dispersion')
      else
        call pglab('frequency (Hz)','phase slowness (s/km)','dispersion')
      endif
      call pgline(nf,fre,vel)
      call pgpt(nf,fre,vel,-5)
c 
      if (fout) then
        open(lu,file=filename)
        write(lu, *) version
        write(lu, '(i10)') 1
        write(lu, '(i10)') nf
        write(lu, '(2(g10.4,2x))') (vel(k),fre(k),k=1,nf)
        close(lu)
      endif
c 
      return
 50   format('   ',i3,' offsets from ',f6.2,'m to ',f6.2,'m ',
     &       ' f0: ',f8.5,' m: ',f9.5,' erms: ',f7.4) 
 51   format('Anpassung bei ',f6.2,'Hz, Langsamkeit: ',f8.3,'s/km')
 52   format('Anpassungsfehler bei ',f6.2,'Hz, Langsamkeit: ',f8.3,'s/km')
      end
c
c ----- END OF phadi.f -----
