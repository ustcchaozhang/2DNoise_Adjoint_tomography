c this is <sousou_analysis.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c analysis routines
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
c    18/11/98   V1.0   Thomas Forbriger
c    23/11/98   V1.1   allow stacking to arrival at mid of sread
c    23/02/99   V1.2   a factor of two was missing
c    04/12/09   V1.3   use correct DIN notation for units
c
c==============================================================================
c
      subroutine scanslow
c
c perform first slowness scan (deconvolution)
c
      include 'sousou_dim.inc'
      include 'sousou_options.inc'
      include 'sousou_data.inc'
      include 'sousou_workspace.inc'
c 
      real minslo,maxslo,slo,vel,freq,pi,maxvalue,minoff,maxoff
      complex sum,ime
      integer limit,idx,i,j,k
      parameter(pi=3.1415927,ime=(0.,1.))
c 
      if (opt_verbose.gt.0) then
         print *,' '
         print *,'perform slant stack'
         print *,'  (which is a deconvolution of a nondispersive wave'
         print *,'  correlating samples at different offsets)'
      endif
c 
      midoff=0.
      if (opt_midoff) then
        minoff=roffset(1)
        maxoff=roffset(1)
        do i=1,ntraces
          minoff=min(minoff,roffset(i))
          maxoff=max(maxoff,roffset(i))
        enddo
        midoff=(minoff+maxoff)*0.5
        if (opt_verbose.gt.0) then
          print *,'  minimum offset: ',minoff,'m'
          print *,'  maximum offset: ',maxoff,'m'
          print *,'      mid offset: ',midoff,'m'
        endif
      endif
c 
      minslo=1./opt_maxvel
      maxslo=1./opt_minvel
      limit=nspecsamp/2+1
c 
      do i=1,maxslow
        slo=minslo+(i-1)*(maxslo-minslo)/(maxslow-1)
        slowvalue(i)=slo
        vel=1./slo
        if (opt_verbose.gt.0) 
     &    print *,'vel=',vel,'m/s   slo=',slo,'s/m'
        slowamp(i)=0.
        do j=1,limit
          freq=(j-1)*df
          sum=(0.,0.)
          do k=1,ntraces
            idx=(k-1)*nspecsamp+j
            sum=sum+spectra(idx)*cexp(ime*2.*pi*freq*slo*(roffset(k)-midoff))
          enddo
          idx=(i-1)*nspecsamp+j
          slowspec(idx)=sum
          if ((j.gt.1).and.(j.lt.limit)) then
            idx=i*nspecsamp-j+2
            slowspec(idx)=conjg(sum)
          endif
          slowamp(i)=slowamp(i)+real(sum)*real(sum)+imag(sum)*imag(sum)
        enddo
        slowamp(i)=sqrt(slowamp(i))
      enddo
c 
      slowestim=0.
      maxvalue=0.
      do i=1,maxslow
        slo=minslo+(i-1)*(maxslo-minslo)/(maxslow-1)
        if (slowamp(i).gt.maxvalue) then
          maxvalue=slowamp(i)
          slowestim=slo
          slowestimidx=i
        endif
      enddo
      print *,'optimum: vel=',1./slowestim,'m/s   slo=',slowestim,'s/m'
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine pgslowestim
c
c plot slowness estimate curve
c
      include 'sousou_dim.inc'
      include 'sousou_options.inc'
      include 'sousou_data.inc'
      include 'sousou_workspace.inc'
c 
      integer i
      real maxslo,minslo,maxvalue,slo,vel
      character*80 title
c
      minslo=1./opt_maxvel
      maxslo=1./opt_minvel
      maxvalue=0.
      do i=1,maxslow
        maxvalue=max(maxvalue,slowamp(i))
      enddo
      do i=1,maxslow
        slowamp(i)=slowamp(i)/maxvalue
      enddo
      call pgenv(opt_minvel,opt_maxvel,0.,1.,0,1)
      write(title, 50) 1./slowestim
      call pglab(
     &  'deconvolution velocity of air-coupled sound wave / m s\u-1',
     &  'normalized rms amplitude of correlated signal', title)
      call pgsave
      call pgslw(3)
      do i=1,maxslow
        slo=minslo+(i-1)*(maxslo-minslo)/(maxslow-1)
        vel=1./slo
        velo(i)=vel
      enddo
      call pgsfs(3)
      call pgsci(2)
      velo(maxslow+2)=1./minslo
      velo(maxslow+1)=1./maxslo
      slowamp(maxslow+1)=0.
      slowamp(maxslow+2)=0.
      call pgpoly(maxslow+2,velo,slowamp)
      call pgslw(3)
      call pgline(maxslow,velo,slowamp)
      call pgsci(1)
      call pgslw(1)
      call pgunsa
      call pgupdt
c
      return
   50 format('first velocity estimate: ',f8.3,'m/s')
      end
c 
c----------------------------------------------------------------------
c 
      subroutine sloenvelope
c
c calculate sloenvelope
c 23/02/99 a factor of two was missing
c
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
      include 'sousou_data.inc'
c 
      integer limit,j,i,idx
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'calculate envelope'
      endif
c 
      limit=nspecsamp/2+1
      do i=1,maxslow
        do j=limit+1,nspecsamp
          idx=(i-1)*nspecsamp+j
          slowspec(idx)=(0.,0.)
        enddo
        idx=(i-1)*nspecsamp+1
        call tf_fork(nspecsamp,slowspec(idx),1.)
        do j=1,nspecsamp
          idx=(i-1)*nspecsamp+j
          slowspec(idx)=cmplx(2.d0*abs(slowspec(idx)), 0.)
        enddo
        idx=(i-1)*nspecsamp+1
        call tf_fork(nspecsamp,slowspec(idx),-1.)
      enddo
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine graytrans
c 
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
      include 'sousou_data.inc'
c 
      integer isamp,i,j,idx
      real freq, pi
      complex fact,ime
      parameter(pi=3.1415927,ime=(0.,1.))
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'transform to graymap'
      endif
c 
c shift
      timeshift=dt(1)*nspecsamp/2
      if (opt_midoff) then
        timeshift=0.
      endif
c
      do i=1,maxslow
        do isamp=1,nspecsamp
          freq=df*(isamp-1)
          fact=cexp(-ime*freq*2.*pi*timeshift)
          if (isamp.gt.nspecsamp/2) fact=conjg(fact)
          slowspec((i-1)*nspecsamp+isamp)=
     &      slowspec((i-1)*nspecsamp+isamp)*fact
        enddo
        idx=(i-1)*nspecsamp+1
        call tf_fork(nspecsamp,slowspec(idx),1.)
        do j=1,nspecsamp
          graymap(i,j)=abs(slowspec(idx-1+j))
        enddo
      enddo
c 
      return 
      end
c 
c----------------------------------------------------------------------
c
      subroutine scantau
c 
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
      include 'sousou_data.inc'
c 
      integer row, i
      real maxim, lastval, trapval
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'scan tau'
      endif
c 
      row=slowestimidx
      maxim=graymap(row,1)
      thirdtrap=1
      thirdtrapval=maxim
      do i=1,nspecsamp
        maxim=max(maxim,graymap(row,i))
        if (maxim.gt.thirdtrapval) then
          thirdtrap=i
          thirdtrapval=maxim
        endif
      enddo
      firsttrap=-1
      secondtrap=-1
      trapval=maxim*opt_threshold
      lastval=graymap(row,1)
      do i=1,nspecsamp
        if (graymap(row,i).gt.trapval) then
          if (firsttrap.eq.-1) then
            firsttrap=i
          endif
          if (secondtrap.eq.-1) then
            if (graymap(row,i).le.lastval) then
              secondtrap=i-1
            endif
            lastval=graymap(row,i)
          endif
        endif
      enddo
c 
      firsttrapval=-timeshift+dt(1)*(firsttrap-1)
      secondtrapval=-timeshift+dt(1)*(secondtrap-1)
      thirdtrapval=-timeshift+dt(1)*(thirdtrap-1)
c 
      print *,'  positive edge is at ',firsttrapval,' sec'
      print *,'  first maximum is at ',secondtrapval,' sec'
      print *,'  absolute maximum is at ',thirdtrapval,' sec'
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine pggraymap
c 
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
      include 'sousou_data.inc'
c 
      real tmin,tmax,smin,smax,tr(6),maxvalue,minvalue,tleft,tright
      real slbot,sltop,xcoor,ycoor
      logical replot,hot
      integer i,j,pgband,retval
      character*15 item,value
      character*1 button
      character*75 xlabel
c 
      if (opt_verbose.gt.0) then
        print *,' '
        print *,'plot graymap'
      endif
c 
      tmin=-timeshift
      tmax=tmin+(nspecsamp-1)*dt(1)
      smin=1000./opt_maxvel
      smax=1000./opt_minvel
c 
      tleft=firsttrapval-0.02
      tright=secondtrapval+0.02
      sltop=smax
      slbot=smin
c 
      replot=.true.
      hot=.true.
c
      tr(2)=0.
      tr(3)=(tmax-tmin)/(nspecsamp-1)
      tr(1)=tmin-tr(3)
      tr(6)=0.
      tr(5)=(smax-smin)/(maxslow-1)
      tr(4)=smin-tr(5)
c
      call interpol(smin,smax,tmin,tmax,
     &  slbot,sltop,tleft,tright,tr)
c 
      minvalue=plotmap(1,1)
      maxvalue=plotmap(1,1)
      do i=1,maxplot
        do j=1,maxplot
          minvalue=min(minvalue,plotmap(i,j))
          maxvalue=max(maxvalue,plotmap(i,j))
        enddo
      enddo
c 
      write (xlabel, 50) midoff
c 
      do while (hot)
        hot=opt_pickmode
        if (replot) then
          call pgenv(tleft,tright,slbot,sltop,0,2)
          if (opt_colmap(1:4).eq.'gray') then
            call pggray(plotmap,maxplot,maxplot,1,maxplot,1,
     &        maxplot,maxvalue,minvalue,tr)
c          call pgwedg('RG', 0.3, 3., maxvalue, 
c     &      minvalue, 'shade')
          else
            call pgp_shade(opt_colmap, 5,100)
            call pgimag(plotmap,maxplot,maxplot,1,maxplot,1,
     &        maxplot,maxvalue,minvalue,tr)
          endif
          if (opt_pickmode) then
            call pglab(xlabel,
     &        'deconvolution slowness / s km\u-1','pick your optimum')
          else
            if (opt_usetitle) then
              call pglab(xlabel,
     &          'deconvolution slowness / s km\u-1',opt_title)
            else
              call pglab(xlabel,
     &          'deconvolution slowness / s km\u-1',
     &          'correlation test of air-coupled sound wave')
            endif
          endif
c          call pgsave
          call pgbox('ABCTS',0.,0,'ABCTS',0.,0)
          call pgsls(4)
          call pgbox('G',0.,0,'G',0.,0)
          call pgsls(1)
c          call pgunsa
c          call pgsave
          call pgsci(3)
          call pgslw(3)
          call pgmove(tleft,slowestim*1000.)
          call pgdraw(tright,slowestim*1000.)
          call pgmove(firsttrapval,slbot)
          call pgdraw(firsttrapval,sltop)
          call pgmove(secondtrapval,slbot)
          call pgdraw(secondtrapval,sltop)
          call pgmove(thirdtrapval,slbot)
          call pgdraw(thirdtrapval,sltop)
          call pgsci(1)
          call pgslw(1)
c          call pgunsa
          call pgannot(firsttrapval,slowestim*1000.)
          call pgannot(secondtrapval,slowestim*1000.)
          call pgannot(thirdtrapval,slowestim*1000.)
          call pgupdt
          replot=.false.
        endif
c 
        item='CURSOR'
        call pgqinf(item,value,15)
        if ((value.eq.'YES').and.(opt_pickmode)) then
          retval=pgband(7,0,0.,0.,xcoor,ycoor,button)
          if (retval.ne.1) stop 'ERROR: using cursor'
          if (index('Ap',button).gt.0) then
            call pgannot(xcoor,ycoor)
          elseif (index('XxQq',button).gt.0) then
            hot=.false.
          elseif (index(' D',button).gt.0) then
            replot=.true.
          endif
        else
          hot=.false.
        endif
      enddo
c 
      return
   50 format('apparent arrival time (in seconds) at offset ',f6.1,'m')
      end
c 
c----------------------------------------------------------------------
c
      subroutine pgannot(tau,slo)
c 
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
c
      real tau,slo
c 
      real calslo, vel, sourceestim ,xbox(4),ybox(4)
      character*20 offsetstring
c 
      calslo=0.001*slo
      vel=1./calslo
      sourceestim=-vel*tau+midoff
      print 50,vel,calslo,tau,sourceestim
c 
      write(offsetstring,'(f6.2,1hm)') sourceestim
      call pgqtxt(tau,slo,0.,0.,offsetstring,xbox,ybox)
c      call pgsave
      call pgsci(0)
      call pgpoly(4,xbox,ybox)
      call pgsci(2)
      call pgptxt(tau,slo,0.,0.,offsetstring)
      call pgsci(2)
      call pgslw(6)
      call pgpt1(tau,slo,-20)
      call pgsci(1)
      call pgslw(1)
c      call pgunsa
      call pgupdt
c
      return
   50 format(/'  velocity of sound=',f10.3,'m/s (slowness=',f10.6,'s/m)',/
     &       '  tau=',f10.6,'sec   ==>   source at ',f8.2,'m')
      end
c
c----------------------------------------------------------------------
c
      subroutine interpol(smin,smax,tmin,tmax,
     &  minsl,maxsl,mint,maxt,tr)
c
c as the Sun NewsPrint is too stupid to interpret some of the grayscales
c produced by this code we use smoothing
c
      include 'sousou_options.inc'
      include 'sousou_dim.inc'
      include 'sousou_workspace.inc'
      include 'sousou_data.inc'
c 
      real smin,smax,tmin,tmax,minsl,maxsl,mint,maxt,tr(6)
c 
      real doslo,dotim,dslo,dtim,slo1,slo2,slp,fr1,fr2,frp
      integer rslo,rfre,islo,ifreq
      real val1,val2,val3,val4,value,tf_rectint
      logical newrect
c 
c prepare some values
      doslo=(smax-smin)/float(maxslow-1)
      dotim=(tmax-tmin)/float(nspecsamp-1)
      dslo=(maxsl-minsl)/float(maxplot-1)
      dtim=(maxt-mint)/float(maxplot-1)
c prepare surrounding ractangle
c   index
      rslo=1
c   coordinates
      slo1=smin
      slo2=smin+doslo
        do 12 islo=1,maxplot
          rfre=1
          fr1=tmin
          fr2=tmin+dotim
          newrect=.TRUE.
          do 13 ifreq=1,maxplot
c coordinates of plotpoint
            frp=mint+dtim*(ifreq-1)
            slp=minsl+dslo*(islo-1)
c check rectangle
            do while ((slp.gt.slo2).and.(rslo.lt.maxslow))
              rslo=rslo+1
              slo1=smin+doslo*(rslo-1)
              slo2=smin+doslo*rslo
              newrect=.TRUE.
            enddo
            do while ((frp.gt.fr2).and.(rfre.lt.nspecsamp)) 
              rfre=rfre+1
              fr1=tmin+dotim*(rfre-1)
              fr2=tmin+dotim*rfre
              newrect=.TRUE.
            enddo
c set values
            if (newrect) then
              val1=abs(graymap(rslo,rfre))
              val2=abs(graymap(rslo,rfre+1))
              val3=abs(graymap(rslo+1,rfre+1))
              val4=abs(graymap(rslo+1,rfre))
              newrect=.FALSE.
            endif
c interpolate
            value=tf_rectint(fr1,fr2,slo1,slo2,
     &                val1, val2, val3, val4, frp, slp)
            plotmap(islo, ifreq)=value
   13     continue
   12   continue
c 
        tr(1)=mint-dtim
        tr(3)=dtim
        tr(2)=0.
        tr(4)=minsl-dslo
        tr(5)=dslo
        tr(6)=0.
c
      return
      end
c 
c ----- END OF sousou_analysis.f -----
