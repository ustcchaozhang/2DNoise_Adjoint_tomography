c this is <refract_domodel.f>
c------------------------------------------------------------------------------
c
c 05/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c some routines we need to work on models
c
c ----
c refract is free software; you can redistribute it and/or modify
c it under the terms of the GNU General Public License as published by
c the Free Software Foundation; either version 2 of the License, or
c (at your option) any later version. 
c 
c refract is distributed in the hope that it will be useful,
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
c    05/07/98   V1.0   Thomas Forbriger
c    24/05/00   V1.1   - subdivided model reading
c                      - allow line width setting for synthetic traveltimes
c    22/01/01   V1.2   plot correct onsets for direct wave
c    17/11/10   V1.3   simplified input format for model reading
c
c==============================================================================
c
      subroutine evalmod
c
c this one just calculates a model from a traveltime curve
c
      include 'refract_picks.inc'
      include 'refract_model.inc'
c
c declare variables
c   
      integer i,j
      real laytravel, tottravel, thickbase, depth
c
      mod_valid=.false.
c check range
      if (pick_n(1).lt.2) then
        call refract_warning(
     &    'WARNING (evalmod): nothing picked or just halfspace')
      else
c 
        mod_nlay=pick_n(1)
        if (mod_nlay.gt.mod_maxlay) then
          mod_nlay=mod_maxlay
          call refract_warning(
     &      'WARNING (evalmod): too many picks... using maximum number of layers')
        endif
c go through all layers
        do i=1,mod_nlay
c evaluate thickness if we got the second slowness
          if (i.gt.1) then
c first evaluate layer velocities
            mod_slo(i)=(pick_x(i,1)-pick_x(i-1,1))/(pick_y(i,1)-pick_y(i-1,1))
c check picks
            if (mod_slo(i).ge.mod_slo(i-1)) then
              call refract_warning(
     &          'WARNING (evalmod): invalid picks - velocity must be increasing')
              mod_valid=.false.
              return
            endif
c now go for thickness
            tottravel=0.
            thickbase=pick_x(i,1)-(pick_y(i,1)*mod_slo(i))
c it's easy if it's the first layer
            if (i.gt.2) then
              do j=1,i-2
                laytravel=sqrt((mod_slo(j)**2)-(mod_slo(i)**2))*mod_thick(j)
                tottravel=tottravel+laytravel
              enddo   
              tottravel=2.*tottravel
            endif
            laytravel=sqrt((mod_slo(i-1)**2)-(mod_slo(i)**2))*2.
            mod_thick(i-1)=(thickbase-tottravel)/laytravel
          else
c ok it's the first
            mod_slo(i)=pick_x(i,1)/pick_y(i,1)
          endif
        enddo
        mod_valid=.true.
c print result
        print *,'resulting model: (units are meters and seconds)'
        print *,'layer thickness     depth  velocity slowness'
    3   format(i5,3(f10.3),f10.7)
    4   format(i5,10x,2(f10.3),f10.7)
        depth=0.
        do 5 i=1,mod_nlay-1
          print 3,i,mod_thick(i),depth,1/mod_slo(i),mod_slo(i)
          depth=depth+mod_thick(i)
    5   continue
        print 4,mod_nlay,depth,1/mod_slo(mod_nlay),mod_slo(mod_nlay)
      endif
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine pgtraveltime
c
c plot synthetic traveltimes
c
      include 'refract_dim.inc'
      include 'refract_seipar.inc'
      include 'refract_model.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
c
c declare variables
c
      integer i,j,k
      real t, ttot, x, xtot
      double precision sqalpha
      double precision therange
      logical rangeok
      real redtime, redx, redy,realtmax
c
      therange=1.
      sqalpha=1.
c
      if (.not.((mod_valid).and.(elem_syntt))) return
c save settings
      call pgsave
c new settings
      call pgslw(pg_syntt_lw)
c 
      if (plflag_ttstyle) call refract_selstyle(1)
c plot direct wave
      call pgmove(0.,0.)
      realtmax=tov_rmax*mod_slo(1)
      redx=redtime(realtmax,tov_rmax)
      call pgdraw(redx,tov_rmax)
c plot refracted onsets
      do i=1,mod_nlay-1
        ttot=0.
        xtot=0.
        do j=1,i
          t=mod_thick(j)*2*mod_slo(j)/
     &      sqrt(1-((mod_slo(i+1)**2)/(mod_slo(j)**2)))
          x=2*mod_thick(j)/sqrt(((mod_slo(j)**2)/(mod_slo(i+1)**2))-1)
          ttot=ttot+t
          xtot=xtot+x
        enddo   
        if (plflag_ttstyle) call refract_selstyle(i+1)
        redx=redtime(ttot, xtot)
        call pgmove(redx,xtot)
        redy=xtot+(realtmax-ttot)/mod_slo(i+1)
        redx=redtime(realtmax,redy)
        call pgdraw(redx,redy)
      enddo   
c now reflected at bottom of layer i
      do i=1,mod_nlay-1
        x=0.
        t=0.
        do j=1,i
          t=t+2*mod_thick(j)*mod_slo(j)
        enddo   
        if (plflag_ttstyle) call refract_selstyle(i+1)
        redx=redtime(t,x)
        call pgmove(redx,x)
c first off all check for a good alpha-range
        rangeok=.FALSE.
        therange=1.
c do this as often as range is not ok
        do while (.not.(rangeok))
          j=1
          xtot=0.
          ttot=0.
          do while ((xtot.lt.tov_rmax).and.(ttot.lt.realtmax).and.(j.lt.100))
            sqalpha=float(j)*therange/100.
            xtot=0.
            ttot=0.
c go through all layers
            do k=1,i
              x=sngl(2*mod_thick(k)*
     &           sqrt(1./(((mod_slo(k)**2)/(sqalpha*mod_slo(1)**2))-1.)))
              t=sngl(2*mod_thick(k)*mod_slo(k)*
     &           sqrt(1./(1.-((sqalpha*mod_slo(1)**2)/(mod_slo(k)**2)))))
              xtot=xtot+x
              ttot=ttot+t
            enddo   
            j=j+1
          enddo
          therange=sqalpha
          if (j.gt.80) rangeok=.TRUE.
        enddo
c go through all ray-angles
        do j=1,100
          sqalpha=float(j)*therange/100.
          xtot=0.
          ttot=0.
c go through all layers
          do k=1,i
            x=sngl(2*mod_thick(k)*
     &         sqrt(1./(((mod_slo(k)**2)/(sqalpha*mod_slo(1)**2))-1.)))
            t=sngl(2*mod_thick(k)*mod_slo(k)*
     &         sqrt(1./(1.-((sqalpha*mod_slo(1)**2)/(mod_slo(k)**2)))))
            xtot=xtot+x
            ttot=ttot+t
          enddo   
          redx=redtime(ttot,xtot)
          call pgdraw(redx, xtot)
        enddo   
      enddo   
c restore old setting
      call pgunsa
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine modelbox
c 
c declare parameters
c
      include 'refract_model.inc'
      include 'refract_dim.inc'
      include 'refract_seipar.inc'
      include 'refract_para.inc'
c
c declare variables
c
      real xbox(5), ybox(5)
      character*20 head(4), inf, field
      real width(4), height, wi, lineheight
      real xrange, yrange
      parameter(inf='  infinity')
      real charsize, value, depth, fac, xcor, ycor
      integer i,j,colind, nc
      integer headlen(4)
      data head/'layer','depth','thickness','velocity'/
      data headlen/5,5,9,8/
c
      if (.not.((mod_valid).and.(elem_modbox))) return
c go
c keep character size
      call pgqch(charsize)
c 
c evaluate size of box and strings
c
      xrange=tov_tmax-tov_tmin
      yrange=tov_rmax-tov_rmin
      call pgsch(1.)
c set dimensions of column width to array width
      do 1 i=1,4
        field=head(i)
        call pglen(4, field(1:headlen(i)), wi, height) 
        width(i)=wi/xrange
        depth=0.
        do 2 j=1,mod_nlay
          if (i.eq.1) then
            value=float(j)
          elseif (i.eq.2) then
            value=depth
          elseif (i.eq.3) then
            if (j.eq.mod_nlay) then
              value=0.
            else
              value=mod_thick(j)
            endif
          else
            value=1/mod_slo(j)
          endif
          write(field, '(f10.3)') value
          nc=int(log10(max(1.,value))+5.)
          if (i.eq.1) then
            write(field, '(i10)') int(value)
            nc=int(log10(max(1.,value))+1.)
          endif
          if ((j.eq.mod_nlay).and.(i.eq.3)) then
            field=inf
            nc=8
          endif
          call pglen(4, field((10-nc):10), wi, height)
          width(i)=max(width(i),(wi/xrange))
          if (j.lt.mod_nlay) depth=depth+mod_thick(j)
    2   continue
    1 continue
c get total height plus 1/10 spaces
      lineheight=1./40.
      height=lineheight*(float(mod_nlay)+1)*1.1
c get total width plus 1/10 spaces
      wi=0
      do 3 i=1,4
        wi=wi+width(i)
    3 continue
      wi=wi*1.1
c now use factor to reduce size to half of plane
      fac=max(wi,height)
      if (fac.lt.0.5) then
        fac=1.
      else
        fac=0.5/fac
      endif
c now reduce values
      wi=wi*fac
      height=height*fac
      lineheight=lineheight*fac
      call pgsch(fac)
c
c plotbox
c
c first create box
      xbox(1)=mod_boxx
      xbox(2)=mod_boxx
      xbox(3)=mod_boxx+xrange*wi
      xbox(4)=mod_boxx+xrange*wi
      xbox(5)=mod_boxx
      ybox(1)=mod_boxy
      ybox(2)=mod_boxy-yrange*height
      ybox(3)=mod_boxy-yrange*height
      ybox(4)=mod_boxy
      ybox(5)=mod_boxy
c now plot box
      call pgqci(colind)
      call pgsci(0)
      call pgpoly(4,xbox,ybox)
      call pgsci(colind)
      call pgline(5,xbox,ybox)
c now fill box
c first titles
      ycor=mod_boxy-(1.05*lineheight*yrange)
      xcor=mod_boxx-(0.0125*wi*xrange)
      do 10 i=1,4
        xcor=xcor+(width(i)+(0.025*wi))*xrange
        call pgptxt(xcor,ycor,0.0,1.0,head(i))
   10 continue
c then values
      xcor=mod_boxx-(0.0125*wi*xrange)
      do 11 i=1,4
        xcor=xcor+(width(i)+(0.025*wi))*xrange
        depth=0.
        do 12 j=1,mod_nlay
          ycor=mod_boxy-lineheight*(1.1*float(j)+1.05)*yrange
          if (i.eq.1) then
            value=float(j)
          elseif (i.eq.2) then
            value=depth
          elseif (i.eq.3) then
            if (j.eq.mod_nlay) then
              value=0.
            else
              value=mod_thick(j)
            endif
          else
            value=1/mod_slo(j)
          endif
          write(field, '(f10.3)') value
          nc=int(log10(max(1.,value))+5.)
          if (i.eq.1) then
            write(field, '(i10)') int(value)
            nc=int(log10(max(1.,value))+1.)
          endif
          if ((j.eq.mod_nlay).and.(i.eq.3)) then
            field=inf
            nc=8
          endif
          call pgptxt(xcor,ycor,0.0,1.0,field(10-nc:10))
          if (j.lt.mod_nlay) depth=depth+mod_thick(j)
   12   continue
   11 continue
c keep character size
      call pgsch(charsize)
      return
      end
c
c----------------------------------------------------------------------
c perform model reading
c
      subroutine refract_doreadmodel(filename)
c
      include 'refract_model.inc'
      include 'refract_dim.inc'
      include 'refract_pgpara.inc'
      include 'refract_para.inc'
c
      character*(*) filename
c
      integer lu, i
      parameter(lu=10)
      real depth, depthold, velocity
c 
      depthold=0.
c
      if ((mod_valid).and.((elem_modbox).or.(elem_syntt))) flag_replot=.true.
      mod_valid=.true.
c
      open(lu, file=filename, status='old', err=99)
c
      read(lu, 52, err=98, end=97) mod_nlay
c 
      if (mod_nlay.le.mod_maxlay) then
        do i=1,mod_nlay
          read(lu, 53, err=98, end=97) depth,velocity
          mod_slo(i)=1./velocity
          if (i.gt.1) then
            mod_thick(i-1)=depth-depthold
          endif
          depthold=depth
        enddo
      else
        mod_valid=.false.
        call refract_warning('WARNING (readmodel): too many layers')
      endif
c 
      close(lu, err=96)
c 
c check model
      if (mod_valid) then
        do i=1,mod_nlay-1
          if (mod_slo(i).le.mod_slo(i+1)) then
            call refract_warning(
     &        'WARNING (readmodel): velocities must be increasing')
            mod_valid=.false.
          endif
          if (mod_thick(i).le.0.) then
            call refract_warning(
     &        'WARNING (readmodel): layer thickness must be positive')
            mod_valid=.false.
          endif
        enddo
      endif
c 
      return
   52 format(/23x,i3//)
   53 format(5x,2x,f15.3,2x,15x,2x,f15.3)
   99 stop 'ERROR (readmodel): opening file'
   98 stop 'ERROR (readmodel): reading file'
   97 stop 'ERROR (readmodel): reading file - unexpected end'
   96 stop 'ERROR (readmodel): closing file'
      end
c 
c----------------------------------------------------------------------
c ask for filename and initiate model reading
c
      subroutine refract_readmodel
c
      include 'refract_model.inc'
      include 'refract_dim.inc'
      include 'refract_pgpara.inc'
      include 'refract_para.inc'
c
      character*80 filename
c 
      write(6, 50)
      read(5,51) filename
c
      print *,'reading from ',filename(1:index(filename,' '))
      print *,'(will read depth and velocity!)'
      call refract_doreadmodel(filename)
c 
      return
   50 format('model input filename:')
   51 format(a)
      end
c 
c----------------------------------------------------------------------
c
      subroutine refract_writemodel
c
      include 'refract_model.inc'
      include 'refract_dim.inc'
      include 'refract_pgpara.inc'
c
      character*80 filename
      integer lu, i
      parameter(lu=10)
      real depth
c
      if (.not.(mod_valid)) then
        call refract_warning('WARNING (writemodel): no valid model')
        return
      endif
c 
      write(6, 50)
      read(5,51) filename
c
      print *,'writing to ',filename(1:index(filename,' '))
c
      open(lu, file=filename, err=99)
c
      write(lu, 52, err=98) pg_title, mod_nlay
c 
      depth=0.
c 
      do i=1,mod_nlay-1
        write(lu, 53, err=98) i,depth,mod_thick(i),1./mod_slo(i),mod_slo(i)
        depth=depth+mod_thick(i)
      enddo
c 
      write(lu, 54, err=98) depth,1./mod_slo(mod_nlay),mod_slo(mod_nlay)
c 
      close(lu, err=97)
c 
      return
   50 format('model output filename:')
   51 format(a)
   52 format(a/'earth model containing ',i3,' layers'//
     &  'layer',2x,'      depth [m]',2x,
     &          '  thickness [m]',2x,
     &          ' velocity [m/s]',2x,
     &          ' slowness [s/m]')
   53 format(i5,3(2x,f15.3),2x,f15.10)
   54 format('hs:  ',2x,f15.3,17x,2x,f15.3,2x,f15.10)
   99 stop 'ERROR (writemodel): opening file'
   98 stop 'ERROR (writemodel): writing file'
   97 stop 'ERROR (writemodel): closing file'
      end
c 
c ----- END OF refract_domodel.f -----
