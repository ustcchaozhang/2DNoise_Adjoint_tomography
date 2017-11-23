c this is <refract_pgtrace.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c plot one trace
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
c    30/04/98   V1.0   Thomas Forbriger
c    24/05/00   V1.1   use selfilestyle
c    19/06/2003 V1.2   introduced trace labels
c    09/09/2004 V1.3   introduced station name labels
c    20/11/2012 V1.4   plot baseline if requested
c    01/12/2016 V1.5   fix check for label bounding box in reverse mode
c
c==============================================================================
cS
c
      subroutine pgtrace(i)
c
c plot one trace i
c
      integer i
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_seipar.inc'
      include 'refract_para.inc'
c
cE
      logical settracevp, reverse
      real baseline
      integer invers, bc, nc, n0, nlast
      real xbox(4), ybox(4),xpos
      character*30 label
      logical inside
      integer j
c
      if (debug) print *,'DEBUG: entered pgtrace'
      invers=1
      nc=1
      if (plflag_bubbles) nc=2
      if (plflag_invers) invers= -1*invers
c in case of bubbles do plotting twice with reversed sign
      do bc=1,nc
        invers= -1*invers
        reverse=.false.
        if (invers.gt.0) reverse=.true.
        if (settracevp(i,reverse)) then
          if (debug) print *, 'DEBUG: upon plotting (pgtrace)'
          if (debug) print *, 'DEBUG: trv_vpleft ',trv_vpleft
          if (debug) print *, 'DEBUG: trv_vpright ',trv_vpright
          if (debug) print *, 'DEBUG: trv_vpbot ',trv_vpbot
          if (debug) print *, 'DEBUG: trv_vptop ',trv_vptop
c select color and line style
          call pgsave
          if (plflag_seistyle) call refract_selfilestyle(fileindex(i))
c set my viewport and window
          call pgsvp(trv_vpleft, trv_vpright, trv_vpbot, trv_vptop)
          call pgswin(trv_tmin, trv_tmax, trv_vmin, trv_vmax)
c 
c plot subscales if ordered
          if (plflag_subscale) then
            call pgsave
            call pgsfs(1)
            call pgsch(0.5)
            call pgbox('BCTI',0.,0,'ABCM',0.,0)
            call pgsls(4)
            call pgbox('ABGC',0.,0,'ABGCTS',0.,0)
            call pgunsa
          endif
c 
c plotrange
          if (trv_tmin.gt.timeofsample(firstsample(i))) then
            n0=int((trv_tmin-timeofsample(firstsample(i)))/dt(i))+
     &        firstsample(i)
          else
            n0=firstsample(i)
          endif
          if (trv_tmax.lt.timeofsample(firstsample(i)+nsamples(i)-1)) then
            nlast=firstsample(i)+nsamples(i)-int((timeofsample(
     &        firstsample(i)+nsamples(i)-1)-trv_tmax)/dt(i))-1
            nlast=max(nlast,firstsample(i)+nsamples(i)-1)
          else
            nlast=nsamples(i)+firstsample(i)-1
          endif
c
c plot this trace
          call pgline((nlast-n0+1), timeofsample(n0),
     &      data(n0))
c
c set correct baseline for variable area
          baseline=0.
          if (plpar_remav) baseline=average(i)
          if (plotbaseline(fileindex(i))) then
            call pgmove (trv_tmin, baseline)
            call pgdraw (trv_tmax, baseline)
          endif
c plot variable area
          if (((plflag_vara).and.(usevarplot(fileindex(i)).ne.0))
     &      .or.(usevarplot(fileindex(i)).eq.1))
     &        call varplot(timeofsample(n0),
     &          data(n0), baseline, (nlast-n0+1),
     &          plflag_invers)
c 
c label trace
          if (plflag_tracenum) then
            write(label, 50) traceinfile(i)
          elseif (plflag_tracename) then
            write(label, 51) station(i)
          endif
          if (plflag_tracenum.or.plflag_tracename) then
            xpos=min(timeofsample(nlast),trv_tmax)
c            print *,label
c check string position
            call pgqtxt(xpos,data(nlast),0.,1.,label, xbox, ybox)
            inside=.false.
            do j=1,4
              if (reverse) then
                if ((ybox(j).lt.trv_vmin).and.(ybox(j).gt.trv_vmax))
     &            inside=.true.
              else
                if ((ybox(j).gt.trv_vmin).and.(ybox(j).lt.trv_vmax))
     &            inside=.true.
              endif
            enddo
            if (inside) then
              call pgsave
              call pgsfs(1)
              call pgsci(0)
              call pgpoly(4,xbox,ybox)
              call pgunsa
              call pgptxt(xpos,data(nlast),0.,1.,label)
            endif
          endif
c
          call pgunsa
        endif
      enddo
c 
c      call pgupdt
c
      return
   50 format(i3.3)
   51 format(a)
      end
c
c ----- END OF refract_pgtrace.f -----
