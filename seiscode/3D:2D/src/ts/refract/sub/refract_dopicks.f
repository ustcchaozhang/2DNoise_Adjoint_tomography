c this is <refract_dopicks.f>
c------------------------------------------------------------------------------
c
c 05/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c all we have to do to work with picking
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
c    18/07/98   V1.1   now allow one single pick per each trace
c                      - changed traveltime units to [km] and [s]
c    24/05/00   V1.2   subdivided readttpicks
c    19/03/02   V1.3   plot traveltime picks also in color and with larger
c                      crosses at the breaks 
c
c==============================================================================
c
      subroutine addpick(xcoor, ycoor)
c 
c add another pick at (xcoor,ycoor)
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_picks.inc'
      include 'refract_para.inc'
      include 'refract_model.inc'
      include 'refract_seipar.inc'
c 
      real xcoor,ycoor
c
c declare variables
c
      integer pick, i
      real realtime
      real mindist, dist, newoff
c
      if (debug) print *,'DEBUG: plpar_pickmode ',plpar_pickmode
c 
      if ((plpar_pickmode.lt.1).or.(plpar_pickmode.gt.pick_ntypes))
     &   stop 'ERROR (addpick): invalid pickmode'
c check range
      if ((xcoor.gt.tov_tmax).or.(xcoor.lt.tov_tmin).or.
     &    (ycoor.gt.tov_rmax).or.(ycoor.lt.tov_rmin)) then
        call refract_warning('WARNING (addpick): out of frame')
      else
c check range
        if (pick_n(plpar_pickmode).ge.pick_max) then
          call refract_warning('WARNING (addpick): reached maximum pick count')
        else
c ok - go and remove picks from plot
          call refract_pgpicks(plpar_pickmode, .false.)
c in case of arrival time mode: make traces sticky
          if (plpar_pickmode.eq.6) then
            mindist=abs(ycoor-roffset(1))
            newoff=roffset(1)
            do i=1,ntraces
              dist=abs(ycoor-roffset(i))
              if (dist.lt.mindist) then
                mindist=dist
                newoff=roffset(i)
              endif
            enddo
            ycoor=newoff
          endif
c look for next position
          pick=1
          do while ((pick.le.pick_n(plpar_pickmode)).and.(pick.gt.0))
            if (pick_y(pick,plpar_pickmode).gt.ycoor) then
c shift all to insert the new pick
              do i=pick_n(plpar_pickmode),pick,-1
                pick_x(i+1,plpar_pickmode)=pick_x(i,plpar_pickmode)
                pick_y(i+1,plpar_pickmode)=pick_y(i,plpar_pickmode)
              enddo 
c insert pick
              pick_n(plpar_pickmode)=pick_n(plpar_pickmode)+1
              pick_x(pick,plpar_pickmode)=
     &          realtime(xcoor,ycoor)
              pick_y(pick,plpar_pickmode)=ycoor
              pick=-1
            endif
            pick=pick+1
          enddo
c handle pick to be appended
          if (pick.eq.(pick_n(plpar_pickmode)+1)) then
            pick_n(plpar_pickmode)=pick_n(plpar_pickmode)+1
            pick_x(pick_n(plpar_pickmode),plpar_pickmode)=
     &        realtime(xcoor,ycoor)
            pick_y(pick_n(plpar_pickmode),plpar_pickmode)=ycoor
          endif
c that's it - display picks again
          call refract_pgpicks(plpar_pickmode, .true.)
c
          if (plpar_pickmode.eq.1) then
            if ((elem_modbox).and.(mod_valid)) then
              flag_replot=.true.
            endif
            mod_valid=.false.
          endif
        endif
      endif
c 
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine delpick(xcoor,ycoor)
c
c delete pick nearest to cursor
c
      real xcoor,ycoor
c 
      include 'refract_dim.inc'
      include 'refract_picks.inc'
      include 'refract_para.inc'
      include 'refract_model.inc'
      include 'refract_seipar.inc'
c
c declare variables
c
      real thisdist, lastdist
      integer pick, i
      real redtime
c
c 
      if ((plpar_pickmode.lt.1).or.(plpar_pickmode.gt.pick_ntypes))
     &   stop 'ERROR (delpick): invalid pickmode'
c check range
      if ((xcoor.gt.tov_tmax).or.(xcoor.lt.tov_tmin).or.
     &    (ycoor.gt.tov_rmax).or.(ycoor.lt.tov_rmin)) then
        call refract_warning('WARNING (delpick): out of frame')
      else
c check range
        if (pick_n(plpar_pickmode).lt.1) then
          call refract_warning('WARNING (delpick): no pick to delete')
        else
c ok - go and remove picks from plot
          call refract_pgpicks(plpar_pickmode, .false.)
c search nearest pick
          lastdist=((ycoor-pick_y(1,plpar_pickmode))/(tov_rmax-tov_rmin))**2
          lastdist=lastdist+((xcoor-redtime(pick_x(1,plpar_pickmode),
     &      pick_y(1,plpar_pickmode)))/
     &      (tov_tmax-tov_tmin))**2
          pick=1
          do i=1,pick_n(plpar_pickmode)
            thisdist=((ycoor-pick_y(i,plpar_pickmode))/(tov_rmax-tov_rmin))**2
            thisdist=thisdist+((xcoor-redtime(pick_x(i,plpar_pickmode),
     &        pick_y(i,plpar_pickmode)))/
     &        (tov_tmax-tov_tmin))**2
            if (thisdist.lt.lastdist) then
              pick=i
              lastdist=thisdist
            endif
          enddo
c delete pick
          if (pick.lt.pick_n(plpar_pickmode)) then
            do i=pick,pick_n(plpar_pickmode)-1
              pick_x(i,plpar_pickmode)=pick_x(i+1,plpar_pickmode)
              pick_y(i,plpar_pickmode)=pick_y(i+1,plpar_pickmode)
            enddo
          endif
          pick_n(plpar_pickmode)=pick_n(plpar_pickmode)-1
c that's it - display picks again
          call refract_pgpicks(plpar_pickmode, .true.)
c 
          if (plpar_pickmode.eq.1) then
            if ((elem_modbox).and.(mod_valid)) then
              flag_replot=.true.
            endif
            mod_valid=.false.
          endif
        endif
      endif
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine refract_eraseallpicks
c 
c erase all picks
c
      include 'refract_picks.inc'
c
      integer i
c
      do i=1,pick_ntypes
        call refract_pgpicks(i, .false.)
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine refract_plotallpicks
c 
c plot all picks
c
      include 'refract_picks.inc'
c
      integer i
c
      do i=1,pick_ntypes
        call refract_pgpicks(i, .true.)
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine refract_pgpicks(i, f)
c 
c plot picks of set i if flag is .true.
c erase otherwise
c
c 19/03/2002      plot traveltime picks also in color and with larger crosses
c                 at the breaks 
c
      include 'refract_picks.inc'
      include 'refract_para.inc'
c 
      integer i
      logical f
c 
      integer j
      real redtime
c 
      if ((i.lt.1).or.(i.gt.pick_ntypes))
     &  stop 'ERROR (pgpicks): check picktype'
c 
c select style
      if (pick_n(i).gt.0) then
        call pgsave
        if (plflag_picol) call refract_selstyle(i+1)
        if (.not.(f)) then
          call pgsci(0)
        endif
c 
c other mode for single picks
        if (i.eq.6) then
          call pgsch(2.)
          do j=1,pick_n(i)
            call pgpt1(redtime(pick_x(j,i),pick_y(j,i)),pick_y(j,i),2)
          enddo
        else
          call pgmove(redtime(pick_x(1,i),pick_y(1,i)),pick_y(1,i))
          if (pick_n(i).gt.1) then
            do j=2,pick_n(i)
              call pgdraw(redtime(pick_x(j,i),pick_y(j,i)),pick_y(j,i))
            enddo
          endif
          if (i.eq.plpar_pickmode) then
            call pgsave
            call pgsch(2.)
            call pgslw(5)
            do j=1,pick_n(i)
              call pgpt1(redtime(pick_x(j,i),pick_y(j,i)),pick_y(j,i),5)
            enddo
            call pgunsa
          endif
          if (i.eq.1) then
            call pgmove(0., 0.)
            call pgdraw(redtime(pick_x(1,1),pick_y(1,1)),pick_y(1,1))
          endif
        endif
c 
        call pgunsa
      endif
c 
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine refract_readtappicks
c 
c read taper picks from file
c
      include 'refract_picks.inc'
      include 'refract_dim.inc'
      include 'refract_pgpara.inc'
      include 'refract_para.inc'
c 
      character*80 filename, junk
c
      write(6, 50)
      read(5, 51) filename
      print *,'reading from ',filename(1:index(filename,' '))
c 
      call tf_ttapread(filename,pick_x(1,2), pick_y(1,2), pick_n(2),
     &  pick_max, junk)
c 
      flag_replot=.true.
c 
      return
   50 format('input filename for tapers:')
   51 format(a)
      end
c 
c----------------------------------------------------------------------
c
      subroutine refract_writetappicks
c 
c write taper picks to file 
c
      include 'refract_picks.inc'
      include 'refract_dim.inc'
      include 'refract_pgpara.inc'
c 
      character*80 filename
      integer lu
      parameter(lu=10)
c
      write(6, 50)
      read(5, 51) filename
      print *,'writing to ',filename(1:index(filename,' '))
c 
      open(lu, file=filename)
      close(lu, status='delete')
c 
      call tf_ttapwrite(filename,pick_x(1,2), pick_y(1,2), pick_n(2),
     &  pick_max, pg_title)
c 
      return
   50 format('output filename for tapers:')
   51 format(a)
      end
c 
c----------------------------------------------------------------------
c
      subroutine refract_doreadttpicks(filename, iset)
c 
c read traveltime picks from file to pickset iset
c perform actual reading
c
      include 'refract_picks.inc'
      include 'refract_para.inc'
c 
      integer iset
      character*(*) filename
c
      integer lu, i
      parameter(lu=10)
c 
      if ((iset.lt.1).or.(iset.gt.pick_ntypes))
     &   stop 'ERROR (readttpicks): wrong pickset number'
c
      open(lu, file=filename, status='old', err=99)
      read(lu, '(/,i10)', err=98, end=97) pick_n(iset)
c 
      if (pick_n(iset).le.pick_max) then
        do i=1,pick_n(iset)
          read(lu, *, err=98, end=97) pick_y(i,iset), pick_x(i,iset)
          pick_y(i, iset)=pick_y(i,iset)*1000.
        enddo
c 
        flag_replot=.true.
        print *,'  ',pick_n(iset),' picks read to set ',iset
      else
        call refract_warning('WARNING (readttpicks): too many samples')
      endif
c 
      close(lu, err=96)
c 
      return
   99 stop 'ERROR (readttpicks): opening travel time file'
   98 stop 'ERROR (readttpicks): reading travel time file'
   97 stop 'ERROR (readttpicks): reading travel time file - unexpected end'
   96 stop 'ERROR (readttpicks): closing travel time file'
      end
c 
c----------------------------------------------------------------------
c
      subroutine refract_readttpicks(iset)
c 
c read traveltime picks from file to pickset iset
c ask for filename and call reading subroutine
c
      include 'refract_picks.inc'
      include 'refract_para.inc'
c 
      integer iset
c 
      character*80 filename
c 
      if ((iset.lt.1).or.(iset.gt.pick_ntypes))
     &   stop 'ERROR (readttpicks): wrong pickset number'
c
      write(6, 50)
      read(5, 51) filename
      print *,'reading from ',filename(1:index(filename,' '))
      call refract_doreadttpicks(filename, iset)
c 
      return
   50 format('input filename for traveltimes:')
   51 format(a)
      end
c 
c----------------------------------------------------------------------
c
      subroutine refract_writettpicks(iset)
c 
c write traveltime picks to file from pickset iset
c
      include 'refract_picks.inc'
      include 'refract_dim.inc'
      include 'refract_pgpara.inc'
c 
      integer iset
c 
      character*80 filename
      integer lu, i
      parameter(lu=10)
c 
      if ((iset.lt.1).or.(iset.gt.pick_ntypes))
     &   stop 'ERROR (readttpicks): wrong pickset number'
c
      write(6, 50)
      read(5, 51) filename
      print *,'writing to ',filename(1:index(filename,' '))
c 
      open(lu, file=filename, err=98)
      write(lu, '(a)', err=97) pg_title
      write(lu, '(i5)', err=97) pick_n(iset)
      write(lu, '(2f12.7)', err=97) (pick_y(i,iset)/1000., pick_x(i,iset),
     &   i=1,pick_n(iset))
      close(lu, err=96)
c 
      return
   50 format('output filename for traveltimes:')
   51 format(a)
   98 stop 'ERROR (writettpicks): opening travel time file'
   97 stop 'ERROR (writettpicks): writing travel time file'
   96 stop 'ERROR (writettpicks): closing travel time file'
      end
c 
c ----- END OF refract_dopicks.f -----
