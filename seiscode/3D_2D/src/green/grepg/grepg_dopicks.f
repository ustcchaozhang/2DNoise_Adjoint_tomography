c this is <grepg_dopicks.f>
c------------------------------------------------------------------------------
c
c 25/11/98 by Thomas Forbriger (IfG Stuttgart)
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
c pick dispersion CURVES
c
c REVISIONS and CHANGES
c    25/11/98   V1.0   Thomas Forbriger
c                      I took this code from grepg
c    24/01/99   V1.1   did split reading routine to provide direct interface
c                      to file reading
c    12/04/00   V1.2   introduced pick curve background
c    05/03/01   V1.3   added color dispersion curve plot
c    21/01/02   V1.4   allow mode picks to be read
c    06/02/17   V1.5   support color curve on white background
c
c==============================================================================
c 
      subroutine grepg_pickinit
c
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
c
      integer i
c
      do i=1,maxpickdim
        npicks(i)=0
      enddo
c 
      active_pick=1
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine addpick(xcoor, ycoor)
c 
c add another pick at (xcoor,ycoor)
c
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
      include 'grepg_para.inc'
c 
      real xcoor,ycoor
c
c declare variables
c
      integer pick, i
c 
      if ((active_pick.lt.1).or.(active_pick.gt.maxpicktypes))
     &   stop 'ERROR (addpick): invalid pickmode'
c check range
      if ((xcoor.gt.maxf).or.(xcoor.lt.minf).or.
     &    (ycoor.gt.maxs).or.(ycoor.lt.mins)) then
        call grepg_warning('WARNING (addpick): out of frame')
      else
c check range
        if (npicks(active_pick).ge.maxpicks) then
          call grepg_warning('WARNING (addpick): reached maximum pick count')
        else
c ok - go and remove picks from plot
          call grepg_pgpicks(active_pick, .false.)
c look for next position
          pick=1
          do while ((pick.le.npicks(active_pick)).and.(pick.gt.0))
            if (fpicks(pick,active_pick).gt.xcoor) then
c shift all to insert the new pick
              do i=npicks(active_pick),pick,-1
                fpicks(i+1,active_pick)=fpicks(i,active_pick)
                spicks(i+1,active_pick)=spicks(i,active_pick)
              enddo 
c insert pick
              npicks(active_pick)=npicks(active_pick)+1
              fpicks(pick,active_pick)=xcoor
              spicks(pick,active_pick)=ycoor
              pick=-1
            endif
            pick=pick+1
          enddo
c handle pick to be appended
          if (pick.eq.(npicks(active_pick)+1)) then
            npicks(active_pick)=npicks(active_pick)+1
            fpicks(npicks(active_pick),active_pick)=xcoor
            spicks(npicks(active_pick),active_pick)=ycoor
          endif
c that's it - display picks again
          call grepg_pgpicks(active_pick, .true.)
c
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
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
      include 'grepg_para.inc'
c
c declare variables
c
      real thisdist, lastdist
      integer pick, i
c
c 
      if ((active_pick.lt.1).or.(active_pick.gt.maxpicktypes))
     &   stop 'ERROR (delpick): invalid pickmode'
c check range
      if ((xcoor.gt.maxf).or.(xcoor.lt.minf).or.
     &    (ycoor.gt.maxs).or.(ycoor.lt.mins)) then
        call grepg_warning('WARNING (delpick): out of frame')
      else
c check range
        if (npicks(active_pick).lt.1) then
          call grepg_warning('WARNING (delpick): no pick to delete')
        else
c ok - go and remove picks from plot
          call grepg_pgpicks(active_pick, .false.)
c search nearest pick
          lastdist=((ycoor-spicks(1,active_pick))/(maxs-mins))**2
          lastdist=lastdist+((xcoor-fpicks(1,active_pick))/
     &      (maxf-minf))**2
          pick=1
          do i=1,npicks(active_pick)
            thisdist=((ycoor-spicks(i,active_pick))/(maxs-mins))**2
            thisdist=thisdist+((xcoor-fpicks(i,active_pick))/
     &        (maxf-minf))**2
            if (thisdist.lt.lastdist) then
              pick=i
              lastdist=thisdist
            endif
          enddo
c delete pick
          if (pick.lt.npicks(active_pick)) then
            do i=pick,npicks(active_pick)-1
              fpicks(i,active_pick)=fpicks(i+1,active_pick)
              spicks(i,active_pick)=spicks(i+1,active_pick)
            enddo
          endif
          npicks(active_pick)=npicks(active_pick)-1
c that's it - display picks again
          call grepg_pgpicks(active_pick, .true.)
c 
        endif
      endif
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine grepg_eraseallpicks
c 
c erase all picks
c
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
c
      integer i
c
      do i=1,maxpickdim
        if (npicks(i).gt.0) call grepg_pgpicks(i, .false.)
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c 
      subroutine grepg_plotallpicks
c 
c plot all picks
c
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
c
      integer i
c
      do i=1,maxpickdim
        if (npicks(i).gt.0) call grepg_pgpicks(i, .true.)
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
      subroutine grepg_pgpicks(i, f)
c 
c plot picks of set i if flag is .true.
c erase otherwise
c
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
      include 'grepg_para.inc'
c 
      integer i
      logical f
c 
      integer j, n, k, w
c      real oldr, oldg, oldb
c 
      if ((i.lt.1).or.(i.gt.maxpickdim))
     &  stop 'ERROR (pgpicks): check picktype'
c 
c select style
      if (npicks(i).gt.0) then
        call pgqlw(w)
        call pgsave
        call grepg_selstyle(i)
        if (.not.(f)) then
          call pgsci(0)
        endif
c 
        n=1
c run plot twice, if dispersion curves shall be displayed on white
c background
        if (plflag_bgcurve) n=2
        do k=1,n
          if (plflag_colcur) then
            call pgsci(3)
            call pgscr(3, plpar_colcurc(1), plpar_colcurc(2),
     &                    plpar_colcurc(3))
            call pgslw(plpar_colcurw)
          else
            call pgsci(1)
            call pgslw(w)
          endif
          if (plflag_bgcurve.and.(k.eq.1)) then
            call pgsci(0)
            call pgslw(plpar_bgcurvewidth)
          endif
          call pgmove(fpicks(1,i),spicks(1,i))
          if (npicks(i).gt.1) then
            do j=2,npicks(i)
              call pgdraw(fpicks(j,i),spicks(j,i))
            enddo
          endif
          if (.not.(plflag_bgcurve.or.plflag_colcur)) then
            if (i.eq.active_pick) then
              do j=1,npicks(i)
                call pgpt1(fpicks(j,i),spicks(j,i),5)
              enddo
            endif
          endif
        enddo
c        if (plflag_colcur) then
c          call pgscr(3, oldr, oldg, oldb)
c        endif
c 
        call pgunsa
      endif
c 
      return
      end
c 
c----------------------------------------------------------------------
c
      subroutine grepg_readpickfile
c 
c ask for filename and read picks
c
      character*80 filename
c
      write(6, 50)
      read(5, 51) filename
      call grepg_readpicks(filename)
c 
      return
   50 format('input filename for dispersion curves:')
   51 format(a)
      end
c 
c----------------------------------------------------------------------
c
      subroutine grepg_readpicks(filename)
c 
c read picks from file
c
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
c 
      character*(*) filename
c 
      integer iset
c 
      integer lu, i, ntoread
      parameter(lu=10)
c 
      print *,'reading from ',filename(1:index(filename,' '))
c 
      open(lu, file=filename, status='old', err=99)
      read(lu, '(/,i10)', err=98, end=97) ntoread
      print *,'  going to read ',ntoread,' curves'
      if (ntoread.gt.maxpickdim) stop 'ERROR: too many curves'
c 
      call grepg_pickinit
      do iset=1,ntoread
        read(lu, '(i10)', err=98, end=97) npicks(iset)
c 
        if (npicks(iset).le.maxpicks) then
          do i=1,npicks(iset)
            read(lu, *, err=98, end=97) spicks(i,iset), fpicks(i,iset)
          enddo
c 
          print *,'  ',npicks(iset),' picks read to set ',iset
        else
          stop 'ERROR: too many samples'
        endif
      enddo
c 
      close(lu, err=96)
c 
      return
   99 stop 'ERROR (readpicks): opening dispersion curve file'
   98 stop 'ERROR (readpicks): reading dispersion curve file'
   97 stop 'ERROR (readpicks): reading dispersion curve file - unexpected end'
   96 stop 'ERROR (readpicks): closing dispersion curve file'
      end
c 
c----------------------------------------------------------------------
c
      subroutine grepg_writepicks
c 
c write picks to file
c
      include 'grepg_dim.inc'
      include 'grepg_picks.inc'
      include 'grepg_para.inc'
c 
      integer iset
c 
      character*80 filename
      integer lu, i, ntowrite
      parameter(lu=10)
c 
      ntowrite=0
      do i=1,maxpicktypes
        if (npicks(i).gt.0) ntowrite=ntowrite+1
      enddo
      print *,'  going to write ',ntowrite,' curves'
c
      write(6, 50)
      read(5, 51) filename
      print *,'writing to ',filename(1:index(filename,' '))
c 
      open(lu, file=filename, err=98)
      write(lu, '(a)', err=97) plstring_version
      write(lu, '(i5)', err=97) ntowrite
      do iset=1,maxpicktypes
        if (npicks(iset).gt.0) then
          write(lu, '(i5)', err=97) npicks(iset)
          write(lu, '(2g12.7)', err=97) (spicks(i,iset), fpicks(i,iset),
     &       i=1,npicks(iset))
         endif
      enddo
      close(lu, err=96)
c 
      return
   50 format('output filename for dispersion curves:')
   51 format(a)
   98 stop 'ERROR (writepicks): opening dispersion curve file'
   97 stop 'ERROR (writepicks): writing dispersion curve file'
   96 stop 'ERROR (writepicks): closing dispersion curve file'
      end
c 
c ----- END OF grepg_dopicks.f -----
