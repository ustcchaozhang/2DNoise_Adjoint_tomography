c this is <refract_pgfilenames.f>
c------------------------------------------------------------------------------
c
c 03/07/98 by Thomas Forbriger (IfG Stuttgart)
c
c annotate with filenames
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
c This function is called from subroutine doplot in refract_doplot.f
c The subroutine refract_pgnamscal is called first to setup appropriate
c coordinates for the legend strings.
c
c REVISIONS and CHANGES
c    03/07/98   V1.0   Thomas Forbriger
c    24/05/00   V1.1   use selfilestyle
c    12/10/12   V1.2   in portrait mode file name legend did not contain
c                      the underlines within the plot area if more than
c                      one line was neede; resolved by workaround:
c                      increased x-range of world coordinates
c    13/11/2012 V1.3   use absolute coordinates
c
c==============================================================================
c
      subroutine refract_pgfilenames
c
      include 'refract_dim.inc'
      include 'refract_data.inc'
      include 'refract_opt.inc'
      include 'refract_pgpara.inc'
      include 'refract_para.inc'
      include 'refract_seipar.inc'
c 
      integer i,j
c
      if (debug) print *,'DEBUG: entered pgfilenames'
c set viewport
      call pgsave
      call pgsvp(tov_vpright, 1., tov_vpbot, tov_vptop)
      call pgswin(0., 1.0, 0., 1.0)
c 
      call pgsch(pg_nam_ch)
      do j=1,nfiles
c file index
        if (opt_Treverselegend) then
          i=nfiles+1-j
        else
          i=j
        endif
c plot label
        call pgptxt(pg_nam_xpos(i),pg_nam_ypos(i),90.,0.,filename(i))
c in case there are different styles to show
        if (((plflag_color).or.(plflag_linestyle).or.
     &       (pg_file_ci(i).gt.0)).and.(nfiles.gt.1)) then
          call pgsave
          call refract_selfilestyle(i)
          call pgmove(pg_nam_xpos(i)+0.75*pg_nam_linesep, pg_nam_ypos(i))
          call pgdraw(pg_nam_xpos(i)+0.75*pg_nam_linesep, pg_nam_yend(i))
          call pgunsa
        endif
c 
      enddo
c 
      call pgunsa
c
      return
      end
c
c ----- END OF refract_pgfilenames.f -----
