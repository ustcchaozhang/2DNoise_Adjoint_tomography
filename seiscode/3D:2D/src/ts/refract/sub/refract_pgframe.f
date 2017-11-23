c this is <refract_pgframe.f>
c------------------------------------------------------------------------------
c
c 30/04/98 by Thomas Forbriger (IfG Stuttgart)
c
c plot a frame
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
c    18/02/99   V1.1   allow axis labelling in degrees
c    24/05/00   V1.2   - use selfilestyle
c                      - use axis label strings
c    13/11/12   V1.3   plot a frame at viewport bounds
c
c==============================================================================
cS
c
      subroutine pgframe
c 
c plot a frame
c
      include 'refract_dim.inc'
      include 'refract_seipar.inc'
      include 'refract_para.inc'
      include 'refract_pgpara.inc'
      include 'refract_data.inc'
c 
      integer i
c 
      real ymax,ymin,pi
      parameter(pi=3.141592653589793)
c 
      if (debug) print *,'DEBUG: entered pgframe'
c
      call pgframeact
c degrees ? 
      if (plpar_radius.gt.0.) then
        ymin=0.18*tov_rmin/plpar_radius/pi
        ymax=0.18*tov_rmax/plpar_radius/pi
        call pgswin(tov_tmin, tov_tmax, ymin, ymax)
      endif
c 
      if (elem_scales) then
        call pgbox('ABCNST',0.0,0,'ABCNST',0.0,0)
      else
        call pgbox('ABCST',0.0,0,'ABCST',0.0,0)
      endif
c back to meters
      call pgframeact
c 
      if (elem_scales) call pglab(pg_xlabel, pg_ylabel, ' ')
      if (elem_version) call pglab(' ',' ',pg_title)
c 
c plot graph markers at offset position
      if (plflag_subscale) then
        call pgsave
        call pgsch(1.8)
        do i=1,ntraces
          call refract_selfilestyle(fileindex(i))
          call pgpt1(tov_tmin, roffset(i), -2-fileindex(i))
        enddo
        do i=ntraces,1,-1
          call refract_selfilestyle(fileindex(i))
          call pgpt1(tov_tmax, roffset(i), -2-fileindex(i))
        enddo
        call pgunsa
      endif
c 
c plot grid
      if (plflag_grid) then
        call pgqls(i)
        call pgsls(4)
        call pgbox('AGST',0.0,0,'AGST',0.0,0)
        call pgsls(i)
      endif
c
      return
      end
c
c ----- END OF refract_pgframe.f -----
