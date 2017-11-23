c this is <refract_pgpara.inc>
c------------------------------------------------------------------------------
c
c 01/05/98 by Thomas Forbriger (IfG Stuttgart)
c
c pure pgplot style settings
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
c    01/05/98   V1.0   Thomas Forbriger
c    24/05/00   V1.1   - file specific style settings
c                      - axis labels
c                      - line width and character height
c                      - fg and bg RGB
c    13/11/2012 V1.2   use different parameters for file name positioning
c
c==============================================================================
c
      integer pg_maindevice
c
c file specific settings
c pg_file_ls will indicate whether these values are used 
c  line style
      integer pg_file_ls(maxfiles)
c  line width
      integer pg_file_lw(maxfiles)
c  line color index
      integer pg_file_ci(maxfiles)
c  line color rgb values
c  negative values means: unused
      real pg_file_rgb(3,maxfiles)
c 
c plotting parameters for file names
      integer pg_nam_maxlines
      real pg_nam_xpos(maxfiles)
      real pg_nam_ypos(maxfiles)
      real pg_nam_yend(maxfiles)
      real pg_nam_ch, pg_nam_linesep
c 
c standard line width
      integer pg_std_lw
c 
c standard character height
      real pg_std_ch
c 
c RGB values
      real pg_std_fgrgb(3), pg_std_bgrgb(3)
c 
c line width for synthetic travel time curves
      integer pg_syntt_lw
c 
c program title
      character*120 pg_title
c
c axis labels
      character*120 pg_xlabel
      character*120 pg_ylabel
c
c actual pmmode
      character*20 pg_pmmode
c
c actual pickmode
      character*30 pg_pickmode
c
c common blocks
      common /refr_pgpara/ pg_maindevice
      common /refr_pgstrings/ pg_title, pg_xlabel, pg_ylabel,
     &                        pg_pmmode, pg_pickmode
      common /refr_pgnam/ pg_nam_xpos, pg_nam_ypos, pg_nam_yend,
     &                    pg_nam_ch, pg_nam_linesep, pg_nam_maxlines
      common /refr_pgfile/ pg_file_ls, pg_file_lw, pg_file_ci, pg_file_rgb
      common /refr_pgstd/ pg_std_lw, pg_std_ch, pg_std_fgrgb, pg_std_bgrgb
      common /refr_pgsettings/ pg_syntt_lw
c
c ----- END OF refract_pgpara.inc -----
