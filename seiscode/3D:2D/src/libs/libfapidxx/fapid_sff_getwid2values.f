c this is <fapid_sff_getwid2values.f>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
c
c functions to extract values from WID2 line
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
c These functions are just copied from stuff.f
c
c REVISIONS and CHANGES
c    17/01/2011   V1.0   Thomas Forbriger
c    01/04/2011   V1.1   decode WID2 line
c
c ============================================================================
c
c  Utilities to extract info from wid2line
cD
c---------------------------------------------------------------------------
      subroutine sff_GetDate(wid2line,date)
c
c  extract date (yyyy/mm/dd)
c
      character wid2line*132, date*(*)
cE
      character wid2decoded*132
c
      call sff_helper_decode_wid2(wid2line, wid2decoded)
      date = wid2decoded(6:15)
      return
      end
cD
c---------------------------------------------------------------------------
      subroutine sff_GetTime(wid2line, time)
c
c  extract time (hh:mm:ss.sss)
c
      character wid2line*132, time*(*)
cE
      character wid2decoded*132
c
      call sff_helper_decode_wid2(wid2line, wid2decoded)
      time = wid2decoded(17:28)
      return
      end
cD
c---------------------------------------------------------------------------
      subroutine sff_GetStation(wid2line, sta)
c
c  extract station name (a5)
c
      character wid2line*132, sta*(*)
cE
      character wid2decoded*132
c
      call sff_helper_decode_wid2(wid2line, wid2decoded)
      sta = wid2decoded(30:34)
      return
      end
cD
c---------------------------------------------------------------------------
      subroutine sff_GetChannel(wid2line, channel)
c
c  extract channel name (a3)
c
      character wid2line*132, channel*(*)
cE
      character wid2decoded*132
c
      call sff_helper_decode_wid2(wid2line, wid2decoded)
      channel = wid2decoded(36:38)
      return
      end
cD
c---------------------------------------------------------------------------
      integer function sff_GetN(wid2line)
c
c  extract number of samples
c
      character wid2line*132
cE
      integer n
      character wid2decoded*132
c
      call sff_helper_decode_wid2(wid2line, wid2decoded)
c 
      read(wid2decoded(49:56),'(i8)') n
      sff_GetN = n
      return
      end
cD
c---------------------------------------------------------------------------
      real function sff_GetDt(wid2line)
c
c  extract sampling interval
c
      character wid2line*132
cE
      real dt
      character wid2decoded*132
c
      call sff_helper_decode_wid2(wid2line, wid2decoded)
c 
      read(wid2decoded(58:68),'(f11.6)') dt
      sff_GetDt = 1./dt
      return
      end
c
c ----- END OF fapid_sff_getwid2values.f ----- 
