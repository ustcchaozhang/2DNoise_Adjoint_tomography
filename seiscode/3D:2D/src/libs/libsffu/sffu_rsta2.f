c this is <sffu_rsta2.f>
c ----------------------------------------------------------------------------
cS
c Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
c
c extract data from a STA2 line according to GSE2.1 format definition
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
c    12/06/2007   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine sffu_rsta2(line, network, lat, lon, 
     &   coordsys, elev, edepth, ierr)
c
      character*(*) line
      character*9 network
      real lat, lon, elev, edepth
      integer ierr
      character*12 coordsys
c  
c line            STA2 line from data file, to read values from
c ierr            if not equal zeor, an error has occured
c
c accroding to the GSE2.1 definition the other parameters mean:
c network         network identifier
c lat             latitude (degrees, S is negative)
c lon             longitude (degrees, W is negative)
c coordsys        reference coordinate system (e.g. WGS-84)
c elev            elevation (km)
c edepth          emplacement depth (km)
c
cE
c
      character*4 id
c
c----------------------------------------------------------------------
c
      ierr=1
      read(line, 50, err=99, end=99) id, network, lat, lon, 
     &  coordsys, elev, edepth
      ierr=0
   99 continue
      if (id.ne.'STA2') ierr=1
c
      return
   50 format(a4,1x,a9,1x,f9.5,1x,f10.5,1x,a12,1x,f5.3,1x,f5.3)
      end
c
c ----- END OF sffu_rsta2.f ----- 
