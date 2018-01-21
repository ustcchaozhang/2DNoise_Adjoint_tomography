c this is <dat_wtweight.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c write traveltime weights to a file
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
c    07/05/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine dat_wtweight(filename,orig)
c
c orig:     write original weights if true (calculated else)
c
      logical orig
      character*(*) filename
c
      include 'glq_dim.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
c 
cE
c
      integer lu,i
      parameter(lu=13)
c
c write code (easy to use)
c 
      if (verb_io) then
        print *,'NOTICE (dat_wtweight): '
        print *,'  opening traveltime weight file ',
     &    filename(1:index(filename,' ')),
     &    ' - overwrite mode'
      endif
      open(lu, file=filename, err=98)
      write(lu, err=97, fmt=50) data_ntts,'offset','weight'
      if (orig) then
        write(lu, err=97, fmt=51) 
     &    (travx(i), rtweight(i), i=1,data_ntts)
      else
        write(lu, err=97, fmt=51) 
     &    (travx(i), tweight(i), i=1,data_ntts)
      endif
      close(lu, err=96)
      if (verb_io) print *,'NOTICE (dat_wtweight): ',
     &                     'traveltime weight file written and closed'
c 
      return
   98 stop 'ERROR: opening traveltime weight file'
   97 stop 'ERROR: writing traveltime weight file'
   96 stop 'ERROR: closing traveltime weight file'
   50 format('traveltime weights',//,i5,' offsets',//,2(2x,a10))
   51 format(2(2x,g10.4))
      end

c
c ----- END OF dat_wtweight.f ----- 
