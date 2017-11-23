c this is <ttime_read.f>
c------------------------------------------------------------------------------
c
c Copyright 02/02/99 by Thomas Forbriger (IfG Stuttgart)
c
c test libtime read-function
c
c ----
c libtime is free software; you can redistribute it and/or modify
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
c    02/02/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      program ttime_read
      character*79 string
      integer date(7)
      string=' '
      print *,'enter ''end'' to exit'
      do while (string(1:4).ne.'end ')
        print *,' '
        print *,'enter timestring: '
        read(5, '(a)') string
        if (string(1:4).ne.'end ') then
          print *,string
          call time_read(string, date)
          call time_sprint(date,string)
          print *,string
        endif
      enddo
      stop
      end
c
c ----- END OF ttime_read.f -----
