c this is <sffutilf.f>
c
c here is some fortran code for sffutil.c
c
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart)
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
c----------------------------------------------------------------------
c
c open file
c
      subroutine sff_fileWOpen(lu, filename, ierr)
      integer lu
      character*(*) filename
      integer ierr
      open(lu, file=filename, err=99, status='new')
      ierr=0
      return
   99 ierr=1
      print *,'sff_fileWOpen: ERROR opening file'
      return
      end
c----------------------------------------------------------------------
c
c close file
c
      subroutine sff_fileClose(lu, ierr)
      integer lu, ierr
      close(lu, err=99)
      ierr=0
      return
   99 ierr=1
      print *,'sff_fileClose: ERROR closing file'
      return
      end
c
c----------------------------------------------------------------------
c 
c write one line
c
      subroutine sff_writeline(funit, text)
c declare parameters
      character*(*) text
      integer funit
c go
      write(funit, '(a)', err=99) text
      return
   99 stop 'sffwriteline: ERROR writing text to file'
      end
c
cccccc END OF sffutilf.f cccccc
