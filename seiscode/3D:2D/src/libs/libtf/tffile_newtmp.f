c this is <tffile_newtmp.f>
c------------------------------------------------------------------------------
c
c Copyright (c) 1999 by Thomas Forbriger (IfG Stuttgart)
c
c find a filename for temporary use
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
c    19/01/99   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c
      subroutine tffile_newtmp(filename)
c 
c input:    filename gives name base
c output:   full unique filename
c
c declare parameters
      character filename*(*)
c 
cE
c
c declare local variables
      integer i, tfstr_trimlen
      character*80 testfilename, startfilename
      character*4 string
      logical lexist
c
      i=0
      if(filename.eq.' ')then
         startfilename='tmpfile'
      else
         startfilename=filename
      endif
      testfilename=startfilename
      inquire(file=testfilename,exist=lexist)
      do while (lexist)
        i=i+1
        write(string, '(i4.4)') i
        testfilename=startfilename(1:tfstr_trimlen(startfilename))//string
        inquire(file=testfilename,exist=lexist)
        if (i.gt.9999) 
     &    stop 'ERROR (tffile_newtmp): could not find unique name'
      enddo
c
      return
      end
c
c ----- END OF tffile_newtmp.f -----
