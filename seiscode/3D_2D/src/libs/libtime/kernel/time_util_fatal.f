c this is <time_util_fatal.f>
c------------------------------------------------------------------------------
c
c Copyright 2000 by Thomas Forbriger (IfG Stuttgart)
c
c Print a fatal error message using FORTRAN i/o routines and abort
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
c
c REVISIONS and CHANGES
c    05/08/2000   V1.0   Thomas Forbriger
c    05/12/2007   V1.1   migration to g77: index function works differently
c
cS
c==============================================================================
c
      subroutine time_util_fatal(caller,text)
c
c declare parameters
      character*(*) caller,text
c
cE
c declare local variables
      integer index, last, len
c
c------------------------------------------------------------------------------
c go
      last=index(caller,' ')-1
      if (last.lt.1) last=len(caller)
      print 50,caller(1:last),text
      call abort()
c
      return
   50 format('ERROR (',a,'): ',a)
      end
c
c ----- END OF time_util_fatal.f -----
