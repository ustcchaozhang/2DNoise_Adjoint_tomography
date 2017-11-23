cS
c this is <dat_wgweight.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
c write green data weights to a file
c
c REVISIONS and CHANGES
c    07/05/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine dat_wgweight(filename,orig)
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
c magic number for binary file identification
      character*4 wcmagic
      parameter(wcmagic='123S')
      integer magic
      integer lu,i,j
      parameter(lu=13)
c
c write taper code (easy to use)
c 
      if (verb_io) then
        print *,'NOTICE (dat_wgweight): '
        print *,'  opening green weight file ',
     &    filename(1:index(filename,' ')),
     &    ' - overwrite mode'
      endif
      open(lu, file=filename, form='unformatted', err=98)
      call tf_magic(wcmagic, magic)
      write(lu, err=97) magic
      write(lu, err=97) data_nfre, data_nslo
      if (orig) then
        write(lu, err=97) 
     &    ((rgweight(j,i), i=1,data_nfre), j=1,data_nslo)
      else
        write(lu, err=97) ((gweight(j,i), i=1,data_nfre), j=1,data_nslo)
      endif
      close(lu, err=96)
      if (verb_io) print *,'NOTICE (dat_wgweight): ',
     &                     'green weight file written and closed'
c 
      return
   98 stop 'ERROR: opening green weight file'
   97 stop 'ERROR: writing green weight file'
   96 stop 'ERROR: closing green weight file'
      end
c
c ----- END OF dat_wgweight.f ----- 
