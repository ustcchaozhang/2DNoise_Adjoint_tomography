c this is <mod_save.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c    24/03/98   V1.0   Thomas Forbriger
c
c==============================================================================
c
c
cS
c----------------------------------------------------------------------
c 
      subroutine mod_save(filename, i, o, comment)
c
c write model to file
c
c filename:  name of file to use
c i:         model index within array
c o:         overwrite original file
c comment:   comment to be written to file
c
      character filename*(*)
      integer   i
      logical   o
      character comment*(*)
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer lu
      parameter(lu=18)
c 
      if (verb_subaction) print *,'ENTER mod_save(',filename,','
     &  ,i,',',o,',',comment,')'
c
c open file
      if (o) then
        if (verb_io) print *,'NOTICE (mod_save): writing model ',i,' to ',
     &          filename(1:index(filename, ' ')),
     &          ' - overwrite mode'
        open(lu, file=filename, err=99)
      else
        if (verb_io) print *,'NOTICE (mod_save): writing model ',i,' to ',
     &          filename(1:index(filename, ' '))
        open(lu, file=filename, status='new', err=99)
      endif
c 
      if (verb_io) print *,comment
c 
      call mod_write(lu, i, comment)
c 
      close(lu, err=97)
      if (verb_io) print *,'NOTICE (mod_save): file written and closed.'
c 
      if (verb_subaction) print *,'LEAVE mod_save'
c 
      return
c
   99 stop 'ERROR (mod_save): opening file'
   97 stop 'ERROR (mod_save): closing file'
c
      end
c
c ----- END OF mod_save.f -----
