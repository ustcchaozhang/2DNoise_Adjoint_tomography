c this is <par_wsave.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c save named model parameter weights
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
c    07/04/98   V1.0   Thomas Forbriger
c
c==============================================================================
cS
c 
      subroutine par_wsave(filename, o)
c
c write model parameter weights to file
c
c filename:  name of file to use
c o:         overwrite original file
c
      character filename*(*)
      logical   o
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer lu
      parameter(lu=18)
      character comment*(79)
      parameter (comment='model parameter WEIGHTS')
c 
      if (verb_subaction) print *,'ENTER par_wsave(',filename,','
     &  ,o,')'
c
c open file
      if (o) then
        if (verb_io) print *,'NOTICE (par_wsave): writing weights to ',
     &          filename(1:index(filename, ' ')),
     &          ' - overwrite mode'
        open(lu, file=filename, err=99)
      else
        if (verb_io) print *,'NOTICE (par_wsave): writing weights to ',
     &          filename(1:index(filename, ' '))
        open(lu, file=filename, status='new', err=99)
      endif
c 
      if (verb_io) print *,comment
c 
      call par_wwrite(lu, comment)
c 
      close(lu, err=97)
      if (verb_io) print *,'NOTICE (par_wsave): file written and closed.'
c 
      if (verb_subaction) print *,'LEAVE par_wsave'
c 
      return
c
   99 stop 'ERROR (par_wsave): opening file'
   97 stop 'ERROR (par_wsave): closing file'
c
      end
c
c ----- END OF par_wsave.f -----
