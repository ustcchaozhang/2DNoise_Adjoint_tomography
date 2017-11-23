c this is <par_wread.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c read named model parameter weights from file
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
c    14/01/99   V1.1   model definition changed (see glq_model.inc)
c                      keep track of follow flag
c    11/02/10   V1.2   correction of read statement to skip line
c
      subroutine par_wread(filename)
c 
c read named model parameter weights from file
c
c filename:  name of file to use
c
      character filename*(*)
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
      include 'glq_para.inc'
c
cE
      character*79 comment
      integer lu, j, k, l
      parameter(lu=18)
      logical par_wcheck
      integer insec
c 
      if (verb_subaction) print *,'ENTER par_wread(',filename,')'
c 
      if (verb_io) print *,'NOTICE (par_wread): reading weights from ',
     &  filename(1:index(filename, ' '))
c 
      open(lu, file=filename, status='old', err=99)
c 
      read(lu, '(a79)', err=98, end=97) comment
      if (verb_io) print *,comment
      read(lu, '(/i15//)', err=98, end=97) insec
      if (insec.lt.glqm_nsec) stop
     &  'ERROR (par_wread): too few model sections'
      if (verb_io) print *,
     &   'NOTICE (par_wread): reading ',glqm_nsec,' model sections...'
      do j=1,glqm_nsec
        read(lu, '(/f15.7)', err=98, end=97) para_mdweights(j)
        read(lu, '(1x)', err=98, end=97) 
        read(lu, '(1x)', err=98, end=97) 
        do k=1,glqm_mpol
          read(lu, fmt=*, err=98, end=97) 
     &     (para_mweights(k, l, j), l=1,glqm_mpar)
        enddo
      enddo
c 
      do j=1,glqm_nsec
        do k=1,glqm_mpar
          if (glqm_npol(j,k).lt.glqm_mpol) then
            do l=glqm_npol(j, k)+1,glqm_mpol
              para_mweights(l, k, j)=-1.
            enddo
          endif
          if (glqm_follow(j,k)) para_mweights(1, k, j)=-1.
        enddo
      enddo
c 
      close(lu, err=96)
      if (verb_io) print *,'NOTICE (par_wread): file read and closed.'
c 
c check weights
      if (par_wcheck()) then
        call par_wenc
      else
        print *,'WARNING (par_wread): weights failed check!'
        print *,'NOTICE (par_wread): ignoring read values'
      endif
c 
      if (verb_subaction) print *,'LEAVE par_wread'
c 
      return
   99 stop 'ERROR (par_wread): opening file'
   98 stop 'ERROR (par_wread): reading file'
   97 stop 'ERROR (par_wread): reading file - unexpected end'
   96 stop 'ERROR (par_wread): closing file'
      end
c
c ----- END OF par_wread.f -----
