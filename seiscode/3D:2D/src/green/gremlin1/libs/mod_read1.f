c this is <mod_read1.f>
c this was <mod_read.f>
c------------------------------------------------------------------------------
cS
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
c read a named model from file
c
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    14/01/99   V1.1   just keep old version
c
      subroutine mod_read1(filename, i)
c 
c
c filename:  name of file to use
c i:         model index within array
c
      character filename*(*)
      integer i
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      character*79 comment
      integer lu, j, k, l
      parameter(lu=18)
c 
      if (verb_subaction) print *,'ENTER mod_read1(',filename,',',i,')'
c 
      if (verb_io) print *,'NOTICE (mod_read1): reading model from ',
     &  filename(1:index(filename, ' ')),' to ',i
c 
      open(lu, file=filename, status='old', err=99)
c 
      read(lu, '(a79)', err=98, end=97) comment
      if (verb_io) print *,comment
      read(lu, '(/i15//)', err=98, end=97) glqm_nsec
      if (verb_io) print *,
     &   'NOTICE (mod_read1): reading ',glqm_nsec,' model sections...'
      do j=1,glqm_nsec
        read(lu, '(/f15.7)', err=98, end=97) mdepth(j, i)
        read(lu, fmt=*, err=98, end=97) (glqm_npol(j, k), k=1,glqm_mpar)
        do k=1,glqm_mpar
          if (glqm_npol(j,k).lt.0) then
            glqm_npol(j,k)=-glqm_npol(j,k)
            glqm_follow(j,k)=.true.
          else
            glqm_follow(j,k)=.false.
          endif
          if (glqm_npol(j, k).lt.1) 
     &      stop 'ERROR (mod_read1): need at least constant value'
          if (glqm_npol(j, k).gt.glqm_mpol) 
     &      stop 'ERROR (mod_read1): too many coefficients!'
        enddo
        read(lu, '(1x)', err=98, end=97) comment
        do k=1,glqm_mpol
          read(lu, fmt=*, err=98, end=97) (model(k, j, l, i), l=1,glqm_mpar)
        enddo
      enddo
c 
      do j=1,glqm_nsec
        do k=1,glqm_mpar
          if (glqm_npol(j,k).lt.glqm_mpol) then
            do l=glqm_npol(j, k)+1,glqm_mpol
              model(l, j, k, i)=0.
            enddo
          endif
        enddo
      enddo
c 
      close(lu, err=96)
      if (verb_io) print *,'NOTICE (mod_read1): file read and closed.'
c 
c make section parameters follow
      call mod_follow(i)
c 
      if (verb_subaction) print *,'LEAVE mod_read1'
c 
      return
   99 stop 'ERROR (mod_read1): opening file'
   98 stop 'ERROR (mod_read1): reading file'
   97 stop 'ERROR (mod_read1): reading file - unexpected end'
   96 stop 'ERROR (mod_read1): closing file'
      end
c
c ----- END OF mod_read.f -----
c
c ----- END OF mod_read1.f -----
