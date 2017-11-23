c this is <mod_write.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c write a polynomial model to file
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
c    13/01/99   V1.1   write model version identifier to mark new files
c                      (read comments in glq_model.inc)
c                      changed format lines 50 and 51
c    07/05/02   V1.2   changed output format to suit non-constrained
c                      parameters in resolution analysis
c 
      subroutine mod_write(lu, i, comment)
c
c write model to file
c
c lu:        logical file unit to write to
c i:         model index within array
c comment:   comment to be written to file
c
      integer   i, lu
      character comment*(*)
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_strings.inc'
      include 'glq_verbose.inc'
c
cE
      integer j,k,l
c 
      if (verb_subaction) print *,'ENTER mod_write(',lu,',',i,',',comment,')'
c
c 
      write(lu, 50, err=98) comment, mod_version2, glqm_nsec
c 
      do j=1,glqm_nsec
        do k=1,glqm_mpar
          if (glqm_follow(j,k)) glqm_npol(j,k)=-glqm_npol(j,k)
        enddo
        write(lu, 51, err=98) mdepth(j, i), j, (glqm_npol(j, k), k=1,glqm_mpar)
        do k=1,glqm_mpar
          if (glqm_follow(j,k)) glqm_npol(j,k)=-glqm_npol(j,k)
        enddo
        do k=1,glqm_mpol
          write(lu, 52, err=98) (model(k, j, l, i), l=1,glqm_mpar)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE mod_write'
c 
      return
c 
   50 format(a,//a,/,i5,t20,'<-- number of sections',//
     &        t5,'Vp',t20,'Vs',t35,'density',t50,'Qp',t65,'Qs')
   51 format(/f12.4,t20,'<-- bottom of section ',i5,
     &       ' / polynomial expansion:',/
     &       5(i12,3x)/)
   52 format(3(g12.4,3x),2(f12.4,3x))
c
   98 stop 'ERROR (mod_write): writing file'
c
      end
c
c ----- END OF mod_write.f -----
