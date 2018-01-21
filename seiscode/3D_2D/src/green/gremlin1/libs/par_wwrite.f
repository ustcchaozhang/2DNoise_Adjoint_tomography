c this is <par_wwrite.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c write named model parameter weights
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
      subroutine par_wwrite(lu, comment)
c
c write model weights to file
c
c lu:        logical file unit to write to
c comment:   comment to be written to file
c
      integer   lu
      character comment*(*)
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
      include 'glq_para.inc'
c
cE
      integer j,k,l
c 
      if (verb_subaction) print *,'ENTER par_wwrite(',lu,',',comment,')'
c
c prepare
      call par_wdec
c 
      write(lu, 50, err=98) comment, glqm_nsec
c 
      do j=1,glqm_nsec
        do k=1,glqm_mpar
          if (glqm_follow(j,k)) glqm_npol(j,k)=-glqm_npol(j,k)
        enddo
        write(lu, 51, err=98) para_mdweights(j), j,
     &    (glqm_npol(j, k), k=1,glqm_mpar)
        do k=1,glqm_mpar
          if (glqm_follow(j,k)) glqm_npol(j,k)=-glqm_npol(j,k)
        enddo
        do k=1,glqm_mpol
          write(lu, 52, err=98) (para_mweights(k, l, j), l=1,glqm_mpar)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE par_wwrite'
c 
      return
c 
   50 format(a,//,i5,t20,'<-- number of sections',//
     &        t5,'Vp',t20,'Vs',t35,'density',t50,'Qp',t65,'Qs')
   51 format(/f12.4,t20,'<-- top of section ',i5,
     &       ' / polynomial expansion:',/
     &       5(i12,3x)/)
   52 format(3(f12.4,3x),2(f12.4,3x))
c
   98 stop 'ERROR (par_wwrite): writing file'
c
      end
c
c ----- END OF par_wwrite.f -----
