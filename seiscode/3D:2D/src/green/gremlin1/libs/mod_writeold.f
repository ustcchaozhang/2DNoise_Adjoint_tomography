c this is <mod_writeold.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c write old model format
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
c we assume to receive the recent format
c
c REVISIONS and CHANGES
c    03/03/99   V1.0   Thomas Forbriger
c 
      subroutine mod_writeold(lu, i, comment)
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
      real p(glqm_mpar),a(glqm_mpol)
      integer j,k,l,poli(glqm_mpar)
      real oldmod(glqm_mpol, glqm_mpar), tos, x
c 
      print *,'WARNING: you are writing an old model format!'
      if (verb_subaction) print *,'ENTER mod_writeold(',lu,',',i,',',comment,')'
c
c 
      write(lu, 50, err=98) comment, 'OLD MODEL FORMAT!', glqm_nsec+1
c 
      do j=1,glqm_nsec
c 
c back transform
        tos=0.
        if (j.gt.1) tos=mdepth(j-1, i)
        x=-0.5*(mdepth(j,i)-tos)
c        print *,'trans ',j,tos,mdepth(j,i),x
        do k=1,glqm_mpar
          oldmod(1,k)=model(1, j, k, i)+
     &                model(2, j, k, i)*x+model(3, j, k, i)*x*x
          oldmod(2,k)=model(2, j, k, i)+
     &                2.*model(3, j, k, i)*x
          oldmod(3,k)=model(3, j, k, i)
c          print *,(oldmod(l,k),l=1,glqm_mpol)
        enddo
c 
        if (j.lt.glqm_nsec) then
c          print *,'write ',j
          do k=1,glqm_mpar
            poli(k)=glqm_npol(j,k)
            if (glqm_follow(j+1,k)) poli(k)=min(-1,1-poli(k))
          enddo
          if (j.gt.1) then
            write(lu, 51, err=98) mdepth(j-1, i), j, 
     &        (poli(k), k=1,glqm_mpar)
          else
            write(lu, 51, err=98) 0., j, 
     &        (poli(k), k=1,glqm_mpar)
          endif
          do k=1,glqm_mpol
            write(lu, 52, err=98) (oldmod(k, l), l=1,glqm_mpar)
          enddo
        else
c          print *,'finish ',j
          write(lu, 51, err=98) mdepth(j-1, i), j, 
     &      (min(-1,1-glqm_npol(j, k)), k=1,glqm_mpar)
          do k=1,glqm_mpol
            write(lu, 52, err=98) (oldmod(k, l), l=1,glqm_mpar)
          enddo
          do k=1,glqm_mpar
            call mod_eval(i, k, sngl(mdepth(j,i)), a)
            p(k)=a(1)
          enddo
          write(lu, 51, err=98) mdepth(j, i), j+1, 
     &      (1, k=1,glqm_mpar)
          write(lu, 52, err=98) (p(k), k=1,glqm_mpar)
          do k=2,glqm_mpol
            write(lu, 52, err=98) (0., l=1,glqm_mpar)
          enddo
        endif
      enddo
c 
      if (verb_subaction) print *,'LEAVE mod_writeold'
c 
      return
c 
   50 format(a,/a,/,i5,t20,'<-- number of sections',//
     &        t5,'Vp',t20,'Vs',t35,'density',t50,'Qp',t65,'Qs')
   51 format(/f12.4,t20,'<-- top of section ',i5,
     &       ' / polynomial expansion:',/
     &       5(i12,3x)/)
   52 format(3(f12.9,3x),2(f12.4,3x))
c
   98 stop 'ERROR (mod_writeold): writing file'
c
      end
c
c ----- END OF mod_writeold.f -----
