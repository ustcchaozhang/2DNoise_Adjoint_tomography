c this is <mops.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2011 by Thomas Forbriger (IfG Stuttgart)
c
c MOdel P to model S with factor 1./sqrt(3)
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
c    30/01/98   V1.0   Thomas Forbriger
c    17/11/10   V1.1   use correct include path
c
c==============================================================================
c
      program mops

      character*79 version
      parameter(version=
     &  'MOPS   V1.1    MOdel P to model S with factor 1./sqrt(3)')

      integer iargc, npol, i, j
      real factor
      character*80 modelname

      include '../libs/glq_dim.inc'
      include '../libs/glq_model.inc'

      print *,version
      if (iargc().ne.1) then
        print *,'Usage: mops model'
        print *,' '
        print *,'original file will be changed'
        stop 'ERROR: arguments?'
      endif

      call getarg(1, modelname)

      factor=sqrt(1./3.)

      call mod_read(modelname, 1)
      print *,' '
c apply changes
      do i=1,glqm_nsec
        npol=glqm_npol(i, mi_alpha)
        glqm_npol(i, mi_beta)=npol
        glqm_follow(i, mi_beta)=glqm_follow(i, mi_alpha)
        do j=1,npol 
          model(j, i, mi_beta, 1)=model(j, i, mi_alpha, 1)*factor
        enddo
      enddo
      call mod_follow(1)
c 
      call mod_save(modelname, 1, .true., version)

      stop
      end
c
c ----- END OF mops.f -----
