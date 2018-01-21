c this is <mod_track.f>
c------------------------------------------------------------------------------
cS
c   ($Id$)
c
c Copyright 2000, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c let vp track vs
c
c REVISIONS and CHANGES
c    11/04/2000   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine mod_track(imod)
c
c imod:     index of model space to modify
c
      integer imod
c
c declare parameters
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c
cE
c declare local variables
      character*(*) mod_track_id
      parameter (mod_track_id='$Id$')
      integer i,j
c
c------------------------------------------------------------------------------
c go
      if (vptrackfactor.gt.0.d0) then
        if (verb_subaction)
     &    print *,'NOTICE (mod_track): track vp model in ',imod,
     &            ' at factor ',vptrackfactor
        do j=1,glqm_nsec
          glqm_npol(j, mi_alpha)=glqm_npol(j, mi_beta)
          do i=1,glqm_mpol
            model(i, j, mi_alpha, imod)=
     &        model(i, j, mi_beta, imod)*vptrackfactor
          enddo
        enddo
      endif
c
      return
c the following line prevents the linker from removing the ID string
   99 print *, mod_track_id
      end
c
c ----- END OF mod_track.f -----
