c this is <mod_identify.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c identify anonymous model parameter
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
c    08/04/98   V1.0   Thomas Forbriger
c    14/01/99   V1.1   model definition changed (see glq_model.inc)
c                      now consider follow flag
c    25/01/99   V1.1   changed depth to thickness
c
      subroutine mod_identify(mano, msec, mpol, mpar, parname)
c 
c input:
c   mano:     anonymous model parameter to identify
c 
c output:
c   msec:     section within model
c   mpar:     parameter index
c   mpol:     polynomial order (array index)
c   parname:  parameter name
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
      integer mano, mpol, msec, mpar
      character parname*(*)
c 
cE
c
      integer ipol, isec, ipar, iano
c 
      if (verb_subaction) print *,'ENTER mod_identify'
c
c identify model parameter
      iano=0
      mpol=0
      mpar=0
      msec=0
      do isec=1,glqm_nsec
        if (destim(isec)) then
          iano=iano+1
          if (iano.eq.mano) then
            msec=isec
            mpol=1
            mpar=mi_depth
          endif
        endif
      enddo
c
      do ipar=1,glqm_mpar
        do isec=1,glqm_nsec
          if (mestim(isec, ipar)) then
            do ipol=1,glqm_npol(isec,ipar)
              if (.not.(glqm_follow(isec,ipar).and.(ipol.eq.1))) then
                iano=iano+1
                if (iano.eq.mano) then
                  msec=isec
                  mpol=ipol
                  mpar=ipar
                endif
              endif
            enddo
          endif
        enddo
      enddo
c
      if (mpar.eq.mi_depth) then
c        parname='depth'
        parname='thickness'
      elseif (mpar.eq.mi_alpha) then
        parname='p-velocity'
      elseif (mpar.eq.mi_beta) then
        parname='s-velocity'
      elseif (mpar.eq.mi_Qalpha) then
        parname='Qp'
      elseif (mpar.eq.mi_Qbeta) then
        parname='Qs'
      elseif (mpar.eq.mi_density) then
        parname='density'
      else
        parname=' '
        print *,'WARNING (mod_identify): could not find specified parameter'
      endif
c 
      if (verb_subaction) print *,'LEAVE mod_identify'
c 
      return
      end
c
c ----- END OF mod_identify.f -----
