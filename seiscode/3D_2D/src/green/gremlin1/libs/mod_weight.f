c this is <mod_weight.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c set anonymous parameter weights
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
c    14/01/99   V1.1   model definition changed (see glq_model.inc)
c                      keep track of follow flag
c
      subroutine mod_weight(mode)
c
c mode=0:       all weights are 1.
c 1<=mode<=3:   increase weight of polynomial order mode by factor 2.
c 11<=mode:     increase weight of section mode-10 by factor 2.
c
      integer mode
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_verbose.inc'
c
cE
      integer i, isec, ipar, ipol
c 
      if (verb_subaction) print *,'ENTER mod_weight(',mode,')'
c 
      if (verb_subaction) print *,
     &  'NOTICE (mod_weight): set anonymous parameter weights in mode ',
     &  mode
c
      if (mode.eq.0) then
        do i=1,glqm_mano
          mweight(i)=1.
        enddo
      elseif ((mode.ge.1).and.(mode.le.3)) then
        i=0
        do isec=1,glqm_nsec
          if (destim(isec)) i=i+1
        enddo
        do ipar=1,glqm_mpar
          do isec=1,glqm_nsec
            if (mestim(isec, ipar)) then
              do ipol=1,glqm_npol(isec,ipar)
                if (.not.(glqm_follow(isec, ipar).and.(ipol.eq.1))) then
                  i=i+1
                  if (ipol.eq.mode) then
                    mweight(i)=mweight(i)*2.
                  endif
                endif
              enddo
            endif
          enddo
        enddo
        if (i.ne.mod_n) 
     &    call mod_panic('ERROR (mod_weight): wrong number of anonymous parameters')
      elseif (mode.gt.10) then
        i=0
        do isec=1,glqm_nsec
          if (destim(isec)) i=i+1
          if (isec.eq.(mode-10)) mweight(i)=mweight(i)*10.
        enddo
        do ipar=1,glqm_mpar
          do isec=1,glqm_nsec
            if (mestim(isec, ipar)) then
              do ipol=1,glqm_npol(isec,ipar)
                if (.not.(glqm_follow(isec, ipar).and.(ipol.eq.1))) then
                  i=i+1
                  if (isec.eq.(mode-10)) then
                    mweight(i)=mweight(i)*2.
                  endif
                endif
              enddo
            endif
          enddo
        enddo
        if (i.ne.mod_n) 
     &    call mod_panic('ERROR (mod_weight): wrong number of anonymous parameters')
      else
        call mod_panic('ERROR (mod_weight): unknown mode')
      endif
c 
      if (verb_subaction) print *,'LEAVE mod_weight'
c
      return
      end
c
c ----- END OF mod_weight.f -----
