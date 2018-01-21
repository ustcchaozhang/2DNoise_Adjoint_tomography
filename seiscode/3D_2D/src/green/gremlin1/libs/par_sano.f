c this is <par_sano.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c set anonymous parameter mode
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
c    25/03/98   V1.1   now check glqm_mano limit
c    13/01/99   V2.0   model definition changed - there is no parameter
c                      chop_hs anymore - lines wth check for chop_hs are
c                      commented now (see glq_model.inc)
c    14/01/99   V2.1   keep track of follow flag
c
      subroutine par_sano(mode, ref)
c
c
c mode=0:     clear all definitions
c 0<mode<7:   set named parameter to anonymous list
c ref:        index of reference model (leads to the "top-of-bottom-
c             halfspace-decision")
c 
c the total number of anonymous parameters will be calculated
c and all anonymous parameters will be cleared
c
      integer mode, ref
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
c
cE
      integer i,j,npol
c
      if (mode.eq.0) then
        print *,'NOTICE (par_sano): clear all'
        do i=1,glqm_msec
          destim(i)=.false.
          do j=1,glqm_mpar
            mestim(i,j)=.false.
          enddo
        enddo
      elseif (mode.eq.mi_depth) then
        print *,'NOTICE (par_sano): set depth'
        do i=1,glqm_nsec
          destim(i)=.true.
        enddo
      elseif ((mode.ge.1).and.(mode.le.glqm_mpar)) then
        print *,'NOTICE (par_sano): set index ',mode
        do i=1,glqm_nsec
          mestim(i, mode)=.true.
        enddo
      else
        call mod_panic('ERROR (mod_sano): unknown mode')
      endif
c 
c exclude anything below top of bottom halfspace
c 13/01/99 no chop_hs anymore...
c      do i=1,glqm_nsec
c        if (mdepth(i, ref).ge.chop_hs) then
c          destim(i)=.false.
c          do j=1,glqm_mpar
c            mestim(i,j)=.false.
c          enddo
c        endif
c      enddo
c 
c calculate number of anonymous parameters
      mod_n=0
      do i=1,glqm_nsec
        if (destim(i)) mod_n=mod_n+1
        do j=1,glqm_mpar
          npol=glqm_npol(i,j)
          if (glqm_follow(i,j)) npol=max(1,npol-1)
          if (mestim(i,j)) mod_n=mod_n+npol
        enddo
      enddo
c 
c check maximum and correct if too many are active
      if (mod_n.gt.glqm_mano) then
        print *,'WARNING (par_sano): there are ',mod_n,
     &    ' parameters active now.'
        print *,'WARNING (par_sano): but a maximum of ',glqm_mano,
     &    ' parameters is allowed.'
        if (mode.eq.0) then
          call mod_panic('ERROR (par_sano): mode was 0 - '//
     &      'check where all these active come from!')
        elseif (mode.eq.mi_depth) then
          print *,'NOTICE (par_sano): deactivate depth'
          do i=1,glqm_nsec
            destim(i)=.false.
          enddo
        elseif ((mode.ge.1).and.(mode.le.glqm_mpar)) then
          print *,'NOTICE (par_sano): deactivate index ',mode
          do i=1,glqm_nsec
            mestim(i, mode)=.false.
          enddo
        else
          call mod_panic('ERROR (mod_sano): unknown mode')
        endif
c 
c calculate number of anonymous parameters again
        mod_n=0
        do i=1,glqm_nsec
          if (destim(i)) mod_n=mod_n+1
          do j=1,glqm_mpar
            npol=glqm_npol(i,j)
            if (glqm_follow(i,j)) npol=max(1,npol-1)
            if (mestim(i,j)) mod_n=mod_n+npol
          enddo
        enddo
      endif
      print *,'NOTICE (par_sano): ',mod_n,' are active'
c 
c clear anonymous parameters
      do i=1,mod_n
        mdelta(i)=0.
      enddo
c
      return
      end
c
c ----- END OF par_sano.f -----
