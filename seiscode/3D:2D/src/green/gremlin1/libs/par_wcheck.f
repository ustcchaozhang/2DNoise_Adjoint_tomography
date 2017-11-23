c this is <par_wcheck.f>
c------------------------------------------------------------------------------
cS
c $Id$
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c check number of named model parameter weights
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
c    14/01/99   V1.1   keep track of follow flag as model definition
c                      changed (see glq_model.inc)
c
      logical function par_wcheck()
c 
c returns .true. if number of set values is ok
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
cE
      integer iano, ipol, ipar, isec, spol
      logical result, thissec
c 
      iano=0
      do isec=1,glqm_nsec
        if (para_mdweights(isec).gt.0.) iano=iano+1
      enddo
c
      do ipar=1,glqm_mpar
        do isec=1,glqm_nsec
          thissec=.false.
          do ipol=1,glqm_npol(isec, ipar)
            if (para_mweights(ipol, ipar, isec).gt.0.) thissec=.true.
          enddo
          if (thissec) then
            spol=1
            if (glqm_follow(isec,ipar)) spol=2
            do ipol=spol,glqm_npol(isec,ipar)
              iano=iano+1
              if (para_mweights(ipol, ipar, isec).le.0.) then
                print *, 'WARNING (par_wcheck): illegal parameter setting:'
                print *, 'WARNING (par_wcheck): sec, par, pol ',
     &            isec, ipar, ipol
                print *,'NOTICE (par_wcheck): all polynomial orders ',
     &                  'of one parameter in one section'
                print *,'NOTICE (par_wcheck): have to be activated together'
              endif
            enddo
          endif
        enddo
      enddo
c enddo last check loop (glqm_mpar)
      if (iano.gt.glqm_mano) then
        print *,'WARNING (par_wcheck): too many free parameters ',
     &    'are activated: ',iano,'.'
        print *,'WARNING (par_wcheck): you may activate a maximum of ',
     &    glqm_mano, ' at once.'
        result=.false.
      else
        result=.true.
      endif
c 
      par_wcheck=result
      return
      end
c
c ----- END OF par_wcheck.f -----
