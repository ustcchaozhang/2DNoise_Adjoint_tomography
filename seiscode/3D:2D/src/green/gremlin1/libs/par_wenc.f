c this is <par_wenc.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c encode model parameter weights from named to anonymous
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
c    14/04/98   V1.1   do not use depth of top of model
c    13/01/99   V1.2   check for correct number of anonymous parameters
c                      (<=glqm_mano) is performed here now too - but you
c                      should still do it in the calling routines to provide
c                      sensible error handling
c    14/01/99   V1.3   keep track of follow flag as definition changed
c                      (see glq_model.inc)
c                      definition of depths has changed - do not refuse
c                      to change first sections depth
c
      subroutine par_wenc
c 
c doedit should be .true. if you want to change values
c
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
cE
      integer iano, ipol, ipar, isec, spol
      logical par_wcheck
c 
c check weights
      if (.not.(par_wcheck())) then
        print *,'WARNING (par_wenc): weights failed check!'
        print *,'WARNING (par_wenc): cant''t handle error condition here!'
        print *,
     &    'WARNING (par_wenc): (this should be done by the calling routine)'
        call mod_panic('ERROR (par_wenc): too many anonymous parameters')
      endif
c 
      iano=0
      do isec=1,glqm_nsec
        if (para_mdweights(isec).gt.0.) then
c          if (isec.eq.1) then
c            print *,'WARNING (par_wenc): refuses to use the depth of the',
c     &        ' top of the model as inversion parameter'
c            destim(isec)=.false.
c          else
            destim(isec)=.true.
            iano=iano+1
            mweight(iano)=para_mdweights(isec)
c          endif
        else
          destim(isec)=.false.
        endif
      enddo
c 
      do ipar=1,glqm_mpar
        do isec=1,glqm_nsec
          spol=1
          if (glqm_follow(isec,ipar)) spol=2
          do ipol=spol,glqm_npol(isec, ipar)
            if (para_mweights(ipol, ipar, isec).gt.0.) then
              mestim(isec,ipar)=.true.
            else
              mestim(isec,ipar)=.false.
            endif
          enddo
          if (mestim(isec, ipar)) then
            do ipol=spol,glqm_npol(isec,ipar)
              iano=iano+1
              if (para_mweights(ipol, ipar, isec).le.0.) then
                print *, 'WARNING (par_wenc): illegal parameter setting:'
                print *, 'WARNING (par_wenc): sec, par, pol ',
     &            isec, ipar, ipol
                print *, 'WARNING (par_wenc): setting value to 1.'
                print *,'NOTICE (par_wenc): all polynomial orders ',
     &                  'of on parameter in one section'
                print *,'NOTICE (par_wenc): have to be activated together'
                mweight(iano)=1.
              else
                mweight(iano)=para_mweights(ipol, ipar, isec)
              endif
            enddo
          endif
        enddo
      enddo
      mod_n=iano
c
      return
      end
c
c ----- END OF par_wenc.f -----
