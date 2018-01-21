c this is <par_wdec.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c decode model parameter weights from anonymous to named
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
c    14/01/99   V1.1   model definition changed (see glq_model.inc)
c                      now keep track of follow flag
c                      do not refuse to change depth of first section
c
      subroutine par_wdec
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
c 
cE
      integer iano, ipol, ipar, isec, spol
c 
c clear work space
      do ipol=1,glqm_mpol
        do ipar=1,glqm_mpar
          do isec=1,glqm_msec
            para_mweights(ipol, ipar, isec)=-10.
          enddo
        enddo
      enddo
c
c set up work space (named weights)
      iano=0
      do isec=1,glqm_nsec
        if (destim(isec)) then
c          if (isec.eq.1.) then
c            print *,'WARNING (par_wdec): refuse to use depth of top of model',
c     &        ' as inversion parameter'
c            para_mdweights(isec)=-1.
c          else
            iano=iano+1
            para_mdweights(isec)=mweight(iano)
c          endif
        else
          para_mdweights(isec)=-1.
        endif
      enddo
c
      do ipar=1,glqm_mpar
        do isec=1,glqm_nsec
          spol=1
          if (glqm_follow(isec,ipar)) spol=2
          do ipol=1,glqm_npol(isec,ipar)
            if ((mestim(isec, ipar)).and.(ipol.ge.spol)) then
              iano=iano+1
              para_mweights(ipol, ipar, isec)=mweight(iano)
            else
              para_mweights(ipol, ipar, isec)=-1.
            endif
          enddo
        enddo
      enddo
c 
      if (iano.ne.mod_n) stop 
     &   'ERROR (par_wdec): wrong number of anonymous parameters'
c
      return
      end
c
c ----- END OF par_wdec.f -----
