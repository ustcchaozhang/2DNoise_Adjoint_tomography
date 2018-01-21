c this is <inv_dssd.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c
c==============================================================================
c
cS
c----------------------------------------------------------------------
c 
      subroutine inv_dssd
c
c calculate D°*S°*S*D
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
c 
cE
      integer im, jm, id
c 
      if (verb_subaction) print *,'ENTER inv_dssd'
c 
      if (verb_subaction) print *,'NOTICE (inv_dssd): calculate D°*S°*S*D'
c 
      do im=1,mod_n
        do jm=1,mod_n
          lq_dssd(im,jm)=(0.d0,0.d0)
          do id=1,dat_n
            lq_dssd(im,jm)=lq_dssd(im,jm)+lq_dss(id, im)*lq_d(id, jm)
c            print *,'DEBUG: im,jm,id ',im,jm,id,lq_dss(id, im),lq_d(id,jm)
          enddo
c          print *,'DEBUG: lq_dssd im,jm',im,jm,lq_dssd(im,jm)
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE inv_dssd'
c 
      return
      end
c
c ----- END OF inv_dssd.f -----
