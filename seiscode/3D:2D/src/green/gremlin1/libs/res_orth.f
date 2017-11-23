c this is <res_orth.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c check scalar product between partial derivatives
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
c    09/04/98   V1.0   Thomas Forbriger
c    16/04/02   V1.1   normalizing factor was foolish
c
c==============================================================================
cS 
c 
      subroutine res_orth
c
c check scalar products of partial derivatives
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_model.inc'
      include 'glq_data.inc'
      include 'glq_inv.inc'
      include 'glq_verbose.inc'
c 
cE
      integer im,jm,id
      double precision prod,norm,jnorm
c 
      if (verb_subaction) print *,'ENTER res_orth'
c
      do im=1,mod_n
        do jm=im,mod_n
          prod=0.d0
          jnorm=0.d0
          do id=1,dat_n
            prod=prod+real(lq_d(id, im))*real(lq_d(id,jm))+
     &        imag(lq_d(id,im))*imag(lq_d(id,jm))
            jnorm=jnorm+real(lq_d(id, jm))*real(lq_d(id,jm))+
     &        imag(lq_d(id,jm))*imag(lq_d(id,jm))
          enddo
          jnorm=sqrt(jnorm)
          if (im.eq.jm) then
            print *,' '
            print *,'length of ',im,': sqrt(',prod,'), ',
     &              'rms: ',jnorm/dat_n
            norm=jnorm
          else
            print *,'scalar product of ',im,' with ',jm,':',prod,',',
     &              ' normalized: ',prod/(norm*jnorm)
          endif
        enddo
      enddo
c 
      if (verb_subaction) print *,'LEAVE res_orth'
c 
      return
      end
c
c ----- END OF res_orth.f -----
