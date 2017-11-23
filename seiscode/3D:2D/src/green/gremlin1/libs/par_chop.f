c this is <par_chop.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c read or set chopping parameters
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
c    14/01/99   V2.0   model definition changed (see glq_model.inc)
c                      there is no more chop_hs - so we have to
c                      change the argument list
c
c      subroutine par_chop(set, step, halfspace)
      subroutine par_chop(set, step)
c
c set=.true.:     set parameters
c set=.false.:    read parameters
c step:           maximum relative step for discrete model parameter
c halfspace:      top of bottom halfspace
c 
      logical set
      real step
c 
      include 'glq_dim.inc'
      include 'glq_para.inc'
c 
cE
      if (set) then
        chop_step=step
c        chop_hs=halfspace
      else
        step=chop_step
c        halfspace=chop_hs
      endif
c 
      return
      end
c
c ----- END OF par_chop.f -----
