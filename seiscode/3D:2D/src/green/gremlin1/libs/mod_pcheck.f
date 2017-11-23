c this is <mod_pcheck.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c check polynomial model section depth
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
      logical function mod_pcheck(mod)
c
c mod:   index of named polynomial model where to check section depths
c 
      integer mod
c 
      include 'glq_dim.inc'
      include 'glq_model.inc'
      include 'glq_para.inc'
      include 'glq_verbose.inc'
c 
cE
      integer sec
      logical result
c
      if (verb_subaction) print *,'ENTER mod_pcheck(',mod,')'
c
      result=.true.
      sec=2
      do while ((result).and.(sec.le.glqm_nsec))
        if (mdepth(sec, mod).le.mdepth(sec-1, mod)) result=.false.
        sec=sec+1
      enddo
c 
      mod_pcheck=result
c 
      if ((.not.(result)).and.(verb_subresult)) print *,
     &  'NOTICE (mod_pcheck): polynomial model ',mod,' failed depth ',
     &  'feasibility check'
c
      if (verb_subaction) print *,'LEAVE mod_pcheck (result=',result,')'
c 
      return
      end
c
c ----- END OF mod_pcheck.f -----
