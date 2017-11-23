c this is <dat_cref.f>
c------------------------------------------------------------------------------
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c PURPOSE
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
      logical function dat_cref()
c
c calculate reference model synthetics
c
c return value will be .true. on success
c 
      include 'glq_verbose.inc'
c
cE
      logical result, mod_prep
c 
      if (verb_subaction) print *,'ENTER dat_cref'
c 
      call mod_aclear
      result=mod_prep()
      if (result) call dat_synt(.true.)
c 
      dat_cref=result
c 
      if (verb_subaction) print *,'LEAVE dat_cref (result=',result,')'
c 
      return
      end
c
c ----- END OF dat_cref.f -----
