c this is <pg_fullset.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot a full set containig model and synthetic data
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
c    02/07/99   V1.1   update monitors
c
c----------------------------------------------------------------------
c
      subroutine pg_fullset(isref)
c
c plot a full set containig model and synthetic data
c
c chops model, calculates synthetics and goes for a plot
c if isref is .true. the reference model will be calculated
c 
      logical isref
c 
      include 'glq_dim.inc'
      include 'glq_verbose.inc'
      include 'glq_pgmon.inc'
c
cE   
      logical result, mod_pcheck, mod_check
      integer mindex, dindex
c 
      if (verb_subaction) print *,'ENTER pg_fullset(',isref,')'
c 
      if (isref) then
        mindex=mb_ref
        dindex=di_mref
      else
        mindex=mb_work
        dindex=di_mcalc
      endif
c 
      result=mod_pcheck(mindex)
      if (result) then
        call mod_chop(mindex)
        result=mod_check()
        if (result) then
          call pgpage
          call dat_dmode
          call dat_synt(isref)
          call pg_mod(mindex)
          call pg_tt(4, isref)
          call pg_green(5, dindex)
        else
          print *,'WARNING (pg_fullset): discrete model failed check'
        endif
      else
        print *,'WARNING (pg_fullset): polynomial model failed check'
      endif
c  update monitor devices
      call pgmon_update(pgmon_l_outer)
c 
      if (verb_subaction) print *,'LEAVE pg_fullset'
c 
      return
      end
c
c ----- END OF pg_fullset.f -----
