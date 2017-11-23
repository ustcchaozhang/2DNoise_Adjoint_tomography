c this is <pg_gdata.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot green data (at set of four panels)
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
      subroutine pg_gdata
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_data.inc'
      include 'glq_verbose.inc'
c 
cE
      real ismin, ismax, ifmin, ifmax
c 
      if (verb_subaction) print *,'ENTER pg_gdata'
c 
      call dat_dmode
      call pgpage
      call pg_green(8, di_read)
      call pg_green(9, di_mread)
      ismin=dat_slo(rng_smin)
      ismax=dat_slo(rng_smax)
      ifmin=dat_fre(rng_fmin)
      ifmax=dat_fre(rng_fmax)
      val_smin=dat_slo(1)
      val_fmin=dat_fre(1)
      val_smax=dat_slo(data_nslo)
      val_fmax=dat_fre(data_nfre)
      call dat_dmode
      call pg_green(10, di_read)
      call pg_green(11, di_mread)
      val_smin=ismin
      val_smax=ismax
      val_fmin=ifmin
      val_fmax=ifmax
      call dat_dmode
c 
      if (verb_subaction) print *,'LEAVE pg_gdata'
c 
      return
      end
c
c ----- END OF pg_gdata.f -----
