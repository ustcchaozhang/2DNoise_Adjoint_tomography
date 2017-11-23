c this is <par_setdefpgmon.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c just set pg-monitor devices to 'not opened'
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
c    01/07/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine par_setdefpgmon

      include 'glq_pgmon.inc'

cE
      pgmon_tapogreen=-1.
      pgmon_tapmgreen=-1.
      pgmon_taprgreen=-1.

      return
      end
c
c ----- END OF par_setdefpgmon.f -----
