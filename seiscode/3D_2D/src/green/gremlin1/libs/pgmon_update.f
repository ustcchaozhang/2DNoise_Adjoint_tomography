c this is <pgmon_update.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c update pg-monitor devices
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
c    02/07/99   V1.1   added rgupdt
c
c==============================================================================
c
      subroutine pgmon_update(level)
c
c declare parameters
c 
c level=pgmon_l_outer     update outer iteration data
c level=pgmon_l_inner     update inner iteration data
c 
      integer level
c 
      include 'glq_pgmon.inc'
c
cE
c declare local variables
c
c------------------------------------------------------------------------------
c go
      if (level.eq.pgmon_l_outer) then
        if (pgmon_tapogreen.gt.0) call pgmon_togupdt
        if (pgmon_tapmgreen.gt.0) call pgmon_tmgupdt
        if (pgmon_taprgreen.gt.0) call pgmon_trgupdt
      elseif (level.eq.pgmon_l_inner) then
c dummy action as long as there are no inner interation monitors defined
        if (pgmon_taprgreen.gt.0) call pgmon_trgupdt
      else
        print *,'WARNING (pgmon_update): unknown level'
      endif
c
      return
      end
c
c ----- END OF pgmon_update.f -----
