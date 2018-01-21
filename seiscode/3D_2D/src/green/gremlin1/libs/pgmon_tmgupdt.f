c this is <pgmon_tmgupdt.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1999, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c update tapered modified green monitor
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
c    02/07/99   V1.0   Thomas Forbriger
c
c==============================================================================
c
      subroutine pgmon_tmgupdt
c
c declare parameters
      include 'glq_dim.inc'
      include 'glq_pgmon.inc'
c
cE
c declare local variables
      integer maindevice
c 
      call pgqid(maindevice)
c 
      call pgslct(pgmon_tapmgreen)
      call pgpage
      call pg_tapgreen(0 ,di_mread, .true.)
c 
      call pgslct(maindevice)
      return
      end
c
c ----- END OF pgmon_tmgupdt.f -----
