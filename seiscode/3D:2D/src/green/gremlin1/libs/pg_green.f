c this is <pg_green.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c plot green matrix amplitudes in current datamode
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
c    20/01/99   V1.1   - added title switching and character height
c    02/07/99   V1.2   - now is just interface to pg_tapgreen
c 
      subroutine pg_green(ivp, di)
c
c ivp:          viewport to use
c di:           dataindex
c               for di>2 X2-values will be calculated
c
      integer ivp, di
c 
      call pg_tapgreen(ivp, di, .false.)
c
      return
      end
c
c ----- END OF pg_green.f -----
