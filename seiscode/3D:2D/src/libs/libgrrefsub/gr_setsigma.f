c this is <gr_setsigma.f>
cS
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
c set imaginary part of frequency
c
c REVISIONS and CHANGES
c    26/03/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      subroutine gr_setsigma(sigma)
c
      double precision sigma
c
cE
      include 'gr_rtc.inc'
      om_sigma=sigma
      freq_is_complex=.true.
      return
      end
c
c ----- END OF gr_setsigma.f ----- 
