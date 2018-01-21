c this is <grepg_dim.inc>
c------------------------------------------------------------------------------
c
c 25/11/98 by Thomas Forbriger (IfG Stuttgart)
c
c set array dimensions
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
c    25/11/98   V1.0   Thomas Forbriger
c    21/01/02   V1.1   now use different dimension for read picks
c                      is useful for flspherplot
c    19/09/02   V1.2   provide magic numbers - for coherency
c
c==============================================================================
c
      integer maxpicktypes, maxpicks, maxpickdim

      parameter(maxpicktypes=10, maxpicks=2000, maxpickdim=7*maxpicktypes)
c
c magic numbers
      character*4 cmagic, wcmagic, pcmagic, gcmagic, kcmagic
c
c original green spectra
      parameter(cmagic='1234')
c
c data taper weights for gremlin (real numbers)
      parameter(wcmagic='123S')
c 
c p,p-resolution as created by grereso
      parameter(pcmagic='123P')
c 
c gabor matrix as created by gabor
      parameter(gcmagic='123G')
c 
c wavenumber spectrum as taken from flgevas
      parameter(kcmagic='123K')
c
c ----- END OF grepg_dim.inc -----
