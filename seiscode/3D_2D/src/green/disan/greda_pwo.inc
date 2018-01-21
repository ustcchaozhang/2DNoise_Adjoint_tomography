c this is <greda_pwo.inc>
c ----------------------------------------------------------------------------
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c data space for phasor walkout
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
c    16/09/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
c phasor walkout dataspace
c
c array of omega,p-pair selections
      integer pwo_nsel
c   omega,p-pairs
      real pwo_om(pwo_maxsel), pwo_p(pwo_maxsel)
c   output filenames
      character*(80) pwo_file(pwo_maxsel)
c   mark processed entries
      logical pwo_process(pwo_maxsel)
c
c lookup table
c   number of entries per frequency
      integer pwo_nlu
c 
c phasor array
      complex pwo_phasor(pwo_maxlu, maxtr)
c 
      integer pwo_lu(pwo_maxlu)
c 
      real pwo_pi2
      parameter(pwo_pi2=2.*3.1415926535897)
c 
c----------------------------------------------------------------------
c common blocks
      common /phasorwalkout/ pwo_p, pwo_om, pwo_file, pwo_lu,
     &  pwo_phasor, pwo_process, pwo_nsel, pwo_nlu
c
c ----- END OF greda_pwo.inc ----- 
