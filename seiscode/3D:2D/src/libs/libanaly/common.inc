c this is <common.inc.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
c
c common block - used to pass parameters from interface routines
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
c    04/07/2002   V1.0   Thomas Forbriger
c
c ============================================================================
c
      integer maxlay
      parameter(maxlay=100)
      double precision slowness, srcamp, srcdep
      integer srctyp
      integer mnlay
      double precision mdepth(maxlay), malpha(maxlay), mbeta(maxlay)
      double precision mdensity(maxlay)
      double precision mQalpha(maxlay), mQbeta(maxlay)

      common /interfacevalues/ slowness, srcamp, srcdep, srctyp
      common /model/ mdepth, malpha, mbeta, mdensity, 
     &               mQalpha, mQbeta, mnlay
c
c ----- END OF common.inc.f ----- 
