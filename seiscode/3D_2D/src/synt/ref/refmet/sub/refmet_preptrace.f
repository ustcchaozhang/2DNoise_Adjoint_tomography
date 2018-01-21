c this is <sub/refmet_preptrace.f>
c ----------------------------------------------------------------------------
c   ($Id$)
c
c Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
c Copyright (c) 1997 by Thomas Forbriger (IfG Stuttgart) 
c
c prepare datasets to be written with SFF subroutines
c
c ============================================================================
c
c this is part of the REFMET reflectivity program
c (for comments and revisions see also the main source refmet.f)
c
c======================================================================
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
c 
c----------------------------------------------------------------------
c 
c prepare output dataset from two traces
c
      subroutine addtraces(I1, I2, O, NS, E, MS, ME)
c 
      integer NS, E, MS, ME
      complex*16 I1(MS, ME), I2(MS, ME)
      real O(MS)
c 
      integer S
c 
      do S=1,NS
        O(S)=real(I1(S,E))+real(I2(S,E))
      enddo
      return
      end

c----------------------------------------------------------------------
c 
c prepare output dataset from one trace
c
      subroutine copytrace(I, O, NS, E, MS, ME)
c 
      integer NS, E, MS, ME
      complex*16 I(MS, ME)
      real O(MS)
c 
      integer S
c 
      do S=1,NS
        O(S)=real(I(S, E))
      enddo
      return
      end

c
c ----- END OF sub/refmet_preptrace.f ----- 
