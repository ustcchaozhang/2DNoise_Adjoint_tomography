c this is <par_setsrc.f>
c------------------------------------------------------------------------------
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c==============================================================================
c
cS
c----------------------------------------------------------------------
c
      subroutine par_setsrc(srctype, srcdepth, srcamp)
c
c set source depth, amplitude and type
c
      integer srctype
      real srcdepth, srcamp
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
c 
cE
      src_depth=srcdepth
      src_type=srctype
      src_amp=srcamp
      print *,'NOTICE (par_setsrc): depth, amp, type ',src_depth,
     &  src_amp,src_type
c
      return
      end
c
c ----- END OF par_setsrc.f -----
