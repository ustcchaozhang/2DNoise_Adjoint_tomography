c this is <mod_panic.f>
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
      subroutine mod_panic(message)
c
c emergency exit
c
      character*(*) message
c 
      include 'glq_dim.inc'
c
cE
      character filename*20
      parameter(filename='glq_panic.mod')
c 
      print *,'ATTENTION (mod_panic): Your program took the emergancy exit'
      print *,'                       leaving the following note:'
      print *,message
      print *,'NOTICE (mod_panic): I will try to save your current reference ',
     &  'model to ', filename
      call mod_save(filename, mb_ref, .true., message)
      stop 'EMERGENCY EXIT (mod_panic)'
      end
c
c ----- END OF mod_panic.f -----
