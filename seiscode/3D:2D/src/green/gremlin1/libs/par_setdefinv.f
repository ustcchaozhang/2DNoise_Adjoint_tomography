c this is <par_setdefinv.f>
c------------------------------------------------------------------------------
cS
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
c After splitting the source into different object files, the linker
c refuses to link blockdata subroutines.
c
c REVISIONS and CHANGES
c    06/04/98   V1.0   Thomas Forbriger
c    04/03/99   V1.1   changed minimum allowed nu
c    07/04/00   V1.2   set new data modification parameters too
c    09/04/00   V1.3   new use conord1 and conord2
c    10/04/00   V1.4   introduced model_interval
c    11/04/00   V1.5   introduced vptrackfactor
c
c==============================================================================
c
      subroutine par_setdefinv
c 
      include 'glq_dim.inc'
      include 'glq_invpara.inc'
      include 'glq_para.inc'
c 
c least squares parameters
      lq_nmean=5
      lq_msteps=20
      lq_mindown=5
      lq_relimp=1.e-5
      lq_x2lim=0.1
      lq_numax=1.e3
      lq_numin=1.e-3
      model_interval=3
c
c set default values for datamode parameters
      conord1=5.
      conord2=1.
      conthresh=0.3
      polyord=6
c
c set default fancy parameters
      vptrackfactor=-1.
      mweightcondition=1.d20
c 
      return
      end
cE
c
c ----- END OF par_setdefinv.f -----
