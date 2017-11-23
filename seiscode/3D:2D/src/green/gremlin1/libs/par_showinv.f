c this is <par_showinv.f>
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
c REVISIONS and CHANGES
c    24/03/98   V1.0   Thomas Forbriger
c    10/04/00   V1.1   introduced model_interval
c    02/06/00   V1.2   introduced mweightcondition
c
c==============================================================================
c 
      subroutine par_showinv
c 
c display inversion parameters
c
      include 'glq_dim.inc'
      include 'glq_para.inc'
      include 'glq_invpara.inc'
c 
cE
      print 50, model_interval,mweightcondition,pvar_pdev
      print 51, lq_x2lim, lq_relimp, lq_nmean, lq_msteps
      print 52, lq_numin, lq_numax, lq_mindown
c 
      return
   50 format(/'inversion parameters',/
     &       '  verbose model interval',t30,'[mint]: ',i10,/
     &       '  parameter weight condition',t30,'[pwco]: ',g10.2,/
     &       '  partial derivative step',t30,'[pder]: ',g10.2)
   51 format('  X2 optimization limit',t30,'[xlim]: ',f10.6,/
     &       '  min. relative improvement',t30,'[reli]: ',f10.6,/
     &       '  steps for mean improvement',t30,'[mean]: ',i10,/
     &       '  maximum iteration steps',t30,'[mste]: ',i10)
   52 format('  minimum nu allowed',t30,'[numi]: ',g10.2,/
     &       '  maximum nu allowed',t30,'[numa]: ',g10.2,/
     &       '  minimum number of downsteps',t30,'[mido]: ',i10)
      end
c
c ----- END OF par_showinv.f -----
