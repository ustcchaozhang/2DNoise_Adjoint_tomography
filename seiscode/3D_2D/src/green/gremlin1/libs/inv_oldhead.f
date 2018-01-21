c this is <inv_oldhead.f>
c this is <glq_inv.f>
c------------------------------------------------------------------------------
cS
c $Id$
c 
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
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
c all to perform the least squares step
c
c REVISIONS and CHANGES
c    28/11/97   V1.0   Thomas Forbriger
c    18/12/97   V1.1   - return success of inv_part
c                      - introduced inv_mat
c                      - introduced inv_X2
c    22/12/97   V1.2   - include verbosity
c                      - inv_model did not return anything
c    29/12/97   V1.3   - had to correct inv_min and pgs_curve2
c    02/01/98   V1.4   - inv_min: start-value condition was too strong
c    05/01/98   V1.5   - removed inv_min start-value fetch-factor lq_x2release
c    16/01/98   V1.6   - code to check linearized forward modeling
c    19/01/98   V1.7   - introduced the term "search range" (see glq_mod.f)
c                        partial derivatives will be calculated over
c                        parameter changes relative to that search range 
c    20/01/98   V2.0   - weights become now real serach ranges
c                        changes in inv_model and inv_part
c    26/01/98   V2.1   - as we use "search ranges" now, it is appropriate
c                        to square mweights in inv_model
c    27/01/98   V2.2   - ugly bug: we have to use gweight instead of rgweight
c                        in inv_dss - the same for tweight and rtweight
c
c----------------------------------------------------------------------
c
c routines we need
c    inv_dss:        calculate dss matrix
c    inv_dssd:       caluclate dssd matrix
c    inv_dssdelta:   calculate dssdelta vector
c    inv_addtolist:  add an element to a history list
c    inv_min:        find minimum of X2(nu)
c    inv_linsynt:    calculate linearized forward modeling
c 
c functions:
c    inv_part:       calculate partial derivatives
c    inv_model:      estimate new model for given stabilizing factor
c    inv_mat:        combine inv_part to inv_dssdelta
c    inv_X2:         estimate new model and calculate X2
c    inv_linX2:      estimate new model and calculate X2 with inv_linsynt
c    inv_X2hist:     estimate new model and calculate X2 add values to history
cE
c ----- END OF inv_oldhead.f -----
