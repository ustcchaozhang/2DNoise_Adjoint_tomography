c this is <mod_oldhead.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c all model manipulation routines for least squares
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
c    28/11/97   V1.0   Thomas Forbriger
c    03/12/97   V1.1   - mod_clear
c    05/12/97   V1.2   - moved mod_sano to par_sano
c                      - depth check
c                      - mod_aclear, mod_prep
c    10/12/97   V1.3   - changed parcor to use different definition
c                        and to ignore sections within halfspace
c                      - changed mod_prep (no arguments anymore)
c    15/12/97   V1.4   - changed mod_parcor to apply the polynomial
c                        orders a2 and a3 in the correct way
c                      - changed mod_chop to build a minimum of
c                        discrete layers even in case the desired
c                        stepsize is not reached. That's necessary
c                        to ensure constraints on the higher
c                        polynomial orders.
c    22/12/97   V1.5   - use verbosity flags
c    29/12/97   V1.6   - added emergency exit mod_panic
c    30/12/97   V1.7   - bug in mod_parcor: the coefficient a1 is needed
c                        to apply changes to a2 and a3! but it is changed
c                        first, which might lead to errors when using
c                        the same model as input and output
c                      - there where cases of extreme values on search
c                        boundaries in mod_chop - we will catch and ignore
c                        them
c    02/01/98   V1.8   - mod_save did not open a file... 
c                      - the nested loops in mod_parcor make it impossible to
c                        change a reference model - we have to apply
c                        changes and save the result to md_work and then
c                        copy to mb_ref by using mod_copy
c    19/01/98   V1.9   - introduced the term "search range": all parcor
c                        parameter changes will be in absolute parameter
c                        values (which have physical units).
c                        the so called "search range" is the reciprocal
c                        parameter weight, where "parameter weight" is
c                        the former term for the reciprocal "search range"
c                        (that sounds redundant)...
c                        the partial derivatives are calculated over
c                        parameter changes relative to the "serach range"
c                        changed subroutines: mod_parcor, inv_part
c    20/01/98   V2.0   - from now on we will use polynomial expansions
c                        for the slowness values 1/Vs and 1/Vp and for
c                        1/Qp and 1/Qs
c                        routines to be changed: mod_chop, mod_check
c                      - weights become now real search ranges
c                        routines to be changed: mod_showparcor
c               V2.1   - return to velocity and Q
c                      - model print out function
c    26/01/98   V2.2   - introduced a follow mode to make a section
c                        parameter follow its lower neighbour
c                        new: mod_follow
c                        changed: mod_read, mod_write, mod_parcor,
c                          mod_clear, mod_chop, mod_showparcor,
c                          mod_copy
c    27/01/98   V2.3   - remember parent of discrete model to support
c                        dat_dctt - changed mod_chop
c
c----------------------------------------------------------------------
c 
c routines we need:
c    mod_clear:       clear model space and set number of sections
c    mod_read:        read model from file
c    mod_save:        write model to file
c    mod_write:       write a model to any lu
c    mod_copy:        copy one model to another array
c    mod_parcor:      create new model from reference and changes
c    mod_chop:        chop model
c    mod_aclear:      clear anonymous parameter space
c    mod_weight:      set weight matrix (different modes)
c    mod_showparcor:  information about anonymous parameter setting
c    mod_panic:       exit in emergency cases
c    mod_follow:      make section parameters follow
c 
c functions:
c    mod_isec:     return section index containing depth
c    mod_check:    check feasibility of model
c    mod_pcheck:   check feasibility of polynomial model depth
c    mod_prep:     prepare and check model for calculations
c 
c Some comments on model design as used in this version:
c The only parameters that use an expansion of polynomial coefficients
c is the s and p-velocities. The density will be given as a
c fraction of Vp. The Q-values are constant within each section.
c All polynomials are relative to the top of each section.
c 
c Dimensions for named models are m, km/s, g/ccm
c Dimensions for anonymous models are km, km/s, g/ccm
cE
c ----- END OF mod_oldhead.f -----
