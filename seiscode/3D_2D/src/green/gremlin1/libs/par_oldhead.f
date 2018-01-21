c this is <par_oldhead.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c control parameters
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
c    03/12/97   V1.0   Thomas Forbriger
c    10/12/97   V1.1   - par_sano looks now at section depth and
c                        excludes any sections below the top of
c                        bottom halfspace
c    22/12/97   V1.2   - par_defaults sets parameter defaults
c                      - some more setting routines (like par_parcor)
c    29/12/97   V1.3   - additional default setting routine
c                      - moved par_verbosity to this file
c    20/01/98   V1.4   - tell correct values of slowness and 1/Q
c                      - weights are now search ranges
c               V1.5   - return to velocity and Q
c    26/01/98   V1.6   - consider follow mode in par_parcor
c    27/01/98   V1.7   - allow travel time prefit
c    30/01/98   V1.8   - set pgplot parameter defaults
c  
c subroutines:
c   par_chop:          control model chopping parameters
c   par_setsrc:        set source parameters
c   par_sano:          define anonymous parameters
c   par_feas:          set feasible parameter range
c   par_defverb:       set parameter defaults for verbose (blockdata)
c   par_definv:        set parameter defaults for inversion (blockdata)
c   par_defpg:         set parameter defaults for pgplot (blockdata)
c   par_parcor:        display and set anonymous parameters
c   par_showsrc:       display source parameters
c   par_showmod:       display model parameters
c   par_showdat:       display data parameters
c   par_showinv:       display inversion parameters
c   par_showindex:     display index values
c   par_set:           display and set parameters
c   par_read:          read parameter setting from file
c   par_verbosity:     set verbosity levels
c   pars_verb:         print verbosity levels
cE
c ----- END OF par_oldhead.f -----
