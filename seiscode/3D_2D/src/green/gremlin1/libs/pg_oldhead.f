c this is <pg_oldhead.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1997, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c some routines to create control plots
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
c    04/12/97   V1.1   - polynomial model plotting
c    05/12/97   V1.2   - plot travel times
c    09/12/97   V1.3   several changes (mainly to pg_green) to get a
c                      somehow running version
c    10/12/97   V1.4   - pg_green uses only one simple mode (no lev anymore)
c    12/12/97   V1.5   - pg_inv invented
c    15/12/97   V1.6   - nu-steps in pg_inv are now equalsized in 1./nu
c    17/12/97   V1.7   - setting of nst was done too late in pg_inv
c    18/12/97   V1.8   - added new viewports 6 & 7
c                      - and pg_curve
c                      - and pg_min
c    22/12/97   V1.9   - use verbosity flags
c                      - use greek chi
c    23/12/97   V1.10  - some new routines like pg_gdata, pg_tdata
c                      - add new optimization routine pg_opt
c    30/12/97   V1.11  - avoid plotting of frequency 0.Hz in pg_gdata
c    05/01/98   V1.12  - some cometics
c                      - new pg_x2
c    15/01/98   V1.13  - set view port 6 and 12 with correct right border
c                      - curve plotting with y1=y2
c    16/01/98   V1.14  - plot also linearized X2(nu)
c    20/01/98   V1.15  - as polynomial model values now mean slowness
c                        and reciprocal Q we have to change one plot
c                        routine: pgs_par
c               V1.16  - return to velocity and Q
c                      - introduced pg_invfix
c    26/01/98   V1.17  - check follow parameter in pgs_par 
c    28/01/98   V1.18  - had to change travel time plotting routines,
c                        because data now is double precision
c                        changed: pg_tt and pg_tdata
c
c----------------------------------------------------------------------
c
c subroutines:
c   pg_selvp:     select a predefined viewport
c   pg_mod:       plot one model
c   pg_tt:        plot travel time curve
c   pg_green:     plot greens matrix
c   pg_inv:       plot proceeding inversion with decreasing nu
c   pg_gdata:     plot green real data
c   pg_tdata:     plot travel time real data
c   pg_opt:       optimize model
c   pg_x2:        plot X2(nu)
c   pg_linx2:     plot linearized X2(nu)
c   pg_invfix:    interation using fixed stabilization (Wielandt method)
c 
c functions:
c   pg_min:       minimizing strategy
c   pg_iter:      iteration strategy
c 
c some usefull paraphernalia
c   pgs_par:      plot one polynomial model parameter 
c   pgs_curve:    plot a simple curve
c   pgs_addval:   add a new value to a curve array
c   pgs_curve:    plot a curve
c   pgs_curve2:   plot a two color curve
cE
c ----- END OF pg_oldhead.f -----
