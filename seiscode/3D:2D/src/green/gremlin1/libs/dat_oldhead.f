c this is <dat_oldhead.f>
c------------------------------------------------------------------------------
cS
c $Id$
c
c Copyright 1998, 2010 by Thomas Forbriger (IfG Stuttgart)
c
c Greens inversion subroutines
c calculating synthetic data, linear inversion for complex source
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
c amplitudes, evaluating the objective function, etc.
c
c REVISIONS and CHANGES
c    04/12/97   V1.0   Thomas Forbriger taken from gin_invsub.f
c    05/12/97   V1.1   - moved dat_setsrc to par_setsrc
c                      - new complete synthetics routine dat_synt
c    09/12/97   V1.2   several changes to get first somehow running version
c    10/12/97   V1.3   - corrected dat_fcamp
c                      - now uses different definition of datamode 2
c                      - no arguments for dmode
c    18/12/97   V1.4   - introduced dat_cref
c    22/12/97   V1.5   - use verbosity flags
c    30/12/97   V1.6   - as there are always arising problems with frequencies
c                        of about 0.Hz we just cancel them in dat_dmode
c                      - there was a major bug in dat_fcamp declaring
c                        refconjg to be real (not complex)
c                      - also changed safty valve behaviour in dat_fcamp
c                        and dat_famp
c                      - changed dat_famp calculation
c                      - changed dat_dcpc loop ranges
c                      - introduced dat_fmamp to prefit mean amplitude
c    27/01/98   V1.7   - introduced a travel time prefit by removing
c                        the average travel time offset
c    28/01/98   V1.8   - promoted travel time calculation to
c                        double precision
c    29/01/98   V1.9   - new travel time synthetics routine with
c                        its own discrete model: dat_dctt
c    30/01/98   V1.10  - ok - found the old bug again:
c                        look out for follow mode in dat_dctt
c
c----------------------------------------------------------------------
c 
c subroutines:
c
c dat_dmode:       tell us how to use the data values
c dat_rgreen:      read real green data
c                  switches sigma usage off
c dat_cgreen:      calculate pure reflectivity green
c dat_famp:        fit real amplitudes (ignore phase)
c dat_fmamp:       fit mean real amplitudes
c dat_fcamp:       fit complex amplitudes
c dat_ftt:         fit mean travel time error
c dat_rweight:     read the error estimates for greens values
c dat_dcpc:        copy data array to complex data mode
c dat_rtt:         read travel time data
c dat_ctt:         calculate travel times
c dat_dctt:        calculate travel times with may own discrete model
c dat_synt:        calculate new set of synthetic data
c 
c functions:
c
c dat_X2:          calculate value of error functional
c dat_cref:        calculate reference model synthetics
cE
c ----- END OF dat_oldhead.f -----
