/*! \file cseife_deriv.c
 * \brief calculate time derivative (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * Copyright 1984 by Erhard Wielandt
 * This algorithm was part of seife.f. A current version of seife.f can be
 * obtained from http://www.software-for-seismometry.de/
 * 
 * $Id$
 *
 * ----
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * ----
 *
 * \date 28/06/2005
 * 
 * calculate time derivative (implementation)
 * 
 * REVISIONS and CHANGES 
 *  - 28/06/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_CSEIFE_DERIV_C_VERSION \
  "TF_CSEIFE_DERIV_C   V1.0   "
#define TF_CSEIFE_DERIV_C_CVSID \
  "$Id$"

#include <cseife.h>

/* subs/seife_deriv.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)

   the code was derived through f2c, but modified thereafter
*/

/* derivative (time constant t0) */
void seife_dif(double* x, int n, double dt, double tau)
{
    /* System generated locals */
    int i__1;

    /* Local variables */
    static int j;
    static double twodt;

/*  derivative (non-recursive, non-causal, symmetric-difference) */
    /* Parameter adjustments */
    --x;

    if (tau < 1.e-23) {
	tau = 1.;
    }
    twodt = dt * 2. / tau;
    i__1 = n - 2;
    for (j = 1; j <= i__1; ++j) {
/* L1: */
	x[j] = (x[j + 2] - x[j]) / twodt;
    }
    for (j = n - 1; j >= 2; --j) {
/* L2: */
	x[j] = x[j - 1];
    }
    x[n] = x[n - 1];
} /* seife_dif */


/* ----- END OF cseife_deriv.c ----- */
