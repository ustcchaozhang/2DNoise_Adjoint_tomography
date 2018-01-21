/*! \file cseife_gauss.c
 * \brief solve system of linear equations (implementation)
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
 * solve system of linear equations (implementation)
 * 
 * REVISIONS and CHANGES 
 *  - 28/06/2005   V1.0   Thomas Forbriger
 *  - 15/11/2010   V1.1   do not use tfmacros.h
 * 
 * ============================================================================
 */
#define TF_CSEIFE_GAUSS_C_VERSION \
  "TF_CSEIFE_GAUSS_C   V1.1"
#define TF_CSEIFE_GAUSS_C_CVSID \
  "$Id$"

#include <cseife.h>
#include <stdio.h>
#include <stdlib.h>

/* subs/seife_gauss.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)

   the code was derived through f2c, but modified thereafter
*/

void seife_gauss(double *aik, int m, int n, double* rs, double* f)
{
    /* System generated locals */
    int aik_dim1, aik_offset, i__1, i__2, i__3;
    double d__1;

    /* Local variables */
    int imax[14];
    double h__[15];
    int j, k, l;
    double q;
    int index;
    double aikmax;

    SEIFE_CHECKERROR( m>13 , "seife_gauss", "matrix is too large" )
    SEIFE_CHECKERROR( n>13 , "seife_gauss", "matrix is too large" )
/*  solve linear equations */
    /* Parameter adjustments */
    --f;
    --rs;
    aik_dim1 = n;
    aik_offset = 1 + aik_dim1 * 1;
    aik -= aik_offset;

    /* Function Body */
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
	aikmax = 0.;
	i__2 = m;
	for (k = 1; k <= i__2; ++k) {
	    h__[k - 1] = aik[j + k * aik_dim1];
	    if ((d__1 = h__[k - 1], abs(d__1)) <= aikmax) {
		goto L1402;
	    }
	    aikmax = (d__1 = h__[k - 1], abs(d__1));
	    index = k;
L1402:
	    ;
	}
	h__[m] = rs[j];
	i__2 = m;
	for (k = 1; k <= i__2; ++k) {
	    q = aik[k + index * aik_dim1] / h__[index - 1];
	    i__3 = m;
	    for (l = 1; l <= i__3; ++l) {
/* L1404: */
		aik[k + l * aik_dim1] -= q * h__[l - 1];
	    }
/* L1403: */
	    rs[k] -= q * h__[m];
	}
	i__2 = m;
	for (k = 1; k <= i__2; ++k) {
/* L1405: */
	    aik[j + k * aik_dim1] = h__[k - 1];
	}
	rs[j] = h__[m];
/* L1401: */
	imax[j - 1] = index;
    }
    i__1 = m;
    for (j = 1; j <= i__1; ++j) {
	index = imax[j - 1];
/* L1406: */
	f[index] = rs[j] / aik[j + index * aik_dim1];
    }
} /* seife_gauss */


/* ----- END OF cseife_gauss.c ----- */
