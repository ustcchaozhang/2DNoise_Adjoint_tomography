/*! \file cseife_rfk.c
 * \brief calculate IIR filter coefficients (implementation)
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
 * calculate IIR filter coefficients (implementation)
 * 
 * REVISIONS and CHANGES 
 *  - 28/06/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_CSEIFE_RFK_C_VERSION \
  "TF_CSEIFE_RFK_C   V1.0   "
#define TF_CSEIFE_RFK_C_CVSID \
  "$Id$"

#include <cseife.h>
#include <math.h>

/* subs/seife_rfk.f -- translated by f2c (version 20000121).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)

   the code was derived through f2c, but modified thereafter
*/

struct seife_IIRcoef seife_rfk(enum seife_filter_type IIRtype, 
                               double t0, double h, double t0s, double hs)
{
    /* Initialized data */

    static double zero = 0.;
    static double one = 1.;
    static double two = 2.;
    static double four = 4.;
    static double eight = 8.;

    /* Local variables */
    double epsq, epss, a, b, c, epssq, as, bs, cs, eps, zpi;

    struct seife_IIRcoef coef;

/*  determine coefficients for recursive filter */

    if (IIRtype == IIRint) {
/* calculate integrator coefficients 
 * seperate from the others
 */
      coef.f0 = one / two / t0;
      coef.f1 = coef.f0;
      coef.f2 = zero;
      coef.g1 = one;
      coef.g2 = zero;
    }
    else
    {
/* values needed for all other filters
 */
      zpi = eight * atan(one);
      eps = zpi / t0;
      coef.f2 = zero;
      coef.g2 = zero;
      if ( (IIRtype == IIRhp1) ||
           (IIRtype == IIRlp1) ||
           (IIRtype == IIRhe1) ||
           (IIRtype == IIRle1) )
      {
/* go for 1st order filters
 * ------------------------
 */
        coef.g1 = (two - eps) / (two + eps);
        if (IIRtype == IIRlp1)
        {
/*   first order low pass */
          coef.f0 = eps / (two + eps);
          coef.f1 = coef.f0;
        }
        else if (IIRtype == IIRhp1)
        {
/*   first order high pass */
          coef.f0 = two / (two + eps);
          coef.f1 = -(coef.f0);
        }
        else if (( IIRtype == IIRhe1) || (IIRtype == IIRle1))
        {
/*   first order equalizers */
          epss = zpi / t0s;
          coef.f0 = (epss + two) / (eps + two);
          coef.f1 = (epss - two) / (eps + two);
        }
      }
      else
      {
/* go for 2nd order filters
 * ------------------------
 */
        epsq = eps * eps;
        a = one - eps * h + epsq / four;
        b = -two + epsq / two;
        c = one + eps * h + epsq / four;
        coef.g1 = -b / c;
        coef.g2 = -a / c;
        if (IIRtype == IIRlp2)
        {
/*   second order low pass */
          coef.f0 = epsq / four / c;
          coef.f1 = coef.f0 + coef.f0;
          coef.f2 = coef.f0;
        }
        else if (IIRtype == IIRhp2)
        {
/*   second order high pass */
          coef.f0 = one / c;
          coef.f1 = -(coef.f0) - coef.f0;
          coef.f2 = coef.f0;
        }
        else if (( IIRtype == IIRhe2) || (IIRtype == IIRle2))
        {
/*   second order equalizers */
          epss = zpi / t0s;
          epssq = epss * epss;
          as = one - epss * hs + epssq / four;
          bs = -two + epssq / two;
          cs = one + epss * hs + epssq / four;
          coef.f0 = cs / c;
          coef.f1 = bs / c;
          coef.f2 = as / c;
        }
        else if ( IIRtype == IIRbp2 )
        {
/*   second order band pass */
          coef.f0 = eps / two / c;
          coef.f1 = zero;
          coef.f2 = -(coef.f0);
        }
      }
    }
    return coef;
} /* seife_rfk */


/* ----- END OF cseife_rfk.c ----- */
