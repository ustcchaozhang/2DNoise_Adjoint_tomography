/*! \file tf_gsl_bess.c
 * \brief Fortran interface to GSL bessel functions (implementation)
 * 
 * ----------------------------------------------------------------------------
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
 * \author Thomas Forbriger
 * \date 13/11/2010
 * 
 * Fortran interface to GSL bessel functions (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 13/11/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_TF_GSL_BESS_C_VERSION \
  "TF_TF_GSL_BESS_C   V1.0   "
#define TF_TF_GSL_BESS_C_CVSID \
  "$Id$"

#include <gsl/gsl_sf_bessel.h>
#include <f2c.h>

doublereal tf_gsl_sf_bessel_k0__(doublereal* x)
{
  doublereal retval;
  retval=(doublereal)gsl_sf_bessel_K0((double)*x);
  return retval;
}

doublereal tf_gsl_sf_bessel_k1__(doublereal* x)
{
  doublereal retval;
  retval=(doublereal)gsl_sf_bessel_K1((double)*x);
  return retval;
}

doublereal tf_gsl_sf_bessel_i0__(doublereal* x)
{
  doublereal retval;
  retval=(doublereal)gsl_sf_bessel_I0((double)*x);
  return retval;
}

doublereal tf_gsl_sf_bessel_i1__(doublereal* x)
{
  doublereal retval;
  retval=(doublereal)gsl_sf_bessel_I1((double)*x);
  return retval;
}

/* ----- END OF tf_gsl_bess.c ----- */
