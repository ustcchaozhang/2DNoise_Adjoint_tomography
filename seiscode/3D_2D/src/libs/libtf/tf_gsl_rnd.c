/*! \file tf_gsl_rnd.c
 * \brief Fortran interface to GSL random number generator (implementation)
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
 * \date 07/05/2007
 * 
 * Fortran interface to GSL functions (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 07/05/2007   V1.0   Thomas Forbriger
 *  - 13/11/2010   V1.1   changed name of file
 * 
 * ============================================================================
 */
#define TF_TF_GSL_RND_C_CC_VERSION \
  "TF_TF_GSL_RND_C_CC   V1.1"
#define TF_TF_GSL_RND_C_CC_CVSID \
  "$Id$"

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <time.h>
#include <f2c.h>

/* create a gaussian random vector of given size
 * the values have unit variance and zero mean
 *
 * a: random vector
 * n: size of random vector
 */
int tf_gsl_rng_ugaussian__(doublereal* a, integer* n)
{
    int i;
    /* set the default type of random number generator 
     * controlled by the environment variable GSL_RNG_TYPE
     * and allocate a random number generator
     */
    gsl_rng_env_setup();
    const gsl_rng_type* T=gsl_rng_default;
    gsl_rng* R=gsl_rng_alloc(T);
    /* seed the generator with the current time */
    gsl_rng_set(R, time(0));
    /* read values from the generator, using the desired distribution */
    for (i=0; i<=*n; ++i)
    { a[i]=(doublereal)gsl_ran_ugaussian(R); }
    /* discard the random number generator */
    gsl_rng_free(R);
    return 0;
} /* tf_gsl_rng_ugaussian__ */

/* create a uniform random vector of given size
 * the values have unit variance and zero mean
 *
 * a: random vector
 * n: size of random vector
 */
int tf_gsl_rng_uniform__(doublereal* a, integer* n)
{
    int i;
    /* set the default type of random number generator 
     * controlled by the environment variable GSL_RNG_TYPE
     * and allocate a random number generator
     */
    gsl_rng_env_setup();
    const gsl_rng_type* T=gsl_rng_default;
    gsl_rng* R=gsl_rng_alloc(T);
    /* seed the generator with the current time */
    gsl_rng_set(R, time(0));
    /* read values from the generator, using the desired distribution */
    for (i=0; i<=*n; ++i)
    { a[i]=(doublereal)gsl_rng_uniform(R); }
    /* discard the random number generator */
    gsl_rng_free(R);
    return 0;
} /* tf_gsl_rng_uniform__ */

/* ----- END OF tf_gsl_rnd.c ----- */
