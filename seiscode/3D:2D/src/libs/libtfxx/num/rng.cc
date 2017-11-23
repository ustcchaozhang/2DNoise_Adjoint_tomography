/*! \file rng.cc
 * \brief interface to GSL random number generators (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2007
 * 
 * interface to GSL random number generators (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 * REVISIONS and CHANGES 
 *  - 19/12/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_RNG_CC_VERSION \
  "TF_RNG_CC   V1.0   "

#include <tfxx/rng.h>
#include <time.h>

namespace tfxx {

  namespace numeric {

    // constructor
    RNGgaussian::RNGgaussian(const double& std,
                             const double& mean):
      Mstd(std), Mmean(mean)
    {
      gsl_rng_env_setup();
      const gsl_rng_type* T=gsl_rng_default;
      MR=gsl_rng_alloc(T);
      this->set();
    }

    // destructor
    RNGgaussian::~RNGgaussian()
    {
      gsl_rng_free(MR);
    }

    // return value
    double RNGgaussian::value() const
    { return(Mmean+Mstd*gsl_ran_ugaussian(MR)); }

    // return value
    double RNGgaussian::operator()() const
    { return(this->value()); }

    // set seed
    void RNGgaussian::set(const unsigned long int& seed) const
    { gsl_rng_set(MR, seed); }

    // set seed using time
    void RNGgaussian::set() const
    { this->set(time(0)); }

  } // namespace numeric

} // namespace tfxx

/* ----- END OF rng.cc ----- */
