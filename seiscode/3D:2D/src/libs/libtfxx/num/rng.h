/*! \file rng.h
 * \brief interface to GSL random number generators (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2007
 * 
 * interface to GSL random number generators (prototypes)
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

// include guard
#ifndef TF_RNG_H_VERSION

#define TF_RNG_H_VERSION \
  "TF_RNG_H   V1.0   "

#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>

/*! \defgroup group_numeric Numerical functions.
 * \brief Provide numerical functions, random numbers, etc.
 *
 * The module is presented in namespace tfxx::numeric.
 */

namespace tfxx {

  /*! \namespace tfxx::numeric
   * \brief Namespace containing all components of module numeric.
   * \ingroup group_numeric
   * \sa group_numeric
   */
  namespace numeric {

    /*! \brief Provide random numbers with Gaussian distribution.
     * \ingroup group_numeric
     *
     * This class is an interface to the GSL random number generator.
     */
    class RNGgaussian {
      public:
        //! initialize random number generator
        explicit RNGgaussian(const double& std=1.,
                             const double& mean=0.);
        ~RNGgaussian();
        //! returns a random number
        double operator()() const;
        //! returns a random number
        double value() const;
        //! feed with seed value
        void set(const unsigned long int& seed) const;
        //! use time as seed value
        void set() const;
      private:
        double Mstd, Mmean;
        gsl_rng* MR;
    }; // class RNGgaussian
  } // namespace numeric

} // namespace tfxx

#endif // TF_RNG_H_VERSION (includeguard)

/* ----- END OF rng.h ----- */
