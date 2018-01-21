/*! \file random.cc
 * \brief create a random series (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/03/2016
 * 
 * create a random series (implementation)
 * 
 * Copyright (c) 2006, 2016 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 27/06/2006   V1.0   Thomas Forbriger
 *  - 19/03/2016   V1.1   seed sequence only once
 * 
 * ============================================================================
 */
#define TS_RANDOM_CC_VERSION \
  "TS_RANDOM_CC   V1.1"

#include <tsxx/random.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <time.h>

namespace ts {

  namespace rnd {

    namespace helper {

      //! internal class used for static variable in dugauss
      class GSLrng {
        public:
          explicit GSLrng(const gsl_rng_type* T)
          { 
            gsl_rng_env_setup();
            MR=gsl_rng_alloc(T); 
            this->seed(); 
          }
          ~GSLrng() { gsl_rng_free(MR); }
          double ugaussian() const { return gsl_ran_ugaussian(MR); }
          void seed(const unsigned long int& seedvalue) const
          { gsl_rng_set(MR, seedvalue); }
          void seed() const { this->seed(time(0)); }
        private:
          gsl_rng* MR;
      }; // class GSLrng

    } // namespace helper

/* ---------------------------------------------------------------------- */

    Tdseries dugauss(const int& n)
    {
      static helper::GSLrng rng(gsl_rng_default);
      Tdseries retval(n);
      for (int i=retval.f(); i<=retval.l(); ++i)
      { retval(i)=rng.ugaussian(); }
      return (retval);
    } // Tdseries dugauss

  } // namespace rnd

} // namespace ts

/* ----- END OF random.cc ----- */
