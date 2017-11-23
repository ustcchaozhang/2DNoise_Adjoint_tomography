/*! \file sum.h
 * \brief evaluate sum of all samples (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/06/2007
 * 
 * evaluate sum of all samples (prototypes)
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
 *  - 13/06/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SUM_H_VERSION

#define AFF_SUM_H_VERSION \
  "AFF_SUM_H   V1.0   "


#include<aff/lib/collector.h>

namespace aff {

  namespace func {

    namespace util {

      /*! utility class to extract the sum of all samples from the container
       *
       * This class should be used together with the
       * aff::func::util::collect() function template.
       *
       * \param C any container class like aff::Array<double>
       */
      template<class C>
        class Extractsum {
          typedef typename C::Tcoc Tcont;
          typedef typename C::Tvalue Tvalue;
          public:
            typedef Tvalue Tretval;
            //! initialize member data
            Extractsum(const Tcont& c): Msum(0), Mn(0) { }
            //! collect another value
            void operator() (const Tvalue& v) { Msum+=v; ++Mn; }
            //! return result of operation
            Tretval result() const { return(Msum); }
          private:
            Tvalue Msum;
            int Mn;
        }; // class Extractsum
      
    } // namespace util

/*----------------------------------------------------------------------*/

    /*! Function template to extract the sum of all samples
     *  stored in a container
     *
     * \param C any container class like aff::Array<double>
     *          (this value is deduced by the compiler)
     * \param c any container of numerical values
     * \return sum of all samples in the container
     *
     * \sa aff::func::util::collect, aff::func::util::Extractsum
     */
    template<class C>
      typename C::Tvalue sum(const C& c)
      {
        return(aff::func::util::collect<C, aff::func::util::Extractsum>(c));
      } // sum()

  } // namespace func

} // namespace aff

#endif // AFF_SUM_H_VERSION (includeguard)

/* ----- END OF sum.h ----- */
