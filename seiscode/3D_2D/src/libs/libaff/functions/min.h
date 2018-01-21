/*! \file min.h
 * \brief return minimum element value (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/03/2005
 * 
 * return minimum element value (prototypes)
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
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 20/03/2005   V1.0   Thomas Forbriger
 *  - 28/04/2005   V1.1   provide return type
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_MIN_H_VERSION

#define AFF_MIN_H_VERSION \
  "AFF_MIN_H   V1.1   "


#include<aff/lib/collector.h>

namespace aff {

  namespace func {

    namespace util {

      /*! utility class to extract the smallest value from a container
       *
       * This class should be used together with the
       * aff::func::util::collect() function template.
       *
       * \param C any container class like aff::Array<double>
       */
      template<class C>
        class Extractmin {
          typedef typename C::Tcoc Tcont;
          typedef typename C::Tvalue Tvalue;
          public:
            typedef Tvalue Tretval;
            //! initialize member data
            Extractmin(const Tcont& c): Mval(c(c.first())) { }
            //! collect another value
            void operator() (const Tvalue& v) 
            { Mval = Mval < v ? Mval : v; }
            //! return result of operation
            Tretval result() const { return(Mval); }
          private:
            Tvalue Mval;
        }; // class Extractmin
      
    } // namespace util

/*----------------------------------------------------------------------*/

    /*! Function template to extract the smallest from the values
     *  stored in a container
     *
     * \param C any container class like aff::Array<double>
     *          (this value is deduced by the compiler)
     * \param c any container of numerical values
     * \return smallest value of container contents
     *
     * \sa aff::func::util::collect, aff::func::util::Extractmin
     */
    template<class C>
      typename C::Tvalue min(const C& c)
      {
        return(aff::func::util::collect<C, aff::func::util::Extractmin>(c));
      } // min()

  } // namespace func

} // namespace aff

#endif // AFF_MIN_H_VERSION (includeguard)

/* ----- END OF min.h ----- */
