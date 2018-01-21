/*! \file absmax.h
 * \brief extract absolute maximum (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 09/03/2010
 * 
 * extract absolute maximum (prototypes)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 09/03/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_ABSMAX_H_VERSION

#define AFF_ABSMAX_H_VERSION \
  "AFF_ABSMAX_H   V1.0   "

#include<aff/lib/collector.h>

namespace aff {

  namespace func {

    namespace util {

      /*! utility class to extract the largest absolute
       * value from a container
       *
       * This class should be used together with the
       * aff::func::util::collect() function template.
       *
       * \param C any container class like aff::Array<double>
       */
      template<class C>
        class Extractabsmax {
          typedef typename C::Tcoc Tcont;
          typedef typename C::Tvalue Tvalue;
          public:
            typedef Tvalue Tretval;
            //! initialize member data
            Extractabsmax(const Tcont& c): Mval(c(c.first())) 
            { 
              if (Mval<0) { Mval = -Mval; }
            }
            //! collect another value
            void operator() (const Tvalue& v) 
            { 
              Tvalue av = v < 0 ? -v : v;
              Mval = Mval > av ? Mval : av; 
            }
            //! return result of operation
            Tretval result() const { return(Mval); }
          private:
            Tvalue Mval;
        }; // class Extractabsmax
      
    } // namespace util

/*----------------------------------------------------------------------*/

    /*! Function template to extract the largest absolute from the values
     *  stored in a container
     *
     *  This is usefull to normalize timeseries data to their maximum.
     *
     * \param C any container class like aff::Array<double>
     *          (this value is deduced by the compiler)
     * \param c any container of numerical values
     * \return largest absolute value of container contents
     *
     * \sa aff::func::util::collect, aff::func::util::Extractabsmax
     */
    template<class C>
      typename C::Tvalue absmax(const C& c)
      {
        return(aff::func::util::collect<C, aff::func::util::Extractabsmax>(c));
      } // absmax()

  } // namespace func

} // namespace aff

#endif // AFF_ABSMAX_H_VERSION (includeguard)

/* ----- END OF absmax.h ----- */
