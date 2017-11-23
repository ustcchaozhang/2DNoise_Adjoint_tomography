/*! \file collector.h
 * \brief collect values (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/03/2005
 * 
 * collect values (prototypes)
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
 *  - 28/04/2005   V1.1   use Tretval type
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_COLLECTOR_H_VERSION

#define AFF_COLLECTOR_H_VERSION \
  "AFF_COLLECTOR_H   V1.1"

#include<aff/iterator.h>

namespace aff {

  /*! Some functions to work on container data
   */
  namespace func {

    /*! Some utilities for container data functions
     */
    namespace util {

      /*! Function template to extract information from container data
       *
       * This is a framework to browse through a container and extract
       * information like the average or the minimum value. This function
       * template is used by container functions like aff::func::avg().
       *
       * \param C any container class like aff::Array<double>
       * \param F<T> any collector utility class like
       *             aff::func::util::Extractavg
       * \return result of operation that is coded in F<T>
       *
       * \sa aff::func::avg(),
       *     aff::func::min(),
       *     aff::func::max(),
       *     aff::func::rms()
       */
      template<class C, template<class> class F>
        typename F<C>::Tretval collect(const typename C::Tcoc& c)
        {
          F<C> exfun(c); 
          aff::Browser<C> browser(c);
          while(browser.valid())
          {
            exfun(*browser);
            ++browser;
          }
          return(exfun.result());
        }; // collect

    } // namespace util

  } // namespace func

} // namespace aff

#endif // AFF_COLLECTOR_H_VERSION (includeguard)

/* ----- END OF collector.h ----- */
