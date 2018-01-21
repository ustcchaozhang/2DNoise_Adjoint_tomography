/*! \file mapper.h
 * \brief map values (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/03/2005
 * 
 * map values (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_MAPPER_H_VERSION

#define AFF_MAPPER_H_VERSION \
  "AFF_MAPPER_H   V1.0   "

#include<aff/iterator.h>

namespace aff {

  namespace func {

    namespace util {

      /*! Function template to map values from one container to another
       *
       * This is a framework to browse through a container and map
       * values to values of a different type. This function template is used
       * by container functions like aff::func::valmap().
       *
       * \param C any container class like aff::Array
       * \param F any mapper utility class like
       *          aff::func::util::Mapvalmap
       *
       * \param c container to read values to be mapped
       * \param exfun fully initialized mapper utility
       * \return result container of same shape as c but with mapped values
       *
       * \sa aff::func::valmap()
       */
      template<template<class> class C, class F>
        C<typename F::Toutvalue> 
          mapvalues(const typename C<typename F::Tinvalue>::Tcoc& c,
                    const F& exfun)
        {
          typedef typename C<typename F::Tinvalue>::Tcoc Tin;
          typedef C<typename F::Toutvalue> Tout;
          Tout retval(c.shape());
          aff::Browser<Tin> browser(c);
          aff::Iterator<Tout> iterator(retval);
          while(browser.valid() && iterator.valid())
          {
            *iterator = exfun(*browser);
            ++browser;
            ++iterator;
          }
          return(retval);
        }; // mapvalues

    } // namespace util

  } // namespace func

} // namespace aff

#endif // AFF_MAPPER_H_VERSION (includeguard)

/* ----- END OF mapper.h ----- */
