/*! \file valmap.h
 * \brief map element values (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/03/2005
 * 
 * map element values (prototypes)
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
#ifndef AFF_VALMAP_H_VERSION

#define AFF_VALMAP_H_VERSION \
  "AFF_VALMAP_H   V1.0   "

#include<aff/lib/mapper.h>
#include<map>

namespace aff {

  namespace func {

    namespace util {

      /*! Utility class to map values using a std::map
       *
       * This class should be used together with the
       * aff::func::util::mapvalues() function template.
       *
       * \param T1 source value type
       * \param T2 target value type
       */
      template<class T1, class T2>
        class Mapvalmap {
          public:
            typedef T1 Tinvalue;
            typedef T2 Toutvalue;
            typedef std::map<Tinvalue,Toutvalue> Tmap;
            //! initialize member data
            Mapvalmap(const Tmap& m): Mmap(m) { }
            //! map another value
            Toutvalue operator() (Tinvalue v) const { return(Mmap[v]); }
          private:
            mutable Tmap Mmap;
        }; // class Mapvalmap
      
    } // namespace util

/*----------------------------------------------------------------------*/

    /*! Function template to map container contents to values of a different
     *  type
     *
     * \param C any container class like aff::Array
     * \param M a std::map that defines source and target type
     *
     * \param c any container of values to be mapped
     * \return container of same shape as c but with mapped values
     *
     * \sa aff::func::util::mapvaues, aff::func::util::Mapvalmap
     */
    template<template <class> class C, class M>
      C<typename M::mapped_type> 
        valmap(const typename C<typename M::key_type>::Tcoc& c, 
               const M& m)
      {
        typedef typename M::key_type Tinvalue;
        typedef typename M::mapped_type Toutvalue;
        return(aff::func::util::mapvalues<C,
               util::Mapvalmap<Tinvalue,Toutvalue> >(c,m));
      } // valmap()

  } // namespace func

} // namespace aff

#endif // AFF_VALMAP_H_VERSION (includeguard)

/* ----- END OF valmap.h ----- */
