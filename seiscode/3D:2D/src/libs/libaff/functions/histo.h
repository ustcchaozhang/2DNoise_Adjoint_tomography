/*! \file histo.h
 * \brief count elements of same value (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/04/2005
 * 
 * count elements of same value (prototypes)
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
 *  - 28/04/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_HISTO_H_VERSION

#define AFF_HISTO_H_VERSION \
  "AFF_HISTO_H   V1.0   "


#include<aff/lib/collector.h>
#include<map>

namespace aff {

  namespace func {

    namespace util {

      /*! utility class to count elements of the same value
       *
       * This class should be used together with the
       * aff::func::util::collect() function template.
       *
       * \param C any container class like aff::Array<double>
       */
      template<class C>
        class Extracthisto {
          typedef typename C::Tcoc Tcont;
          typedef typename C::Tvalue Tvalue;
          public:
            typedef typename std::map<Tvalue,int> Tmap;
            typedef Tmap Tretval;
            //! initialize member data
            Extracthisto(const Tcont& c) { }
            //! collect another value
            void operator() (const Tvalue& v)
            { ++Mmap[v]; }
            //! return result of operation
            Tretval result() const { return(Mmap); }
          private:
            Tmap Mmap;
        }; // class Extracthisto
      
    } // namespace util

/*----------------------------------------------------------------------*/

    /*! Function template to count elements of the same value
     *
     * \param C any container class like aff::Array<double>
     *          (this value is deduced by the compiler)
     * \param c any container of numerical values
     * \return a map the associates every value with the number of elements
     *         that have this value
     *
     * \sa aff::func::util::collect, aff::func::util::Extracthisto
     */
    template<class C>
      typename aff::func::util::Extracthisto<C>::Tmap histo(const C& c)
      {
        return(aff::func::util::collect<C, aff::func::util::Extracthisto>(c));
      } // histo()

  } // namespace func

} // namespace aff

#endif // AFF_HISTO_H_VERSION (includeguard)

/* ----- END OF histo.h ----- */
