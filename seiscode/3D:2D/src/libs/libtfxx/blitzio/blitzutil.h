/*! \file blitzutil.h
 * \brief some helpers for Blitz++ (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/11/2002
 * 
 * some helpers for Blitz++ (prototypes)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 27/11/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_BLITZUTIL_H_VERSION

#define TF_BLITZUTIL_H_VERSION \
  "TF_BLITZUTIL_H   V1.0   "

#include<blitz/array.h>
#include<tfxx/error.h>

namespace tfxx {

/*! \defgroup group_blitzutil Blitz++ utilities
 */

/*! \brief Blitz++ utilities
 *
 * \ingroup group_blitzutil
 */
namespace blitzutil {

  /*! \brief test for conforming shapes
   *
   * \ingroup group_blitzutil
   * \param a1 any blitz array
   * \param a2 any other blitz array
   * \return true if both have same shape
   */
  template<class T1, class T2, int N1, int N2>
  bool sameShape(const blitz::Array<T1,N1>& a1,
                 const blitz::Array<T2,N2>& a2)
  { return(blitz::areShapesConformable(a1.shape(),a2.shape())); }

  /*! \brief test for conforming domains
   *
   * \ingroup group_blitzutil
   * \param a1 any blitz array
   * \param a2 any other blitz array
   * \return true if both have same domain
   */
  template<class T1, class T2, int N1, int N2>
  bool sameDomain(const blitz::Array<T1,N1>& a1,
                  const blitz::Array<T2,N2>& a2)
  { return(blitz::areShapesConformable(a1.lbound(),a2.lbound())&&
           blitz::areShapesConformable(a1.ubound(),a2.ubound())); }

  /*! \brief create a second array spanning the same domain
   *
   * \ingroup group_blitzutil
   * \param array A blitz array to be reshaped to \p domain.
   *              Upon return it references a new data area.
   * \param domain Domain to apply to \p array. May by return value of
   *               Array member function domain().
   * \param storage Blitz++ Array storage layout to be used for \p array.
   */
  template<class T, int N>
    void setToSameDomain(blitz::Array<T,N>& array,
                         const blitz::RectDomain<N>& domain,
                         const blitz::GeneralArrayStorage<N>& 
                           storage=blitz::fortranArray)
  { 
    blitz::TinyVector<int, N> extent;
    for (int i=0; i<N; i++)
    { extent[i]=domain.ubound()[i]-domain.lbound()[i]+1; }
    blitz::Array<T, N> result(domain.lbound(),extent,storage);
    array.reference(result);
  }

} // namespace blitzutil

} // namespace tfxx

#endif // TF_BLITZUTIL_H_VERSION (includeguard)

/* ----- END OF blitzutil.h ----- */
