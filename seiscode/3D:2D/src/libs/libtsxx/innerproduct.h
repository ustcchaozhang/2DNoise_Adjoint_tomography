/*! \file innerproduct.h
 * \brief calculate an inner product (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/02/2004
 * 
 * calculate an inner product (prototypes)
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 07/02/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_INNERPRODUCT_H_VERSION

#define TF_INNERPRODUCT_H_VERSION \
  "TF_INNERPRODUCT_H   V1.0   "

#include<aff/series.h>

namespace ts {

  /*! \brief Calculate inner product of two series.
   *
   * If both series are of unequal index range, they are padded with zeros.
   * 
   * The function evaluates
   * \f[
   *   c_k = \sum\limits_{l=F}^{L} a_l b_l,
   * \f]
   * where the index ranges of the input series are
   * \f$ a_l: l \in [F_a,L_a] \f$ and
   * \f$ b_l: l \in [F_b,L_b]. \f$
   * and
   * \f$F=\textrm{max}(F_a,F_b)\f$
   * as well as
   * \f$L=\textrm{min}(L_a,L_b).\f$
   * 
   * \param a \f$ a_l \f$
   * \param b \f$ b_l \f$
   * \return c \f$ c_l \f$
   */
  template<class T>
    T innerproduct(const aff::ConstSeries<T>& a, 
                   const aff::ConstSeries<T>& b)
    {
      T retval=0;
      long int first=(a.first() > b.first() ? a.first() : b.first());
      long int last=(a.last() < b.last() ? a.last() : b.last());
      if (last >= first) 
      {
        for (int i=first; i<=last; i++)
        {
          retval += a(i)*b(i);
        }
      }
      return(retval);
    } // innerproduct

  /*! \brief Calculate rms value.
   *
   * The function evaluates the root mean square
   * \f[
   *   c = \sqrt{\frac{1}{N} \sum\limits_l a_l^2}
   * \f]
   * 
   * \param a \f$ a_l \f$
   * \return c 
   */
  template<class T>
    T rms(const aff::ConstSeries<T>& a) 
    { return(sqrt(innerproduct(a,a)/a.size())); }

} // namespace ts

#endif // TF_INNERPRODUCT_H_VERSION (includeguard)

/* ----- END OF innerproduct.h ----- */
