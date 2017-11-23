/*! \file correlate.h
 * \brief calculate correlation of two series (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/02/2004
 * 
 * calculate correlation of two series (prototypes)
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
#ifndef TF_CORRELATE_H_VERSION

#define TF_CORRELATE_H_VERSION \
  "TF_CORRELATE_H   V1.0   "

#include<aff/series.h>

namespace ts {

  /*! \brief Calculate crosscorrelation of two series.
   *
   * The function evaluates
   * \f[
   *   c_k = \sum\limits_l a_l b_{k+l},
   * \f]
   * where the index ranges of the input series are
   * \f$ a_l: l \in [F_a,L_a] \f$ and
   * \f$ b_l: l \in [F_b,L_b]. \f$
   * From the second factor we conclude
   * \f$ F_b -l \leq k \leq L_b - l \f$ and thus the index range of 
   * \f$ c_k \f$ is
   * \f[
   *    F_b - L_a \leq k \leq L_b - F_a.
   * \f]
   * For a given \f$ k \f$ the range of summation index \f$ l \f$ is
   * defined by the two conditions
   * \f[
   *    F_b - k \leq l \leq L_b - k
   * \f]
   * and
   * \f[
   *    F_a \leq k \leq L_a.
   * \f]
   * 
   * \param a \f$ a_l \f$
   * \param b \f$ b_l \f$
   * \return c \f$ c_l \f$
   */
  template<class T>
    aff::Series<T> correlate(const aff::ConstSeries<T>& a, 
                             const aff::ConstSeries<T>& b)
    {
      aff::Series<T> retval(b.first()-a.last(),b.last()-a.first());
      for (long int k=retval.first(); k<=retval.last(); ++k)
      {
        retval(k)=T(0);
        long int lmin=a.first() > (b.first()-k) ? a.first() : (b.first()-k);
        long int lmax=a.last() < (b.last()-k) ? a.last() : (b.last()-k);
        for(long int l=lmin; l<=lmax; ++l)
        {
          retval(k)+=a(l)*b(l+k);
        }
      }
      return(retval);
    } // correlate

} // namespace ts

#endif // TF_CORRELATE_H_VERSION (includeguard)

/* ----- END OF correlate.h ----- */
