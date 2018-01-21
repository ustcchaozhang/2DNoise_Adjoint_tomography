/*! \file convolve.h
 * \brief convolve to series (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/02/2004
 * 
 * convolve to series (prototypes)
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
#ifndef TF_CONVOLVE_H_VERSION

#define TF_CONVOLVE_H_VERSION \
  "TF_CONVOLVE_H   V1.0"

#include<aff/series.h>

namespace ts {

  /*! \brief Calculate convolution of two series.
   *
   * The function evaluates
   * \f[
   *   c_k = \sum\limits_l a_l b_{k-l},
   * \f]
   * where the index ranges of the input series are
   * \f$ a_l: l \in [F_a,L_a] \f$ and
   * \f$ b_l: l \in [F_b,L_b]. \f$
   * From the second factor we conclude
   * \f$ F_b +l \leq k \leq L_b + l \f$ and thus the index range of 
   * \f$ c_l \f$ is
   * \f[
   *    F_b + F_a \leq k \leq L_b + L_a.
   * \f]
   * For a given \f$ k \f$ the range of summation index \f$ l \f$ is
   * defined by the two conditions
   * \f[
   *    k- L_b \leq l \leq k- F_b 
   * \f]
   * and
   * \f[
   *    F_a \leq k \leq L_a.
   * \f]
   *
   * \note
   * This function is designed to be used as a discrete approximation to the
   * convolution integral, if convolved series are sampled versions of
   * functions.
   * This has two consequences:
   *  -# The result should be appropriately scaled with the sampling interval.
   *     The sampling interval is not available to this function.
   *     The scaling therefore has to take place in the calling program.
   *  -# The time series are understood to have infinite length, while all
   *     samples outside the provided index range implicitely equal zero.
   *     The convolution result then necessarily equals zero for index values
   *     \f$k < F_b + F_a\f$ and \f$k >  L_b + L_a\f$.
   *     The result \f$c_k\f$ will be output for
   *     \f$ k \in [F_a+F_b,L_a+L_b] \f$
   *     with \f$(L_a-F_a)+(L_b-F_b)+1\f$ samples, thus being longer than
   *     either one of the input series.
   *     If the convolution filter response is not given completely, but just
   *     as a cut-out of the filter response function, the convolution
   *     potentially produces a \b step-response \b at \b the \b end \b of \b
   *     the \b filter \b sequence.
   *     This exactly is the behaviour of the convolution integral, if only
   *     a cut-out of the originally infinite filter impulse response
   *     function is used.
   *     If this is missed, the user might be surprised by an unexpected coda
   *     in the result of this function.
   *     Consider to trim the output sequence to a reasonable sample window to
   *     represent a proper cut-out of the expected result.
   * 
   * \param a \f$ a_l \f$
   * \param b \f$ b_l \f$
   * \return c \f$ c_l \f$
   */
  template<class T>
    aff::Series<T> convolve(const aff::ConstSeries<T>& a, 
                            const aff::ConstSeries<T>& b)
    {
      aff::Series<T> retval(a.first()+b.first(),b.last()+a.last());
      for (long int k=retval.first(); k<=retval.last(); ++k)
      {
        retval(k)=T(0);
        long int lmin=a.first() > (k-b.last()) ? a.first() : (k-b.last());
        long int lmax=a.last() < (k-b.first()) ? a.last() : (k-b.first());
        for(long int l=lmin; l<=lmax; ++l)
        {
          retval(k)+=a(l)*b(k-l);
        }
      }
      return(retval);
    } // convolve

} // namespace ts

#endif // TF_CONVOLVE_H_VERSION (includeguard)

/* ----- END OF convolve.h ----- */
