/*! \file operators.h
 * \brief matrix and vector operators (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2007
 * 
 * matrix and vector operators (prototypes)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 * We do not overload operators for two reasons:
 * 1. The dot product operator must sum over the correct dimensions. Since NxM
 *    matrices are represented by NxMxKxL libaff arrays the operator would
 *    have the wrong meaning to them.
 * 2. operator+ and operator- are already defined for libaff arrays. They can
 *    be used for our purposes. However, they provide now shape-checking.
 *    Overloading these operators would introduce non-uniqueness to function
 *    name resolution.
 * 
 * REVISIONS and CHANGES 
 *  - 19/12/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef LINEAR_OPERATORS_H_VERSION

#define LINEAR_OPERATORS_H_VERSION \
  "LINEAR_OPERATORS_H   V1.0"

#include<linearxx/matrix.h>

namespace linear {

  /*! \brief Operators for simple matrix and vector operations
   */
  namespace op {

    //! \brief check expected libaff array shape for NxM matrix.
    void checkNxM(const TDmatrix::Tcoc& A);

    /*! \brief dot product for NxM matrices.
     *
     * We cannot simply overload the operator*() function. The general libaff
     * array underlying TDmatrix is an NxMxKxL matrix, even if used for an NxM
     * matrix. In most cases this doesn't matter. But with matrix dot products
     * we have to distinguish.
     */
    TDmatrix dotNxM(const TDmatrix::Tcoc&, const TDmatrix::Tcoc&);

    //! \brief calculate transpose of NxM matrix.
    TDmatrix transposeNxM(const TDmatrix::Tcoc&);

  } // namespace op

} // namespace linear

#endif // LINEAR_OPERATORS_H_VERSION (includeguard)

/* ----- END OF operators.h ----- */
