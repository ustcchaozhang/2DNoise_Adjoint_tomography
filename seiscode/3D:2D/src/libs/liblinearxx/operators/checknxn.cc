/*! \file checknxn.cc
 * \brief check for reasonable shape (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/12/2007
 * 
 * check for reasonable shape (implementation)
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
 * REVISIONS and CHANGES 
 *  - 20/12/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define LINEAR_CHECKNXN_CC_VERSION \
  "LINEAR_CHECKNXN_CC   V1.0"

#include <linearxx/operators.h>
#include <linearxx/error.h>

namespace linear {

  namespace op {

    void checkNxM(const TDmatrix::Tcoc& A)
    {
      const int dim3=2;
      const int dim4=3;
      LINEAR_assert(A.first(dim3)==A.last(dim3),
                    "unexpected shape of matrix!");
      LINEAR_assert(A.first(dim4)==A.last(dim4),
                    "unexpected shape of matrix!");
    }

  } // namespace op

} // namespace linear

/* ----- END OF checknxn.cc ----- */
