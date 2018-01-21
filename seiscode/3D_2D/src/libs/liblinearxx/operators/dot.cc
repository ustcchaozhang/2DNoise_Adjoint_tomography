/*! \file dot.cc
 * \brief dot product (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2007
 * 
 * dot product (implementation)
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
 *  - 19/12/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define LINEAR_DOT_CC_VERSION \
  "LINEAR_DOT_CC   V1.0   "

#include <linearxx/operators.h>
#include <linearxx/error.h>
#include <aff/shaper.h>

namespace linear {

  namespace op {

    TDmatrix dotNxM(const TDmatrix::Tcoc& A, const TDmatrix::Tcoc& B)
    {
      const int dim1=0;
      const int dim2=1;
      checkNxM(A);
      checkNxM(B);
      LINEAR_assert(A.first(dim2)==B.first(dim1),
                    "size of input matrices does not match!");
      LINEAR_assert(A.last(dim2)==B.last(dim1),
                    "size of input matrices does not match!");
      TDmatrix C(aff::Shaper(A.f(dim1),A.l(dim1))(B.f(dim2),B.l(dim2)));
      for (int i=C.f(dim1); i<= C.l(dim1); ++i)
      {
        for (int j=C.f(dim2); j<= C.l(dim2); ++j)
        {
          C(i,j)=0.;
          for (int k=A.f(dim2); k<=A.l(dim2); ++k)
          {
            C(i,j)+=A(i,k)*B(k,j);
          }
        }
      }
      return(C);
    }

  } // namespace op

} // namespace linear

/* ----- END OF dot.cc ----- */
