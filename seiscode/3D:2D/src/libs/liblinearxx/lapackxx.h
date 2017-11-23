/*! \file lapackxx.h
 * \brief prototypes for C++ LAPACK interface functions (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/02/2004
 * 
 * prototypes for C++ LAPACK interface functions (prototypes)
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
 *  - 08/02/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef LINEAR_LAPACKXX_H_VERSION

#define LINEAR_LAPACKXX_H_VERSION \
  "LINEAR_LAPACKXX_H   V1.0   "

#include<linearxx/matrix.h>

/*! \mainpage
 *
 * \author Thomas Forbriger
 * \since 2004
 *
 * This library provides a C++ interface to LAPACK.
 * It uses libaff arrays to store matrix data.
 * The library builds a framework to be extended in the future with further
 * interface functions to LAPACK.
 */

namespace linear {

//! lapack interface routines
namespace lapack {
  //
  //! Compute the solution to a real system of linear equations  A * X = B.
  void dposv(char UPLO, TDmatrix& A, TDmatrix& B, int& INFO, 
             const bool& debug=false);
  TDmatrix dposv(const TDmatrix::Tcoc& A, 
                 const TDmatrix::Tcoc& B, 
                 char UPLO='U', const bool& debug=false);

} // namespace lapack
} // namespace linear

#endif // LINEAR_LAPACKXX_H_VERSION (includeguard)

/* ----- END OF lapackxx.h ----- */
