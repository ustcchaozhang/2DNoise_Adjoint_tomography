/*! \file matrix.h
 * \brief define Matrix class based on libaff (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/02/2004
 * 
 * define Matrix class based on libaff (prototypes)
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
#ifndef LINEAR_MATRIX_H_VERSION

#define LINEAR_MATRIX_H_VERSION \
  "LINEAR_MATRIX_H   V1.0   "

#include<aff/array.h>

//! Modules of liblinearxx.a
namespace linear {

  typedef aff::Array<double> TDmatrix;
  typedef aff::Array<float> TSmatrix;

} // namespace linear

#endif // LINEAR_MATRIX_H_VERSION (includeguard)

/* ----- END OF matrix.h ----- */
