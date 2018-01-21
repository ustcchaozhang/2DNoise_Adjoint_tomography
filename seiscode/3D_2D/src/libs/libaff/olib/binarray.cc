/*! \file binarray.cc
 * \brief explicit array template instantiation for standard template parameters (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 01/05/2002
 * 
 * explicit array template instantiation for standard template parameters
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
 * (implementation)
 *
 * We have to compile into separate object files. Up to now the linker is not
 * able to extract only those classes that re definitely used by our code.
 * This is only possible if every class is compiled into a different object
 * file. To accomplish multiple object files from only one source file, we
 * pass template parameters through preprocessor macros.
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 01/05/2002   V1.0   Thomas Forbriger
 *  - 22/11/2002   V1.1   shifted to Strided being default shape
 * 
 * ============================================================================
 */
#define TF_BINARRAY_CC_VERSION \
  "TF_BINARRAY_CC   V1.1   "

#include<complex>

// we are compiling the binary library
#define TF_COMPILING_LIBRARY

#include <contxx/binarray.h>
#include <contxx/shape/densestrided.h>
#include <contxx/shape/densestrided_repr.h>

#ifndef BIN_TYPE
#error "macro BIN_TYPE (template parameter) not defined!"
#endif

#ifndef BIN_N
#error "macro BIN_N (template parameter) not defined!"
#endif

#ifndef BIN_REPR
#error "macro BIN_REPR (template parameter) not defined!"
#endif

#ifndef BIN_SUBS
#error "macro BIN_SUBS (template parameter) not defined!"
#endif
  
// explicit instatiations for common types:
template class contxx::prebuilt::Array<BIN_TYPE, BIN_N, BIN_REPR, BIN_SUBS>;

/* ----- END OF binarray.cc ----- */
