/*! \file types.h
 * \brief some typedefs we refer to
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * some typedefs we refer to
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
 * \sa \ref sec_main_modules_basic
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 15/12/2002   V1.1   never place in namespace prebuilt
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_TYPES_H_VERSION

#define AFF_TYPES_H_VERSION \
  "AFF_TYPES_H   V1.0   "

#include <cstddef>
 
namespace aff {

  //! Type to hold an array dimensionality.
  typedef unsigned short int  Tdim;
  //! Type to hold the size of an array dimension.
  typedef size_t              Tsize;
  //! Type to hold an array's subscript value.
  typedef ptrdiff_t           Tsubscript;
  /*
  //! Type to hold an array dimensionality.
  typedef unsigned int        Tdim;
  //! Type to hold the size of an array dimension.
  typedef unsigned int        Tsize;
  //! Type to hold an array's subscript value.
  typedef int                 Tsubscript;
  */

} // namespace aff

#endif // AFF_TYPES_H_VERSION (includeguard)

/* ----- END OF types.h ----- */
