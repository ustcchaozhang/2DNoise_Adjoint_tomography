/*! \file gsexx_TDAT2.h
 * \brief helper function prototypes for TDAT2 (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 29/03/2002
 * 
 * helper function prototypes for TDAT2 (prototypes)
 *
 * \note
 * This file is not used by the public. It is only for internal use to make
 * the subformat reading and writing functions available to the TDAT2 classes.
 *
 * \sa GSE2::waveform::TDAT2read
 * \sa GSE2::waveform::TDAT2write
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * libgsexx is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 29/03/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_GSEXX_TDAT2_H_VERSION

#define TF_GSEXX_TDAT2_H_VERSION \
  "TF_GSEXX_TDAT2_H   V1.0   "
#define TF_GSEXX_TDAT2_H_CVSID \
  "$Id$"

#include <gsexx.h>
#include <string>
#include <iostream>

namespace GSE2 {
namespace waveform {

//! Funtions for reading and writing %CM6 encoded data.
namespace CM6 {

//! %CM6 subformat encoding function.
std::string encode(const intT& value);

//! %CM6 subformat decoding function.
intT decode(std::istream& is);

} // namespace cm6

} // namespace waveform
} // namespace GSE2

#endif // TF_GSEXX_TDAT2_H_VERSION (includeguard)

/* ----- END OF gsexx_TDAT2.h ----- */
