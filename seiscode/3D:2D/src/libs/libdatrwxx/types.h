/*! \file types.h
 * \brief internal data types (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/10/2011
 * 
 * internal data types (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 06/10/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_TYPES_H_VERSION

#define DATRW_TYPES_H_VERSION \
  "DATRW_TYPES_H   V1.0"

#include <aff/series.h>

namespace datrw {

  typedef aff::Series<double> Tdseries;
  typedef aff::Series<float> Tfseries;
  typedef aff::Series<int> Tiseries;

} // namespace datrw

#endif // DATRW_TYPES_H_VERSION (includeguard)

/* ----- END OF types.h ----- */
