/*! \file dump_strided.h
 * \brief factored out Strided dump function (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2002
 * 
 * factored out Strided dump function (prototypes)
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
 * \ingroup group_helpers
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 19/12/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_DUMP_STRIDED_H_VERSION

#define AFF_DUMP_STRIDED_H_VERSION \
  "AFF_DUMP_STRIDED_H   V1.0   "

#include<iostream>
#include<aff/lib/strided.h>

namespace aff {

using std::endl;

/*! \brief Dump array shape information
 *
 * \ingroup group_helpers
 */
void dump(const Strided& shape, std::ostream& os=std::cout);

/*----------------------------------------------------------------------*/

/*! \brief Dump array shape mapping
 *
 * \ingroup group_helpers
 */
void dump_map(const Strided& shape, const Tdim& i=(Strided::Mmax_dimen-1),
              std::ostream& os=std::cout);

} // namespace aff

#endif // AFF_DUMP_STRIDED_H_VERSION (includeguard)

/* ----- END OF dump_strided.h ----- */
