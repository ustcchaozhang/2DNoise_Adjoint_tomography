/*! \file utilitystructures.cc
 * \brief some useful structures (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/01/2008
 * 
 * some useful structures (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 29/01/2008   V1.0   Thomas Forbriger
 *  - 08/08/2008   V1.1   moved global settings to its own translation unit
 * 
 * ============================================================================
 */
#define STUPLO_UTILITYSTRUCTURES_CC_VERSION \
  "STUPLO_UTILITYSTRUCTURES_CC   V1.1"

#include "utilitystructures.h"

namespace stuplo {

  //! compare
  bool operator!=(const GLstyle& a, const GLstyle& b)
  {
    if (a.underlinelabel != b.underlinelabel) return(true);
    if (a.colourlabel != b.colourlabel) return(true);
    if (a.eraselabelbox != b.eraselabelbox) return(true);
    return(false);
  }

} // namespace stuplo

/* ----- END OF utilitystructures.cc ----- */
