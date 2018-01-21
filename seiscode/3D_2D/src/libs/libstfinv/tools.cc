/*! \file tools.cc
 * \brief tools and utilities (implementation)
 * 
 * \ingroup group_tools
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/05/2011
 * 
 * tools and utilities (implementation)
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
 * 
 * REVISIONS and CHANGES 
 *  - 28/05/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STFINV_TOOLS_CC_VERSION \
  "STFINV_TOOLS_CC   V1.0"

#include <cmath>
#include <stfinv/tools.h>

namespace stfinv {

  namespace tools {

    bool sameineps(const double &a, const double& b, const double& eps)
    {
      double reldif=std::abs(a-b);
      return(reldif<=(std::abs(b*eps)));
    } // bool sameineps(const double &a, const double& b, const double& eps)

  } // namespace tools

} // namespace stfinv

/* ----- END OF tools.cc ----- */
