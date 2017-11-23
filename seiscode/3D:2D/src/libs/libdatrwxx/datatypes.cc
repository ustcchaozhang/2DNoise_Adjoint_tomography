/*! \file datatypes.cc
 * \brief handle data types and data type conversion (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/12/2010
 * 
 * handle data types and data type conversion (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 17/12/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_DATATYPES_CC_VERSION \
  "DATRW_DATATYPES_CC   V1.0"

#include <datrwxx/datatypes.h>

namespace datrw {

  namespace datatypes {

    bool verbose_type_conversion=false;

    const char* unknown_type_id="UNKNOWN";

    void print_conversion_report(std::ostream& os, 
                                 const std::string& from,
                                 const std::string& to)
    {
      if (verbose_type_conversion)
      {
        if (from==to)
        {
          if (from==std::string(unknown_type_id))
          {
            os << "type conversion: cannot identify types!" << std::endl;
          }
        }
        else
        {
          os << "type conversion: from " << from << " to " << to << std::endl;
        }
      }
    }

  } // namespace datatypes

} // namespace datrw

/* ----- END OF datatypes.cc ----- */
