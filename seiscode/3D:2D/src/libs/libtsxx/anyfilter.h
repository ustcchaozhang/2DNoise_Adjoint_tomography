/*! \file anyfilter.h
 * \brief provide access to all filters (seife and other) (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2005
 * 
 * provide access to all filters (seife and other) (prototypes)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 05/07/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_ANYFILTER_H_VERSION

#define TF_ANYFILTER_H_VERSION \
  "TF_ANYFILTER_H   V1.0   "

#include<string>
#include<tsxx/filterbase.h>

namespace ts {

  namespace filter {

    //! combine all filters (seife and others)
    Tfilterhandle make_any_filter(const std::string& s,
                                  const bool& debug=false);

    //! print information on available filters
    void print_any_help(std::ostream& os);

  } // namespace filter

} // namespace ts

#endif // TF_ANYFILTER_H_VERSION (includeguard)

/* ----- END OF anyfilter.h ----- */
