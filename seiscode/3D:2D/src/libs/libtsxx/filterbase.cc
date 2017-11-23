/*! \file filterbase.cc
 * \brief base class for all filter classes (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2005
 * 
 * base class for all filter classes (implementation)
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
 *  - 18/12/2007   V1.1   - support debugging
 * 
 * ============================================================================
 */
#define TF_FILTERBASE_CC_VERSION \
  "TF_FILTERBASE_CC   V1.1"

#include <iostream>
#include <tsxx/filterbase.h>
#include <tsxx/debug.h>

namespace ts {

  namespace filter {

    Ttimeseries FilterCollection::operator()(const Ttimeseries& s,
                                             const bool& debug) const
    {
      Tfilterlist::const_iterator i=this->Tfilterlist::begin();
      while (i!=this->Tfilterlist::end())
      { 
        TSXX_debug(debug, "FilterCollection::operator()",
                   "apply filter" );
        (*i)->operator()(s, debug); 
        ++i;
      }
      TSXX_debug(debug, "FilterCollection::operator()",
                 "applied all filters" );
      return s;
    }

    void Exception::report() const {
      this->Tbase::report();
    }

    void UnknownFilterException::report() const {
      this->Tbase::report();
      std::cerr << "requested filter type: \"" << Mfilter << "\""
        << std::endl;
    }

    // place these into the binary library
    BasicFilter::BasicFilter() { }
    BasicFilter::~BasicFilter() { }

  } // namespace filter

} // namespace ts

/* ----- END OF filterbase.cc ----- */
