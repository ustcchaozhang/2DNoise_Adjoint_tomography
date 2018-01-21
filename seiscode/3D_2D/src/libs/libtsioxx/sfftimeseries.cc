/*! \file sfftimeseries.cc
 * \brief libtsxx TimeSeries class template with SFF trace header
 *        (implementation).
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 18/07/2005
 * \date 30/01/2014
 * 
 * libtsxx TimeSeries class template with SFF trace header (implementation)
 * 
 * Copyright (c) 2005-2007, 2012, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 30/01/2014   V1.0   Thomas Forbriger (thof):
 *                        copied from sffheaders.cc
 * 
 * ============================================================================
 */
#define TSIO_SFFTIMESERIES_CC_VERSION \
  "TF_SFFTIMESERIES_CC   2014/01/30"

#include <tsioxx/sfftimeseries.h>

namespace ts {

  namespace sff {

    void TraceHeader::info(const ::sff::INFO& s)
    {
      Minfo=s;
      Mhasinfo=true;
    }

    void TraceHeader::free(const ::sff::FREE& f)
    {
      Mfree=f;
      Mhasfree=true;
    }

    void TraceHeader::append(const ::sff::FREE& f)
    {
      Mfree.append(f),
      Mhasfree=true;
    }

  } // namespace sff
} // namespace ts

/* ----- END OF sfftimeseries.cc ----- */
