/*! \file mseed_keywords.cc
 * \brief keywords used in mseed module (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2016
 * 
 * keywords used in mseed module (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ----
 *
 * REVISIONS and CHANGES 
 *  - 05/07/2016   V1.0   Thomas Forbriger
 *  - 12/07/2016   V1.1   add usec consistency check
 * 
 * ============================================================================
 */
#define DATRW_MSEED_KEYWORDS_CC_VERSION \
  "DATRW_MSEED_KEYWORDS_CC   V1.1"

#include <datrwxx/mseed_keywords.h>

namespace datrw {

  namespace mseed {

    const char* const key::dumpascii="dumpascii";
    const char* const key::ttolerance="ttolerance";
    const char* const key::estimateNframes="estimateNframes";
    const char* const key::skipcheck="skipcheck";
    const char* const key::nonfatal="nonfatal";

    const char* const key::nframes="nframes";
    const char* const key::nsamples="nsamples";
    const char* const key::data="data";
    const char* const key::usec="usec";
    const char* const key::all="all";

  } // namespace mseed

} // namespace datrw

/* ----- END OF mseed_keywords.cc ----- */
