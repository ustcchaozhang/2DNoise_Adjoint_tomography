/*! \file wid2lastsample.cc
 * \brief return time of last sample (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/02/2004
 * 
 * return time of last sample (implementation)
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 06/02/2004   V1.0   Thomas Forbriger
 *  - 17/09/2004   V1.1   corrected offset by one error
 * 
 * ============================================================================
 */
#define TF_WID2LASTSAMPLE_CC_VERSION \
  "TF_WID2LASTSAMPLE_CC   V1.1   "

#include <sffxx.h>

namespace sff {

  //! return date of last sample
  libtime::TAbsoluteTime wid2lastsample(const WID2& wid2)
  {
    libtime::TRelativeTime dt=libtime::double2time(wid2.dt);
    libtime::TAbsoluteTime retval=wid2.date+(dt*(wid2.nsamples-1));
    return(retval);
  }

  //! return date of sample after last sample
  libtime::TAbsoluteTime wid2nextdate(const WID2& wid2)
  {
    libtime::TRelativeTime dt=libtime::double2time(wid2.dt);
    libtime::TAbsoluteTime retval=wid2.date+(dt*wid2.nsamples);
    return(retval);
  }

} // namespace sff

/* ----- END OF wid2lastsample.cc ----- */
