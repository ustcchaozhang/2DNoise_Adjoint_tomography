/*! \file wid2isample.cc
 * \brief calculate sample index for given time (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/02/2004
 * 
 * calculate sample index for given time (implementation)
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
 *  - 17/09/2004   V1.1   used wrong operator for rest
 *  - 28/07/2005   V1.2   returned signed sample index
 * 
 * ============================================================================
 */
#define TF_WID2ISAMPLE_CC_VERSION \
  "TF_WID2ISAMPLE_CC   V1.2"

#include <sffxx.h>

namespace sff {

  //! return index offset of sample at idate
  long int wid2isample(const WID2& wid2, 
                       const libtime::TAbsoluteTime& idate)
  {
    libtime::TRelativeTime dt=libtime::double2time(wid2.dt);
    long int retval=(idate-wid2.date)/dt;
    if (idate<wid2.date) retval *= -1;
    return(retval);
  }

  //! return time of sample with index offset i
  libtime::TAbsoluteTime wid2isample(const WID2& wid2, 
                                     const long int& i)
  {
    libtime::TRelativeTime dt=libtime::double2time(wid2.dt);
    libtime::TAbsoluteTime retval=wid2.date+i*dt;
    return(retval);
  }

  //! return residual between idate and sample next to it
  libtime::TRelativeTime wid2isamplerest(const WID2& wid2, 
                                         const libtime::TAbsoluteTime& idate)
  {
    libtime::TRelativeTime dt=libtime::double2time(wid2.dt);
    libtime::TRelativeTime retval=(idate-wid2.date)%dt;
    return(retval);
  }

} // namespace sff

/* ----- END OF wid2isample.cc ----- */
