/*! \file convert.cc
 * \brief convert time representation (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/02/2004
 * 
 * convert time representation (implementation)
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * libtime is free software; you can redistribute it and/or modify
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
 *  - 06/02/2004   V1.0   Thomas Forbriger
 *  - 17/12/2007   V1.1   replace long int by typedef timeint
 * 
 * ============================================================================
 */
#define TF_CONVERT_CC_VERSION \
  "TF_CONVERT_CC   V1.1"

#include <libtime++.h>

namespace libtime {

TRelativeTime double2time(const double& seconds)
{
  libtime_assert(seconds>=0,
                 "ERROR (double2time): only positive values are accepted");
  time_kernel::time_Ts thetime_Ts(TRelativeTime(0));
  double remain=seconds;
  typedef timeint li;
  thetime_Ts.second=li(seconds);
  remain-=double(thetime_Ts.second);
  remain*=1.e3;
  thetime_Ts.milsec=li(remain);
  remain-=double(thetime_Ts.milsec);
  remain*=1.e3;
  thetime_Ts.micsec=li(remain);
  return(TRelativeTime(thetime_Ts));
} // TRelativeTime double2time(const double& seconds)

double time2double(const TRelativeTime& rtime)
{
  double retval(0.);
  retval=rtime.float_second();
  retval += 60.*rtime.minute();
  retval += 3600.*rtime.hour();
  retval += 24.*3600.*rtime.days();
  return(retval);
} // double time2float(const TRelativeTime&)

} // namespace libtime

/* ----- END OF convert.cc ----- */
