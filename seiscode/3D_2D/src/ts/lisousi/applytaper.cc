/*! \file lisousi_applytaper.cc
 * \brief apply a taper to the time series (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * apply a taper to the time series (implementation)
 * 
 * Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * lisousi is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * lisousi is distributed in the hope that it will be useful,
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
 *  - 17/04/2013   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_LISOUSI_APPLYTAPER_CC_VERSION \
  "TF_LISOUSI_APPLYTAPER_CC   V1.0   "

#include "lisousi.h"
#include "functions.h"

Tseries applytaper(const Tseries::Tcoc& input, const Tseries::Tcoc& taper,
                   const double& factor, const double& dt, 
                   const double& offset, const Options& opt)
{
  if (opt.verbose)
  {
    cout << "  apply taper function" 
      << " with delay: " << dt*int(opt.tapdel/dt) << "s"
      << endl;
  }
  TFXX_debug(opt.debug, "applytaper", "apply taper to time series\n"
             << "    "
             << "size of series: " << input.size() << "; "
             << "size of taper: " << taper.size());
  // calculate taper delay
  double taperdelay=opt.tapdel;
  if (opt.tapsloset)
  {
    taperdelay=taperdelay<opt.tapslo*offset ? taperdelay : opt.tapslo*offset;
  }
  // prepare return value
  Tseries retval(0,input.size()-1);
  retval=Tseries::Tvalue(0.);
  // prepare iterators
  aff::Browser<Tseries::Tcoc> IT(taper);
  aff::Browser<Tseries::Tcoc> II(input);
  aff::Iterator<Tseries> IS(retval);
  unsigned int i=0;
  // apply taper
  while (IT.valid() && IS.valid() && II.valid())
  {
    double t=i*dt;
    (*IS) = (*II) * (*IT) * factor;
    ++IS;
    ++II;
    if (t >= taperdelay) { ++IT; }
    ++i;
  }
  return(retval);
}

/* ----- END OF lisousi_applytaper.cc ----- */
