/*! \file lisousi_filterresponse.cc
 * \brief create a 1/sqrt(t) filter response for given parameters (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * create a 1/sqrt(t) filter response for given parameters (implementation)
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
#define TF_LISOUSI_FILTERRESPONSE_CC_VERSION \
  "TF_LISOUSI_FILTERRESPONSE_CC   V1.0   "

#include "lisousi.h"
#include "functions.h"

/* create a 1/sqrt(t) filter response */
Tseries filterresponse(const int& size, const double& dt, const Options& opt)
{
  if (opt.verbose)
  {
    cout << "  construct 1/sqrt(t); ";
    if (opt.nointeg)
    {
      cout << "tshift: " << opt.tshift << "; "
        << "tlim: " << opt.tlim << "; "
        << "tfac: " << opt.tfac << endl;
    }
    else
    {
      cout << "integshift: " << opt.integshift << " means " 
        << opt.integshift*dt << "s time shift" << endl;
    }
  }
  Tseries retval=Tseries(0,size-1);
  aff::Iterator<Tseries> I(retval);
  for (unsigned int i=0; i<retval.size(); ++i)
  {
    if (opt.nointeg)
    {
      double t=dt*(double(i)+opt.tshift);
      if (t < (opt.tlim*dt)) { t=opt.tfac*dt; }
      *I=sqrt(1./t);
    }
    else
    {
      if (i == 0)
      {
        double tref=1.-opt.integshift;
        *I=2.*sqrt(tref/dt);
      }
      else
      {
        double tref=(double(i)-opt.integshift);
        *I=2.*(sqrt(tref+1)-sqrt(tref))/sqrt(dt);
      }
    }
    ++I;
  }
  return(retval);
}

/* ----- END OF lisousi_filterresponse.cc ----- */
