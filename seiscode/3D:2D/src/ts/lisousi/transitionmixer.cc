/*! \file transitionmixer.cc
 * \brief offset dependent mixer for two approaches (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * offset dependent mixer for two approaches (implementation)
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
#define TF_TRANSITIONMIXER_CC_VERSION \
  "TF_TRANSITIONMIXER_CC   V1.0"

#include "lisousi.h"
#include "functions.h"

TFourier::Tseries transitionmixer(TFourier::Tseries singlevelocityseries,
                                  TFourier::Tseries directwaveseries,
                                  const Parameters& par,
                                  const Options& opt)
{
  TFourier::Tseries series;

  // trim number of samples
  if (opt.transition1<=par.offset)
  {
    directwaveseries.setlastindex(par.nsamples+
                                  directwaveseries.f()-1);
  }
  if (opt.transition2>=par.offset)
  {
    singlevelocityseries.setlastindex(par.nsamples+
                                      singlevelocityseries.f()-1);
  }
  // report offset
  if (opt.verbose)
  {
    cout << "  offset " << par.offset << "m: ";
  }
  // enter mixing operation
  if (opt.transition1>=par.offset)
  {
    series=singlevelocityseries;
    if (opt.verbose)
    {
      cout << "100% single velocity transformation" << endl;
    }
  }
  else if (opt.transition2<=par.offset)
  {
    series=directwaveseries;
    if (opt.verbose)
    {
      cout << "100% direct wave transformation" << endl;
    }
  }
  else
  {
    const double dwf=(par.offset-opt.transition1)/
      (opt.transition2-opt.transition1);
    const double svf=1.-dwf;
    series=Tseries(par.nsamples);
    if (opt.verbose)
    {
      cout << 100.*svf << "% single velocity and "
        << 100.*dwf << "% direct wave transformation" << endl;
    }

    aff::Browser<Tseries> DWB(directwaveseries);
    aff::Browser<Tseries> SVB(singlevelocityseries);
    aff::Iterator<Tseries> SI(series);
    while (DWB.valid() && SVB.valid() && SI.valid())
    {
      (*SI) = dwf*(*DWB) + svf*(*SVB);
      ++DWB;
      ++SVB;
      ++SI;
    }
  }
  return(series);
}

/* ----- END OF transitionmixer.cc ----- */
