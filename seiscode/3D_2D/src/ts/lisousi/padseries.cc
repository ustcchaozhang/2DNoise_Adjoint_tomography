/*! \file padseries.cc
 * \brief pad time series for processing in Fourier domain (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * pad time series for processing in Fourier domain (implementation)
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
#define TF_PADSERIES_CC_VERSION \
  "TF_PADSERIES_CC   V1.0"

#include "lisousi.h"
#include "functions.h"

TFourier::Tseries padseries(const TFourier::Tseries& series, 
                            const Options& opt)
{
  if (opt.verbose)
  {
    cout << "  frequency domain processing; "
      "padding factor: " << opt.npad << endl;
  }
  // convolve by numerical transform
  TFXX_assert(opt.npad>0,
              "padding factor must be 1 or larger");
  int newnsamples=series.size()*opt.npad;

  if (static_cast<int>(workseries.size())<newnsamples)
  {
    cout << "  allocate new memory for "
      << newnsamples << " samples"
      << endl;
    workseries=Tseries(0,newnsamples-1);
  }

  TFourier::Tseries newseries(workseries);
  newseries.setlastindex(newnsamples-1);
  newseries=0.;
  newseries.copyin(series);
  return(newseries);
}

/* ----- END OF padseries.cc ----- */
