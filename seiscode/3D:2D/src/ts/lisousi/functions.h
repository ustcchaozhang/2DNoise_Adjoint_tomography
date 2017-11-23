/*! \file functions.h
 * \brief lisousi functions (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/04/2013
 * 
 * lisousi functions (prototypes)
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
 *  - 19/04/2013   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FUNCTIONS_H_VERSION

#define TF_FUNCTIONS_H_VERSION \
  "TF_FUNCTIONS_H   V1.0"

#include "lisousi.h"
#include "wnintegration.h"

/*======================================================================*/
/* functions */

Tseries applytaper(const Tseries::Tcoc& input, const Tseries::Tcoc& taper,
                   const double& factor, const double& dt, 
                   const double& offset, const Options& opt);

Tseries filterresponse(const int& size, const double& dt, const Options& opt);

TFourier::Tseries padseries(const TFourier::Tseries& series, 
                            const Options& opt);

TFourier::Tseries singlevelocitytransformation(const TFourier::Tseries& series,
                                               const Parameters& par,
                                               const Exco& ec,
                                               const Options& opt);

TFourier::Tseries tdtapertransformation(TFourier::Tseries series,
                                        Parameters& par,
                                        const Options& opt);

TFourier::Tseries transitionmixer(TFourier::Tseries singlevelocityseries,
                                  TFourier::Tseries directwaveseries,
                                  const Parameters& par,
                                  const Options& opt);

TFourier::Tspectrum zfpointfc(const int& n, const double& dt, 
                              const double& offset, 
                              const double& vs,
                              const double& vp,
                              const bool& debug=false);

TFourier::Tspectrum zflinefc(const int& n, const double& dt, 
                             const double& offset, 
                             const double& vs,
                             const double& vp,
                             const bool& debug=false);

TFourier::Tcoeff hankel(const double& arg);

#endif // TF_FUNCTIONS_H_VERSION (includeguard)

/* ----- END OF functions.h ----- */
