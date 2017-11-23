/*! \file wid2timeseries.h
 * \brief time series with WID2 header (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/11/2016
 * 
 * time series with WID2 header (prototypes)
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
 *  - 22/11/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TSXX_WID2TIMESERIES_H_VERSION

#define TSXX_WID2TIMESERIES_H_VERSION \
  "TSXX_WID2TIMESERIES_H   V1.0"

#include<tsxx/tsxx.h>
#include<sffxx.h>

namespace ts {

  /*----------------------------------------------------------------------*/
  // SFF version
    
  //! double precision data with SFF header
  typedef TimeSeries<aff::Series<double>, sff::WID2> TDsfftimeseries;
  //! single precision data with SFF header
  typedef TimeSeries<aff::Series<float>, sff::WID2> TSsfftimeseries;
  //! integer data with SFF header
  typedef TimeSeries<aff::Series<int>, sff::WID2> TIsfftimeseries;

} // namespace ts

#endif // TSXX_WID2TIMESERIES_H_VERSION (includeguard)

/* ----- END OF wid2timeseries.h ----- */
