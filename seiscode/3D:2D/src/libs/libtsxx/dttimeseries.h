/*! \file dttimeseries.h
 * \brief time series with basic sampling interval header (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/11/2016
 * 
 * time series with basic sampling interval header (prototypes)
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
 *                 V1.1   provide default constructor
 * 
 * ============================================================================
 */

// include guard
#ifndef TSXX_DTTIMESERIES_H_VERSION

#define TSXX_DTTIMESERIES_H_VERSION \
  "TSXX_DTTIMESERIES_H   V1.1"

#include<tsxx/tsxx.h>

namespace ts {

  /*----------------------------------------------------------------------*/
  // generic version

  /*! \brief simple header                      
   *
   * Adding a sampling interval to a series qualifies it to represent a time
   * series. That's the bare minimum required to make a time series.
   */
  template<class T>
    struct DTHeader {
      typedef T Tvalue;
      DTHeader(const Tvalue& sdt): dt(sdt) { }
      DTHeader(): dt(1) { }
      //! sampling interval
      Tvalue dt;
    }; // DTHeader

  //! double precision version of simple time series
  typedef ts::TimeSeries<aff::Series<double>, DTHeader<double> > TDtimeseries;
  //! single precision version of simple time series
  typedef ts::TimeSeries<aff::Series<float>, DTHeader<float> > TStimeseries;

} // namespace ts

#endif // TSXX_DTTIMESERIES_H_VERSION (includeguard)

/* ----- END OF dttimeseries.h ----- */
