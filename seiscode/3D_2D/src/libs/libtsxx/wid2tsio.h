/*! \file wid2tsio.h
 * \brief input/output operators for time series with WID2 header (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/11/2016
 * 
 * input/output operators for time series with WID2 header (prototypes)
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
#ifndef TSXX_WID2TSIO_H_VERSION

#define TSXX_WID2TSIO_H_VERSION \
  "TSXX_WID2TSIO_H   V1.0"

#include<tsxx/wid2timeseries.h>
#include<datrwxx/datread.h>
#include<datrwxx/datwrite.h>
#include<tsxx/debug.h>

namespace ts {
  
  /*! output operator template for time series with WID2 header
   */
  template<class C>
    datrw::odatstream& operator<<(datrw::odatstream& os,
                                  TimeSeries<C, sff::WID2>& s)
  {
    TSXX_debug(os.debug(),
               "datrw::odatstream& operator>>(datrw::odatstream& os,\n"
               "                              TimeSeries<C, sff::WID2>& s)",
               TSXX_value(s.header.line()));
    os << s.header;
    typename TimeSeries<C, sff::WID2>::Tseries series(s);
    os << series;
    return(os);
  } // datrw::odatstream& operator<<(datrw::odatstream& os,
    //                              TimeSeries<C, sff::WID2>& s)

  /* ---------------------------------------------------------------------- */
  
  /*! input operator template for time series with WID2 header
   */
  template<class C>
    datrw::idatstream& operator>>(datrw::idatstream& is,
                                  TimeSeries<C, sff::WID2>& s)
  {
    typename TimeSeries<C, sff::WID2>::Tseries series;
    is >> series;
    s=series;
    is >> s.header;
    TSXX_debug(is.debug(),
               "datrw::idatstream& operator>>(datrw::idatstream& is,\n"
               "                              TimeSeries<C, sff::WID2>& s)",
               TSXX_value(s.header.line()));
    return(is);
  } // datrw::idatstream& operator>>(datrw::idatstream& is,
    //                              TimeSeries<C, sff::WID2>& s)

} // namespace ts

#endif // TSXX_WID2TSIO_H_VERSION (includeguard)

/* ----- END OF wid2tsio.h ----- */
