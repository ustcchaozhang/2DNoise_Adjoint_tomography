/*! \file datatrace.h
 * \brief all stuff to handle data traces (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle data traces (prototypes)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * ----
 * 
 * REVISIONS and CHANGES 
 *  - 28/01/2008   V1.0   Thomas Forbriger
 *  - 01/02/2014 thof:    use tsioxx/cmdlinefiles.h (successor of
 *                        tsxx/sffheaders.h and tfxx/readtsdata.h)
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_DATATRACE_H_VERSION

#define STUPLO_DATATRACE_H_VERSION \
  "STUPLO_DATATRACE_H   2014-02-01"

#include <string>
#include <aff/functions/max.h>
#include <aff/functions/min.h>
#include <libtime++.h>
#include <tsioxx/cmdlinefiles.h>
#include <tfxx/stringfunc.h>

#include "scaling.h"
#include "datafile.h"
#include "utilitystructures.h"

namespace stuplo {

  /*----------------------------------------------------------------------*/

  //! struct to hold trace data together with file parameters
  struct DataTrace {
    FileParameters para;
    Ttimeseries ts;
    sff::WID2 wid2() const { return ts.header.wid2(); }
    libtime::TAbsoluteTime first() const { return(this->wid2().date); }
    libtime::TAbsoluteTime last() const 
    { return(sff::wid2lastsample(this->wid2())); }
    Ttimeseries::Tvalue max() const { return(aff::func::max(ts)); }
    Ttimeseries::Tvalue min() const { return(aff::func::min(ts)); }
    libtime::TRange timerange() const
    { return(libtime::TRange(this->first(), this->last())); }
    pgplot::Trange ordinaterange() const;
    //! return time axis series matching panel time
    Tseries time(const PanelTime& pt) const;
    //! return sample index next to time
    int isample(const libtime::TAbsoluteTime& time) const;
    //! return label with pattern appropriately replaced
    std::string label() const;
  }; // struct DataTrace

  //! a list of data traces
  class DataTraceList: public std::list<DataTrace> 
  {
    public:
      typedef std::list<DataTrace> Tbase;
      libtime::TRange timerange() const;
      pgplot::Trange ordinaterange() const;
  }; // class DataTraceList

} // namespace stuplo

#endif // STUPLO_DATATRACE_H_VERSION (includeguard)

/* ----- END OF datatrace.h ----- */
