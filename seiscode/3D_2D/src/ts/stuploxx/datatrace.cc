/*! \file datatrace.cc
 * \brief all stuff to handle data traces (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2008
 * 
 * all stuff to handle data traces (implementation)
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
 *  - 17/03/2015   V1.1   adjust libpgplotxx interface
 * 
 * ============================================================================
 */
#define STUPLO_DATATRACE_CC_VERSION \
  "STUPLO_DATATRACE_CC   V1.1   (17-03-2015)"

#include <pgplotxx/affpgplot.h>
#include <sstream>

#include "datatrace.h"

namespace stuplo {

  pgplot::Trange DataTrace::ordinaterange() const
  {
    pgplot::Trange retval;
    if (this->para.fixedordinatelimits) 
    {
      retval=this->para.ordinatelimits;
    }
    else
    {
      retval=pgplot::pgaff::series_value_range(this->ts);
      retval.extendf(0.5*(this->para.ordinatescalefactor-1.));
    }
    return(retval);
  } // pgplot::Trange DataTrace::ordinaterange() const

  /*----------------------------------------------------------------------*/

  int DataTrace::isample(const libtime::TAbsoluteTime& time) const
  {
    int retval;
    retval=sff::wid2isample(this->wid2(), time);
    return(retval);
  } // int DataTrace::isample(pgplot::TAbsoluteTime) const

  /*----------------------------------------------------------------------*/

  Tseries DataTrace::time(const PanelTime& pt) const
  {
    int ifirst=this->isample(pt.range().begin());
    int ilast=this->isample(pt.range().end());
    ifirst = ifirst > ts.first() ? ifirst : ts.first();
    ilast = ilast < ts.last() ? ilast : ts.last();
    ilast = ilast > ifirst ? ilast : ifirst;
    Tseries retval(ifirst,ilast);
    float foffset=pt.foffset(sff::wid2isample(this->wid2(), ifirst));
    double dt=this->wid2().dt;
    for (int i=ifirst; i<=ilast; ++i)
    { retval(i)=foffset+dt*(i-ifirst); }
    return (retval);
  } // Tseries DataTrace::time(const PanelTime& pt) const

  /*----------------------------------------------------------------------*/
  
  //! return label with pattern appropriately replaced
  std::string DataTrace::label() const
  {
    std::string retval=this->para.label;

    std::ostringstream oss;
    
    std::string timestring=this->wid2().date.timestring();
    // pattern to be replaced by file name
    retval=tfxx::string::patsubst(retval, filenamepattern,
                                  this->para.filename.name);
    // pattern to be replaced by file number
    oss.str("");
    oss << this->para.ifile;
    retval=tfxx::string::patsubst(retval, filenumberpattern,
                                  tfxx::string::trimws(oss.str()));
    // pattern to be replaced by trace number
    oss.str("");
    oss << this->ts.traceindex();
    retval=tfxx::string::patsubst(retval, tracenumberpattern,
                                  tfxx::string::trimws(oss.str()));
    // pattern to be replaced by date of first samples
    retval=tfxx::string::patsubst(retval, datepattern,
                                  timestring.substr(4,10));
    // pattern to be replaced by time of first samples
    retval=tfxx::string::patsubst(retval, timepattern,
                                  timestring.substr(15,8));
    // pattern to be replaced by time of first samples
    retval=tfxx::string::patsubst(retval, utimepattern,
                                  timestring.substr(15,15));
    // pattern to be replaced by station identifier
    retval=tfxx::string::patsubst(retval, stationpattern,
                                  tfxx::string::trimws(this->wid2().station));
    // pattern to be replaced by channel identifier
    retval=tfxx::string::patsubst(retval, channelpattern,
                                  tfxx::string::trimws(this->wid2().channel));
    // pattern to be replaced by auxiliary identifier
    retval=tfxx::string::patsubst(retval, auxiliarypattern,
                                  tfxx::string::trimws(this->wid2().auxid));
    // pattern to be replaced by instrument identifier
    retval=tfxx::string::patsubst(retval, instrumentpattern,
                                  tfxx::string::trimws(this->wid2().instype));
    // double-per-cent to be replaced by per-cent
    retval=tfxx::string::patsubst(retval, percentpattern, "%");

    return(retval);
  } // std::string DataTrace::label() const

  /*======================================================================*/

  libtime::TRange DataTraceList::timerange() const
  {
    Tbase::const_iterator I=this->begin();
    libtime::TRange retval(I->timerange());
    while (I != this->end())
    {
      retval.expand(I->timerange());
      ++I;
    }
    return(retval);
  } // libtime::TRange DataTraceList::timerange() const

  /*----------------------------------------------------------------------*/

  pgplot::Trange DataTraceList::ordinaterange() const
  {
    Tbase::const_iterator I=this->begin();
    pgplot::Trange retval(I->ordinaterange());
    while (I != this->end())
    {
      retval.extend(I->ordinaterange());
      ++I;
    }
    return(retval);
  } // pgplot::Trange DataTraceList::ordinaterange() const

} // namespace stuplo

/* ----- END OF datatrace.cc ----- */
