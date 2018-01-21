/*! \file ipo.cc
 * \brief interpolation interface (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/11/2017
 * 
 * interpolation interface (implementation)
 * 
 * Copyright (c) 2005, 2017 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 13/07/2005   V1.0   Thomas Forbriger
 *  - 28/07/2005   V1.1   indicate wrong time window by specific exception
 *  - 07/11/2017   V1.2   
 *                        - provide new exception to indicate empty time window
 *                        - provide option for automatic adjustment of time
 *                          window in function resample
 * 
 * ============================================================================
 */
#define TF_IPO_CC_VERSION \
  "TF_IPO_CC   V1.2"

#include <tsxx/ipo.h>
#include <tsxx/debug.h>

namespace ts {

  namespace ipo {

    // exception constructor
    ExceptionTimeWindowOutside::ExceptionTimeWindowOutside(const char* message, 
                                                           const char* file,
                                                           const int& line,
                                                           const char* cond)
      : Exception(message, file, line, cond) 
      { }

    /* ----------------------------------------------------------------------
     */

    // exception constructor
    ExceptionTimeWindowEmpty::ExceptionTimeWindowEmpty(const char* message, 
                                                       const char* file,
                                                       const int& line,
                                                       const char* cond)
      : Exception(message, file, line, cond) 
      { }

    /* ======================================================================
     */

    //! resample a time series
    ts::TDsfftimeseries resample(const Interpolator& ip,
                                 const libtime::TAbsoluteTime& first,
                                 const libtime::TRelativeTime& dt,
                                 const int& n,
                                 const bool& shrink)
    {
      typedef Interpolator::Ttimeseries Ttimeseries;
      typedef Ttimeseries::Theader Theader;
      typedef Ttimeseries::Tseries Tseries;
      using libtime::TAbsoluteTime;
      using libtime::TRelativeTime;
      using libtime::TRange;
      Theader hd=ip.header();
      TAbsoluteTime old_first=hd.date;
      TAbsoluteTime old_last=sff::wid2lastsample(hd);
      TAbsoluteTime new_first=first;
      TAbsoluteTime new_last=new_first+(n-1)*dt;
      int nsamples=n;
      // if requested, shrink time window to available data
      if (shrink)
      {
        if (old_first > new_first)
        {
          int noff=(old_first-new_first)/dt;
          new_first += (dt*noff);
          nsamples -= noff;
          while (new_first < old_first) { new_first += dt; nsamples--; }
        }
        if (old_last < new_last)
        {
          int noff=(new_last-old_last)/dt;
          new_last -= (dt*noff);
          nsamples -= noff;
          while (new_last > old_last) { new_last -= dt; nsamples--; }
        }
        TSXX_Xassert(nsamples > 0,
                    "new time window is empty",
                    ExceptionTimeWindowEmpty);
      }
      TSXX_debug(ip.debug(), "resample",
                 "window covered by input series:\n         " 
                 << old_first.timestring() << " - "
                 << old_last.timestring());
      TSXX_debug(ip.debug(), "resample",
                 "window requested for output series:\n         " 
                 << new_first.timestring() << " - "
                 << new_last.timestring());
      TRange inputwindow(old_first, old_last);
      TRange outputwindow(new_first, new_last);
      TSXX_Xassert((inputwindow.includes(outputwindow)),
                  "new time span is outside time series",
                  ExceptionTimeWindowOutside);
      Ttimeseries retval=Ttimeseries(Tseries(nsamples), hd);
      retval.header.date=new_first;
      retval.header.nsamples=nsamples;
      retval.header.dt=libtime::time2double(dt);
      TAbsoluteTime it=new_first;
      for (int i=0; i<nsamples; ++i) { retval(i)=ip(it); it += dt; }
      return(retval);
    } // ts::TDsfftimeseries resample(...)

  } // namespace ipo

} // namespace ts


/* ----- END OF ipo.cc ----- */
