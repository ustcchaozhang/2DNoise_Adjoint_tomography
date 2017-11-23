/*! \file formatted_timestring.cc
 * \brief produce a string represenation using the strftime function (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/12/2012
 * 
 * produce a string represenation using the strftime function (implementation)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * libtime is free software; you can redistribute it and/or modify
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
 *  - 16/12/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FORMATTED_TIMESTRING_CC_VERSION \
  "TF_FORMATTED_TIMESTRING_CC   V1.0   "

#include <libtime++.h>
#include <ctime>

namespace libtime {

  /*! \brief return string representation of time.
   *
   * The function takes a format definition and passes it to strftime(3) to
   * receive a formatted time string.
   * \param format format string, see strftime(3)
   * \return time string in requested format
   */
  std::string TAbsoluteTime::timestring(const std::string& format) const
  {
    const int incr=20;
    const int nstart=format.length()+incr;
    int n=nstart;
    bool success=false;
    struct std::tm timetm;
    timetm.tm_year=this->year()-1900;
    timetm.tm_mon=this->month()-1;
    timetm.tm_mday=this->day();
    timetm.tm_yday=this->doy();
    timetm.tm_hour=this->hour();
    timetm.tm_min=this->minute();
    timetm.tm_sec=this->second();
    timetm.tm_isdst=0;
    timetm.tm_wday=0;
    std::string retval;
    while (!success)
    {
      char* ts=new char[n];
      size_t nret=std::strftime(ts, n, format.c_str(), &timetm);
      retval=std::string(ts);
      delete[] ts;
      if ((nret<(n-1))&&(nret>0))
      {
        success=true;
      }
      else
      {
        n+=incr;
        libtime_assert(n<1000,
                       "ERROR TAbsoluteTime::timestring(): "
                       "time string is unreasonably long");
      }
    }
    return retval;
  }

} // namespace libtime

/* ----- END OF formatted_timestring.cc ----- */
