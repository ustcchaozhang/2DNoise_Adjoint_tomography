/*! \file thiesdl1line.cc
 * \brief handle a ThiesDL1 data line (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * handle a ThiesDL1 data line (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 13/09/2011   V1.0   Thomas Forbriger
 *  - 25/10/2012   V1.1   resolved rounding error when deriving counts from
 *                        sample values
 * 
 * ============================================================================
 */
#define DATRW_THIESDL1LINE_CC_VERSION \
  "DATRW_THIESDL1LINE_CC   V1.1"

#include <sstream>
#include <cmath>
#include <datrwxx/thiesdl1line.h>

namespace datrw {

  namespace thiesdl1 {

    namespace helper {

      const libtime::TRelativeTime chalfminute(0,0,0,30);
      const libtime::TRelativeTime coneday(1);

    } // namespace helper

    void ExceptionInconsistentLine::report() const {
      this->Exception::report();
      this->my_report();
    } // void ExceptionInconsistentLine::report() const

    /*----------------------------------------------------------------------*/

    void ExceptionInconsistentLine::my_report() const {
      std::cerr  << "  DL1 data line: " << this->Mdataline;
    } // void ExceptionInconsistentLine::my_report() const

    /*======================================================================*/

    const double DataLine::gain=0.1;

    /*----------------------------------------------------------------------*/

    DataLine::DataLine(const std::string& line)
    {
      Mline=line;
      this->evalline();
    } // DataLine::DataLine(const std::string& line)

    /*----------------------------------------------------------------------*/

#define DL1_dlassert( C, M, L ) \
    if (!(C)) { throw( ExceptionInconsistentLine( M , __FILE__, \
                                                       __LINE__, #C, L )); }

    typedef unsigned int Tuint;

    // example data line:
    // 00.017   0.0 01.12.08 00:01
    // 0000000000111111111122222222223
    // 0123456789012345678901234567890
    void DataLine::evalline()
    {
      DL1_dlassert(Mline.length()==27, "unexpected length of data line", Mline);
      // check for expected characters
      const char cmismatch[]="character mismatch";
      DL1_dlassert(Mline.substr(2,1)==".", cmismatch, Mline);
      DL1_dlassert(Mline.substr(10,1)==".", cmismatch, Mline);
      DL1_dlassert(Mline.substr(15,1)==".", cmismatch, Mline);
      DL1_dlassert(Mline.substr(18,1)==".", cmismatch, Mline);
      DL1_dlassert(Mline.substr(24,1)==":", cmismatch, Mline);
      DL1_dlassert(Mline.substr(6,1)==" ", cmismatch, Mline);
      DL1_dlassert(Mline.substr(12,1)==" ", cmismatch, Mline);
      DL1_dlassert(Mline.substr(21,1)==" ", cmismatch, Mline);
      // extract time and date
      int year, month, day, hour, minute;
      std::istringstream iss;
      iss.clear();
      iss.str(Mline.substr(13,2));
      iss >> day;
      iss.clear();
      iss.str(Mline.substr(16,2));
      iss >> month;
      iss.clear();
      iss.str(Mline.substr(19,2));
      iss >> year;
      iss.clear();
      iss.str(Mline.substr(22,2));
      iss >> hour;
      iss.clear();
      iss.str(Mline.substr(25,2));
      iss >> minute;
      if (year < 100) { year += 1900; }
      if (year < 1970) { year += 100; }
      libtime::TAbsoluteTime intime(year,month,day,hour,minute);
      // check time and date
      if (hour!=24)
      {
        DL1_dlassert(((intime.year()==year)
                      && (intime.month()==month)
                      && (intime.day()==day)
                      && (intime.hour()==hour)
                      && (intime.minute()==minute)),
                     "invalid date value", Mline);
      }
      else
      {
        DL1_dlassert((minute==0),
                     "invalid time value (expected: 24:00UT)", Mline);
        libtime::TAbsoluteTime thedayonly(intime-helper::coneday);
        DL1_dlassert(((thedayonly.year()==year)
                      && (thedayonly.month()==month)
                      && (thedayonly.day()==day)),
                     "invalid date value", Mline);
      }
      Mtime=intime-helper::chalfminute;
      // check float value for time
      double ftime=double(hour)+int(0.5+1000.*double(minute)/60.)/1000.;
      double rftime;
      iss.clear();
      iss.str(Mline.substr(0,6));
      iss >> rftime;
      double diff=ftime-rftime;
      if (diff<0) { diff=-diff; }
      if (!(diff < 0.002))
      { 
        /*
        Logger(log_err) << "ftime: " << ftime 
          << " rftime: " << rftime 
          << " diff: " << diff; 
          */
      }
      DL1_dlassert(diff < 0.002, 
                   "inconsistent floating point time values", Mline);
      // read data value
      iss.clear();
      iss.str(Mline.substr(7,6));
      iss >> this->Mvalue;
      this->Mcounts=Tuint(nearbyint(Mvalue/DataLine::gain));
    } // DataLine::evalline()

  } // namespace thiesdl1

} // namespace datrw

/* ----- END OF thiesdl1line.cc ----- */
