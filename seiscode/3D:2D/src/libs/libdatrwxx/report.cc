/*! \file report.cc
 * \brief report errors and warnings (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/07/2016
 * 
 * report errors and warnings (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
 * 
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
 *  - 08/07/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_REPORT_CC_VERSION \
  "DATRW_REPORT_CC   V1.0"

#include <datrwxx/report.h>
#include <iostream>
#include <datrwxx/error.h>
#include <datrwxx/aalibdatrwxx.h>
#include <datrwxx/util.h>

using std::cerr;
using std::endl;

namespace datrw {

  namespace util {

    //! report violation of assertion
    void report_violation(const Ereport& t,
                          const std::string& message, 
                          const std::string& file,
                          const int& line,
                          const std::string& condition)
    {
      if (t==Fnonfatal) 
      {
        cerr << "ERROR (non-fatal)";
      }
      else if (t==Fwarning)
      {
        cerr << "WARNING";
      }
      else
      {
        cerr << "ERROR";
      }

      cerr << " reported by " << datrw::libversion << "!" << endl;
      cerr << "| A problem occured while "
        "reading or writing time series data" << endl;
      if (!condition.empty())
      {
        cerr << "| the following condition is violated:" << endl
          << "|   \"" << condition << "\"" << endl;
      }
      if (!file.empty())
      {
        cerr << "| in \"" << file << "\" at line #" << line << endl;
      }
      if (message.empty())
      {
        cerr << "| no further comment is provided." << endl;
      }
      else
      {
        const std::string delimiter("\n");
        std::string::size_type posend=0, pos=0;
        std::string::size_type len=0;
        while (posend!=std::string::npos) 
        {
          posend=message.find(delimiter, pos);
          len=posend-pos;
          cerr << "* ";
          cerr << message.substr(pos, len) << endl;
          if (posend!=std::string::npos) { len+=delimiter.length(); }
          pos+=len;
        } // while (posend!=std::string::npos)
      } // if (message.empty())
      if (t==Ffatal) 
      {
        cerr << "This error is fatal. The program probably will abort..."
          << endl;
      }
      cerr.flush();
    } // void report_violation(...)

    /*----------------------------------------------------------------------*/

    //! report deprecated function
    void report_deprecated(const std::string& function,
                           const std::string& reason)
    {
      cerr << "WARNING: program uses deprecated function" << endl;
      cerr << "    \"" << function << "\"" << endl; 
      cerr << "  from " << datrw::libversion << endl;
      cerr << "  This function should no longer be used because\n"
        << "  " << reason << endl;
      cerr << "  Please report this issue at "
        "http://git.scc.kit.edu/Seitosh/Seitosh"
        << endl;
    }

  } // namespce util

} // namespace datrw

/* ----- END OF report.cc ----- */
