/*! \file util.cc
 * \brief utilities used by more than one type of data reader (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/12/2004
 * 
 * utilities used by more than one type of data reader (implementation)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 22/12/2004   V1.0   Thomas Forbriger
 *  - 29/07/2011   V1.1   added clipstring
 *  - 06/09/2011   V1.2   moved string manipulation functions to this file:
 *                        commatospace and trimws
 * 
 * ============================================================================
 */
#define DATRW_UTIL_CC_VERSION \
  "DATRW_UTIL_CC   V1.2"

#include <cmath>
#include <algorithm>
#include <datrwxx/util.h>

namespace datrw {

  namespace util {

    //! return number of significant digits
    int nsignificantdigits(double v, const bool& debug)
    {
      const double epsilon=1.e-30;
      if (debug) 
      { 
        std::cerr << "DEBUG (nsignificantdigits): "
          << "entered function for value:" << v << std::endl; 
      }
      if (v<0) { v = -v; }
      int n=0;
      if (v<epsilon) 
      {
        n=2;
      }
      else
      {
        double basefactor=pow(10.,-floor(log10(v)));
        double v1,v2;
        if (debug) 
        { 
          std::cerr << "DEBUG (nsignificantdigits): "
            << "basefactor: " << basefactor << " "
            << "v: " << v << " "
            << std::endl; 
        }
        do
        { 
          double factor=basefactor*pow(10.,n);
          v1=v*factor;
          v2=floor(v1);
          n++;
          if (debug) 
          { 
            std::cerr << "DEBUG (nsignificantdigits): "
              << "factor: " << factor << " "
              << "n: " << n << " "
              << "v1: " << v1 << " "
              << "v2: " << v2 << " "
              << std::endl; 
          }
        }
        while (v1 != v2);
      }
      return (n);
    } // int nsignificantdigits(const double& v)

    /*----------------------------------------------------------------------*/

    int ntrailingdigits(double v, const bool& debug)
    {
      int retval=nsignificantdigits(v, debug)-floor(log10(v))-1;
      return(retval);
    } // int ntrailingdigits(double v, const bool& debug=false)

    /*----------------------------------------------------------------------*/

    std::string clipstring(std::string& s, const std::string& delim)
    {
      std::string::size_type i=s.find(delim);
      std::string result;
      if ((i>=0) && (i<s.length())) {
        result=s.substr(0,i);
        s.erase(0,i+delim.length());
      } else {
        result=s;
        s.erase();
      }
      return(result);
    } // std::string clipstring

    /*----------------------------------------------------------------------*/

    std::string commatospace(std::string s)
    {
      std::replace(s.begin(), s.end(), ',', ' ');
      return(s);
    } // std::string commatospace(const std::string& s)

    /*----------------------------------------------------------------------*/

    std::string trimws(std::string s)
    {
      if (s.length()>0)
      {
        std::string::size_type ib=s.find_first_not_of(" ", 0);
        if (ib==std::string::npos)
        {
          s="";
        }
        else
        {
          std::string::size_type il=s.find_last_not_of(" \r", s.length());
          std::string::size_type n=il>=ib ? il-ib+1 : 0;
          if (n==0) { ib = 0; }
          if ((ib!=0) || (n!=s.length())) { s=s.substr(ib,n); }
        }
      }
      return(s);
    } // std::string trimws(std::string s)

  } // namespace util

} // namespace datrw

/* ----- END OF util.cc ----- */
