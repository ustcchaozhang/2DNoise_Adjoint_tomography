/*! \file helper.cc
 * \brief some helper functions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2010
 * 
 * some helper functions (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 18/11/2010   V1.0   Thomas Forbriger
 *  - 25/11/2010   V1.1   implemented more efficient string trimming
 *  - 02/04/2011   V1.2   implement WID2container::WID2struct
 *  - 05/07/2014   V1.3   move WID2container codse to wid2container.cc
 * 
 * ============================================================================
 */
#define FAPIDXX_HELPER_CC_VERSION \
  "FAPIDXX_HELPER_CC   V1.3"

#include<algorithm>
#include<sstream>
#include<cstring>
#include <fapidxx/helper.h>
#include <fapidxx/wid2container.h>
#include <fapidxx/error.h>

namespace fapidxx {

  //! create a C++ string from a Fortran string
  std::string stringfromfstring(char *fstring, ftnlen slen)
  {
    std::string cxxstring(fstring, slen);
    return(trimws_end(cxxstring));
  } // std::string stringfromfstring(char *fstring, ftnlen slen)

  /*----------------------------------------------------------------------*/

  //! fill a Fortran string with a C++ string
  void fillfstring(const std::string& s, char *fstring, ftnlen slen)
  {
    std::string::const_iterator is=s.begin();
    char *op=fstring;
    for (int i=0; i<slen; ++i)
    {
      if (i<int(s.size()))
      {
        *op=*is;
      }
      else
      {
        *op=' ';
      }
      ++op;
      ++is;
    }
  } // void fillfstring(const std::string& s, char *fstring, ftnlen slen)

  /*----------------------------------------------------------------------*/

  //! remove whitespace at begin and end of string
  std::string trimws(std::string s)
  {
    if (s.length()>0)
    {
      string::size_type ib=s.find_first_not_of(" ", 0);
      if (ib==string::npos)
      {
        s="";
      }
      else
      {
        string::size_type il=s.find_last_not_of(" \r", s.length());
        string::size_type n=il>=ib ? il-ib+1 : 0;
        if (n==0) { ib = 0; }
        if ((ib!=0) || (n!=s.length())) { s=s.substr(ib,n); }
      }
    }
    return(s);
  } // std::string trimws(std::string s)

  /*----------------------------------------------------------------------*/

  //! remove whitespace at end of string
  std::string trimws_end(std::string s)
  {
    if (s.length()>0)
    {
      string::size_type il=s.find_last_not_of(" \r", s.length());
      string::size_type n=il>=0 ? il+1 : 0;
      if (n!=s.length()) { s=s.substr(0,n); }
    }
    return(s);
  } // std::string trimws_end(std::string s)

  /*----------------------------------------------------------------------*/

  float maketanf(const libtime::TAbsoluteTime& time)
  {
    libtime::TAbsoluteTime theday(time.year(), time.month(), time.day());
    libtime::TRelativeTime inday=time-theday;
    return(float(libtime::time2double(inday)));
  } // float maketanf(const libtime::TAbsoluteTime& time)

  /*----------------------------------------------------------------------*/

  libtime::TAbsoluteTime SRCEdate(char *date, char* time, 
                                  ftnlen date_len, ftnlen time_len)
  {
    std::string datestring=stringfromfstring(date, date_len);
    std::string timestring=stringfromfstring(time, time_len);
    std::string fulldate("");
    fulldate+=datestring.substr(0,2);
    fulldate+="/";
    fulldate+=datestring.substr(2,2);
    fulldate+="/";
    fulldate+=datestring.substr(4,2);
    fulldate+=" ";
    fulldate+=timestring.substr(0,2);
    fulldate+=":";
    fulldate+=timestring.substr(2,2);
    fulldate+=":";
    fulldate+=timestring.substr(4,6);
    libtime::TAbsoluteTime srcedate(fulldate);
    return(srcedate);
  } // libtime::TAbsoluteTime SRCEdate

  /*----------------------------------------------------------------------*/

  sff::FREE freeblock(integer *nline, char *lines, ftnlen lines_len)
  {
    sff::FREE free;
    char* pfree=lines;
    for (int ifline=0; ifline<*nline; ++ifline)
    {
      free.append(stringfromfstring(pfree, lines_len));
      pfree += lines_len;
    }
    return(free);
  } // sff::FREE freeblock(integer *nline, char *lines, ftnlen lines_len)

  /*----------------------------------------------------------------------*/

  void freeblock(const ::sff::FREE& free,
                 integer *nline, char *lines, integer *lindim, 
                 integer *lenmax, ftnlen lines_len)
  {
    int ifline=0;
    *lenmax=0;
    typedef sff::FREE::Tlines Tlines;
    Tlines::const_iterator I=free.lines.begin();
    char* pfree=lines;
    while ((I!=free.lines.end()) && (ifline< *lindim))
    {
      int linelen=static_cast<int>(I->length());
      *lenmax= *lenmax>linelen ? *lenmax : linelen;
      fillfstring(*I, pfree, lines_len);
      ++I;
      ++ifline;
      pfree += lines_len;
    }
    *nline=static_cast<integer>(ifline);
  } // freeblock(const ::sff::FREE& free,

} // namespace fapidxx

/* ----- END OF helper.cc ----- */
