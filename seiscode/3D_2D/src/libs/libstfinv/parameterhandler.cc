/*! \file parameterhandler.cc
 * \brief handle a parameter configuration string (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * handle a parameter configuration string (implementation)
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
 * 
 * REVISIONS and CHANGES 
 *  - 06/05/2011   V1.0   Thomas Forbriger
 *  - 18/01/2016   V1.1   rename function to secomtospace
 * 
 * ============================================================================
 */
#define STFINV_PARAMETERHANDLER_CC_VERSION \
  "STFINV_PARAMETERHANDLER_CC   V1.1"

#include <algorithm>
#include <stfinv/parameterhandler.h>

std::string stfinv::tools::clipstring(std::string& s, const std::string& delim)
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
} // std::string stfinv::tools::clipstring()

/*----------------------------------------------------------------------*/

stfinv::tools::Tparamap stfinv::tools::makeparamap(const std::string& p,
                                                   const std::string& delimiter,
                                                   const std::string& assign)
{
  std::string pstring=p;
  stfinv::tools::Tparamap retval;
  while (pstring.length()>0)
  {
    std::string para=stfinv::tools::clipstring(pstring, delimiter);
    std::string key=stfinv::tools::clipstring(para, assign);
    retval.insert(make_pair(key, para));
  }
  return retval;
} // stfinv::tools::Tparamap stfinv::tools::makeparamap()

/*----------------------------------------------------------------------*/

std::string stfinv::tools::secomtospace(std::string s)
{
  std::replace(s.begin(), s.end(), ',', ' ');
  std::replace(s.begin(), s.end(), ';', ' ');
  return(s);
} // std::string stfinv::tools::secomtospace(const std::string& s)

/*----------------------------------------------------------------------*/

// \brief remove leading and trailing whitespace
std::string stfinv::tools::trimws(std::string s)
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

/* ----- END OF parameterhandler.cc ----- */
