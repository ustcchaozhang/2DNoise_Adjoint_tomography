/*! \file stringfunc.cc
 * \brief string functions copied from former class library (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/06/2005
 * 
 * string functions copied from former class library (implementation)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 30/06/2005   V1.0   Thomas Forbriger
 *  - 25/11/2010   V1.1   implemented more efficient method to strip
 *                        whitespace
 * 
 * ============================================================================
 */
#define TF_STRINGFUNC_CC_VERSION \
  "TF_STRINGFUNC_CC   V1.1"

#include <tfxx/stringfunc.h>

#include<algorithm>
#include<iterator>
#include<functional>
#include<ctype.h>

namespace tfxx {

  namespace string {

    // remove leading and trailing whitespace
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

    /*----------------------------------------------------------------------*/

    // remove leading whitespace
    std::string trimws_begin(std::string s)
    {
      if (s.length()>0)
      {
        std::string::size_type ib=s.find_first_not_of(" ", 0);
        if (ib==std::string::npos)
        {
          s="";
        }
        else if (ib!=0)
        {
          std::string::size_type n=s.length()-ib;
          s=s.substr(ib,n); 
        }
      }
      return(s);
    } // std::string trimws_begin(std::string s)

    /*----------------------------------------------------------------------*/

    // remove trailing whitespace
    // This is not as straight forward as trimws_begin as I did not manage to
    // get string::erase compiled with reverse iteraters. On the other hand
    // the only appropriate method to start a search at the end of the string
    // is to use a reverse iterator. Doing reverse twice IS ugly as it means
    // copying the string twice. Maybe I'll write some better code some
    // time... 
    std::string trimws_end(std::string s)
    {
      if (s.length()>0)
      {
        std::string::size_type il=s.find_last_not_of(" \r", s.length());
        std::string::size_type n=il>=0 ? il+1 : 0;
        if (n!=s.length()) { s=s.substr(0,n); }
      }
      return(s);
    } // std::string trimws_end(std::string s)

    /*----------------------------------------------------------------------*/

    /*! strip substring
     *
     * \param c character to find in string
     * \param delim delimiter to look for
     * \return number of c characters in string
     */
    std::string strip_string(std::string& s, const char& delim)
    {
      std::string::size_type i=s.find(delim);
      std::string result;
      if ((i>=0) && (i<s.size())) {
        result=s.substr(0,i);
        s.erase(0,i+1);
      } else {
        result=s;
        s.erase();
      }
      return(result);
    }

    /*----------------------------------------------------------------------*/

    /*! count characters in string
     *
     * \param s string to look for c 
     * \param c character to find in string
     * \return number of c characters in string
     */
    int count_char(const std::string& s, const char& c)
    {
      int result=0;
      for (std::string::const_iterator i=s.begin(); i!=s.end(); i++)
      { if (*i == c) { result++; } }
      return(result);
    }

    /*----------------------------------------------------------------------*/

    /*! replace pattern by string
     *
     * \param s string that contains pattern
     * \param p pattern string
     * \param r replacement for pattern
     * \return string \p s with all instances of pattern \p p 
     *                replaced by \p r
     */
    std::string patsubst(const std::string& s,
                         const std::string& p,
                         const std::string& r)
    {
      std::string retval=s;
      std::string::size_type n=retval.find(p);
      while (n !=std::string::npos)
      {
        retval.replace(n,p.length(),r);
        n=retval.find(p,n);
      }
      return(retval);
    }

  } // namespace string

} // namespace tfxx
 
/* ----- END OF stringfunc.cc ----- */
