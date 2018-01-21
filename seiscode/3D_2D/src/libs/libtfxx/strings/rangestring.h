/*! \file rangestring.h
 * \brief read ranges from strings (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/06/2005
 * 
 * read ranges from strings (prototypes)
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
 *  - 11/11/2009   V1.1   changed use of split function
 *  - 24/04/2013   V1.2   implement step size like in tf_listselect.f
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_RANGESTRING_H_VERSION

#define TF_RANGESTRING_H_VERSION \
  "TF_RANGESTRING_H   V1.2"

#include<tfxx/range.h>
#include<tfxx/rangelist.h>
#include<tfxx/stringfunc.h>
#include<list>
#include<vector>
#include<sstream>

namespace tfxx {

  namespace string {

    /*! read range from string 
     *
     * The string may contain a single number or a range in a format like
     * "5-19". Open ranges (like "3-" or "-18") as well as negative values are
     * not supported.
     */ 
    template<class T>
      tfxx::Range<T> range(const std::string& s)
      {
        typedef tfxx::Range<T> Trange;
        typename Trange::Tvalue v1, v2;
        std::list<std::string> slist;
        tfxx::string::gen_split(slist, s, "-", true);
        std::list<std::string>::const_iterator i=slist.begin();
        std::istringstream iss(*i);
        iss >> v1;
        if (slist.size()>1)
        { 
          ++i; 
          iss.str(*i); 
          // clear status after reading first number
          iss.clear();
          iss >> v2; 
        }
        else
        { v2=v1; }
        Trange retval(v1,v2);
        return retval;
      } // tfxx::Range<T> range(const std::string& s)

    /*----------------------------------------------------------------------*/

    /*! read rangelist from string 
     *
     * Reads a comma separated list of range strings an passes tham to the
     * range reading function to build a list of ranges.
     *
     * The input lists in the string are ranges separated by comma:
     * 4-7,9,13-26
     *
     * A step size can be specified using the '+' symbol.
     * '5+3-12' means: take every third staring at 5 and ending at 12
     */
    template<class T>
      tfxx::RangeList<T> rangelist(const std::string& s)
      {
        typedef tfxx::Range<T> Trange;
        typedef tfxx::RangeList<T> Tlist;
        Tlist retval;
        std::list<std::string> slist;
        tfxx::string::gen_split(slist, s, ",", true);
        std::list<std::string>::const_iterator i=slist.begin();
        while (i!=slist.end())
        { 
          std::vector<std::string> svec;
          tfxx::string::gen_split(svec, *i, "+", true);
          if (svec.size()>1)
          {
            std::vector<std::string> svec2;
            tfxx::string::gen_split(svec2, svec[1], "-", true);
            TFXX_assert(svec2.size()>1, "range not properly defined");
            typename Trange::Tvalue start(0), step(0), end(0);
            { std::istringstream iss(svec[0]); iss >> start; }
            { std::istringstream iss(svec2[0]); iss >> step; }
            { std::istringstream iss(svec2[1]); iss >> end; }
            /*
            std::cerr << "start=" << svec[0] 
              << " step=" << svec2[0] 
              << " end=" << svec2[1] << std::endl;
            std::cerr << "start=" << start 
              << " step=" << step 
              << " end=" << end << std::endl;
              */
            TFXX_assert(step>0, "range step size must be positive");
            for (typename Trange::Tvalue j=start; j<=end; j=j+step)
            {
               retval.append(Trange(j,j));
            }
          }
          else
          {
            // no '+' symbol, go ahead with simple range
            retval.append(range<typename Tlist::Tvalue>(*i)); 
          }
          ++i; 
        }
        return retval;
      } // tfxx::RangeList<T> rangelist(const std::string& s)

  } // namespace string

} // namespace tfxx

#endif // TF_RANGESTRING_H_VERSION (includeguard)

/* ----- END OF rangestring.h ----- */
