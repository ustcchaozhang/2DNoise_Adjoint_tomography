/*! \file func_pathname.cc
 * \brief functions to prepare and check data path (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id: $
 * \author Thomas Forbriger
 * \date 23/03/2014
 * 
 * functions to prepare and check data path (implementation)
 * 
 * Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 23/03/2014   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DL1_FUNC_PATHNAME_CC_VERSION \
  "DL1_FUNC_PATHNAME_CC   V1.0   "
#define DL1_FUNC_PATHNAME_CC_CVSID \
  "$Id: $"

#include <tfxx/stringfunc.h>
#include <tfxx/filestatus.h>
#include <tfxx/fs.h>
#include <sstream>
#include <sys/stat.h>
#include "functions.h"
#include "logger.h"

namespace dl1 {

  const char* const TPLyear="%Y";
  const char* const TPLmonth="%M";
  const char* const TPLday="%D";
  const char* const TPLtype="%T";

  /*======================================================================*/

  // replace a template string by a number of a given number of digits.
  std::string patsubstnum(const std::string& p,
                          const std::string& t,
                          const int& n, const int& d)
  {
    std::ostringstream oss;
    oss.width(d);
    oss.fill('0');
    oss << n;
    return(tfxx::string::patsubst(p, t, oss.str()));
  } // std::string patsubstnum(...)

  /*----------------------------------------------------------------------*/

  // check if date templates are present in pattern.
  bool datetemplatespresent(const std::string& p)
  {
    return((p.find(TPLyear)!=std::string::npos)
           && (p.find(TPLmonth)!=std::string::npos)
           && (p.find(TPLday)!=std::string::npos));
  } // bool datetemplatespresent(const std::string& p)

  /*----------------------------------------------------------------------*/

  // replace a templates in pattern by date values.
  std::string patsubstdate(const std::string& p,
                           const libtime::TAbsoluteTime& date)
  {
    return(patsubstnum(patsubstnum(patsubstnum(p, 
           TPLyear, date.year(), 4),
           TPLmonth, date.month(), 2),
           TPLday, date.day(), 2));
  } // std::string patsubstdate(...)

  /*----------------------------------------------------------------------*/

  // prepare path from pattern
  std::string mkpathname(const std::string& pattern,
                         const libtime::TAbsoluteTime& date,
                         const std::string& type,
                         const bool& active)
  {
    // check for file type pattern template
    if (pattern.find(TPLtype)==std::string::npos)
    {
      Logger(log_err) << "file type template " << TPLtype
        << " is missing in pattern " << pattern;
      DL1_abort("file type template is missing in path name pattern");
    }

    // check if date template is present if not active
    if ((!active) && (!datetemplatespresent(pattern)))
    {
      Logger(log_err) << "date templates " 
        << TPLyear << ", "
        << TPLmonth << ", and "
        << TPLday << ", "
        << " are mandatory";
      Logger(log_err) << "mandatory date templates are "
        << "missing in pattern " << pattern;
      DL1_abort("mandatory date templates are missing in path name pattern");
    }

    // substitute templates in pattern
    std::string pathname=tfxx::string::patsubst(pattern, TPLtype, type);
    pathname=patsubstdate(pathname, date);

    // create base directories if not present
    std::string dirname=tfxx::fs::dirname(pathname);
    if (!tfxx::file::writable(dirname.c_str()))
    { 
      try {
        tfxx::fs::mkdirp(dirname);
      }
      catch (tfxx::error::Exception& e) {
        Logger(log_err) << "cannot create directory " << dirname;
        DL1_abort("cannot create directory for output file");
      }
    }

    // if path name must be unique, find a unique one
    if (!active)
    {
      std::string filename=pathname;
      int i=0;
      while (tfxx::file::exists(filename.c_str()))
      {
        ++i;
        std::ostringstream oss;
        oss.width(3);
        oss.fill('0');
        oss << i;
        filename=pathname+"_"+oss.str();
        if (i==999)
        {
          Logger(log_err) << "cannot find new unused path name "
            << "based on " << pathname;
          DL1_abort("cannot find unique path name for output");
        }
      }
      pathname=filename;
    }

    // is the result of this function useable for writing?
    if (!tfxx::file::creatable(pathname.c_str()))
    {
      Logger(log_err) << "file path " << pathname 
        << " is not useable for output";
      DL1_abort("output file path is not useable");
    }

    return(pathname);
  } // std::string mkpathname(...)

} // namespace dl1

/* ----- END OF func_pathname.cc ----- */
