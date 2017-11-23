/*! \file exception.cc
 * \brief libdatrwxx exception class (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/07/2016
 * 
 * libdatrwxx exception class (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 07/07/2016   V1.0   Thomas Forbriger
 *  - 17/05/2017   V1.1   bug fix: string members must be initialized with
 *                        character sequence
 * 
 * ============================================================================
 */
#define DATRW_EXCEPTION_CC_VERSION \
  "DATRW_EXCEPTION_CC   V1.1"

#include <iostream>
#include <datrwxx/exception.h>
#include <datrwxx/aalibdatrwxx.h>
#include <datrwxx/util.h>

using std::cerr;
using std::endl;

namespace datrw {

  //! initialize and instantiate
  bool Exception::Mreport_on_construct=true;

  //! construct from nothing
  Exception::Exception(): 
    Mmessage(""), Mfile(""), Mline(0), Mcondition("")
    { if (Mreport_on_construct) { report(); } }

  //! construct with message
  Exception::Exception(const std::string& message):
    Mmessage(message), Mfile(""), Mline(0), Mcondition("")
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info
  Exception::Exception(const std::string& message,
                       const std::string& condition):
    Mmessage(message), Mfile(""), Mline(0), Mcondition(condition)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info
  Exception::Exception(const std::string& message,
                       const std::string& file,
                       const int& line):
    Mmessage(message), Mfile(file), Mline(line), Mcondition("")
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info and condition
  Exception::Exception(const std::string& message,
                       const std::string& file,
                       const int& line,
                       const std::string& condition):
    Mmessage(message), Mfile(file), Mline(line), Mcondition(condition)
    { if (Mreport_on_construct) { report(); } }
      
  //! switch on
  void Exception::report_on_construct() 
  {
  Mreport_on_construct=true;
  }

  //! switch off
  void Exception::dont_report_on_construct()
  {
  Mreport_on_construct=false;
  }

  //! report
  void Exception::report() const
  {
    base_report();
  }

  //! report
  void Exception::base_report() const
  {
    datrw::util::report_violation(datrw::util::Ffatal,
                                  Mmessage,
                                  Mfile,
                                  Mline,
                                  Mcondition);
  }

} // namespace datrw

/* ----- END OF exception.cc ----- */
