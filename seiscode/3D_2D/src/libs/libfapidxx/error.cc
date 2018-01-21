/*! \file error.cc
 * \brief exceptions and error handling macros (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/11/2010
 * 
 * exceptions and error handling macros (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 18/11/2010   V1.0   Thomas Forbriger
 *                        copied from libtfxx
 * 
 * ============================================================================
 */
#define FAPIDXX_ERROR_CC_VERSION \
  "FAPIDXX_ERROR_CC   V1.0"

#include <iostream>
#include <fapidxx/error.h>

using std::cerr;
using std::endl;

namespace fapidxx {
namespace error {

  //! initialize and instantiate
  bool Exception::Mreport_on_construct=true;

  //! construct from nothing
  Exception::Exception(): 
    Mmessage(NULL), Mfile(NULL), Mline(0), Mcondition(NULL)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message
  Exception::Exception(const char* message):
    Mmessage(message), Mfile(NULL), Mline(0), Mcondition(NULL)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* condition):
    Mmessage(message), Mfile(NULL), Mline(0), Mcondition(condition)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* file,
                       const int& line):
    Mmessage(message), Mfile(file), Mline(line), Mcondition(NULL)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info and condition
  Exception::Exception(const char* message,
                       const char* file,
                       const int& line,
                       const char* condition):
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
    std::cout.flush();
    cerr << "Exception report:" << endl;
    if (Mmessage==NULL)
    {
      cerr << "  No message" << endl;
    }
    else
    {
      cerr << "  message: " << Mmessage << endl;
    }
    if (Mfile!=NULL)
    {
      cerr << "  triggered in \"" << Mfile << "\" at line #" << Mline << endl;
    }
    if (Mcondition!=NULL)
    {
      cerr << "  by test condition:" << endl
        << "    \"" << Mcondition << "\"" << endl;
    }
    cerr.flush();
  }

  /*----------------------------------------------------------------------*/

  //! create FUException
  FUException::FUException(const int& unit,
                           const char* message, 
                           const char* file,
                           const int& line,
                           const char* condition):
    Exception(message, file, line, condition), Munit(unit) { }

  /*----------------------------------------------------------------------*/

  //! report
  void FUException::report() const
  {
    this->Exception::report();
    cerr << "  Selected file unit: " << Munit << endl;
    cerr.flush();
  }

} // namespace error
} // namespace fapidxx

/* ----- END OF error.cc ----- */
