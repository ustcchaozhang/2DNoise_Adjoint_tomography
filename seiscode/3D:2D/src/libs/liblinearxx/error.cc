/*! \file error.cc
 * \brief Exception class for this library (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/02/2004
 * 
 * Exception class for this library (implementation)
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 08/02/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define LINEAR_ERROR_CC_VERSION \
  "LINEAR_ERROR_CC   V1.0   "

#include <iostream>
#include <linearxx/error.h>

using std::cerr;
using std::endl;

namespace linear {

  //! initialize and instantiate
  bool Exception::Mreport_on_construct=true;

  //! construct from nothing
  Exception::Exception(): 
    Mmessage(0), Mfile(0), Mline(0), Mcondition(0)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message
  Exception::Exception(const char* message):
    Mmessage(message), Mfile(0), Mline(0), Mcondition(0)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* condition):
    Mmessage(message), Mfile(0), Mline(0), Mcondition(condition)
    { if (Mreport_on_construct) { report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* file,
                       const int& line):
    Mmessage(message), Mfile(file), Mline(line), Mcondition(0)
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
    cerr << "Exception report:" << endl;
    if (Mmessage==0)
    {
      cerr << "  No message" << endl;
    }
    else
    {
      cerr << "  message: " << Mmessage << endl;
    }
    if (Mfile!=0)
    {
      cerr << "  triggered in \"" << Mfile << "\" at line #" << Mline << endl;
    }
    if (Mcondition!=0)
    {
      cerr << "  by condition:" << endl
        << "    \"" << Mcondition << "\"" << endl;
    }
  }

  /*======================================================================*/
  // LapackException code 

  //! instantiate LapackException
  LapackException::LapackException(const char* message, 
                                   const char* subroutine,
                                   const char* file,
                                   const int& line,
                                   const char* condition,
                                   const int& value):
    Exception("ERROR: return value of LAPACK subroutine!",
              file,line,condition), Mvalue(value), Msubroutine(subroutine),
              Mmeaning(message) { }

  //! report AllocException
  void LapackException::report() const
  {
    base_report();
    std::cerr << "  The LAPACK subroutine \"" << Msubroutine 
      << "\" returned " << Mvalue << "." << std::endl;
    std::cerr << "  This means:" << std::endl;
    std::cerr << "  " << Mmeaning << std::endl;
  }


} // namespace linear

/* ----- END OF error.cc ----- */
