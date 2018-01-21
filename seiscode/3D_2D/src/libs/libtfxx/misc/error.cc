/*! \file error.cc
 * \brief exceptions and error handling macros (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/11/2002
 * 
 * exceptions and error handling macros (implementation)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 27/11/2002   V1.0   Thomas Forbriger
 *  - 10/06/2015   V1.1   add report violation and report deprecated functions
 *                        copied from libdatrwxx
 * 
 * ============================================================================
 */
#define TF_ERROR_CC_VERSION \
  "TF_ERROR_CC   V1.1"

#include <iostream>
#include <tfxx/error.h>

using std::cerr;
using std::endl;

namespace tfxx {
namespace error {

  //! initialize and instantiate
  bool Exception::Mreport_on_construct=true;
  std::stack<bool> Exception::Mprevious_report_state;

  //! construct from nothing
  Exception::Exception(): 
    Mmessage(NULL), Mfile(NULL), Mline(0), Mcondition(NULL)
    { if (this->report_on_construct_is_true()) { base_report(); } }

  //! construct with message
  Exception::Exception(const char* message):
    Mmessage(message), Mfile(NULL), Mline(0), Mcondition(NULL)
    { if (this->report_on_construct_is_true()) { base_report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* condition):
    Mmessage(message), Mfile(NULL), Mline(0), Mcondition(condition)
    { if (this->report_on_construct_is_true()) { base_report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* file,
                       const int& line):
    Mmessage(message), Mfile(file), Mline(line), Mcondition(NULL)
    { if (this->report_on_construct_is_true()) { base_report(); } }

  //! construct with message and file info and condition
  Exception::Exception(const char* message,
                       const char* file,
                       const int& line,
                       const char* condition):
    Mmessage(message), Mfile(file), Mline(line), Mcondition(condition)
    { if (this->report_on_construct_is_true()) { base_report(); } }
      
  //! switch on
  void Exception::report_on_construct() 
  {
    Mprevious_report_state.push(Mreport_on_construct);
    Mreport_on_construct=true;
  }

  //! switch off
  void Exception::dont_report_on_construct()
  {
    Mprevious_report_state.push(Mreport_on_construct);
    Mreport_on_construct=false;
  }

  //! restore
  void Exception::restore_report_state()
  {
    if (!Mprevious_report_state.empty())
    {
      Mreport_on_construct=Mprevious_report_state.top();
      Mprevious_report_state.pop();
    }
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

  //! report violation of assertion
  void report_violation(const char* message, 
                        const char* file,
                        const int& line,
                        const char* condition)
  {
    std::cerr << std::endl;
    std::cerr << "VIOLATION of condition: " << condition << std::endl; 
    std::cerr << "* in " << file  << " at line " << line << std::endl; 
    std::cerr << "* message: " << message << std::endl; 
  }

  /*----------------------------------------------------------------------*/

  //! report deprecated function
  void report_deprecated(const char* function,
                         const char* reason)
  {
    std::cerr << "WARNING: program uses deprecated function in libtfxx\n"
      << "* " << function << std::endl; 
    std::cerr << "* This function should no longer be used because\n"
      << "* " << reason << std::endl;
    std::cerr << "* Please place a ticket at "
      "http://git.scc.kit.edu:Seitosh/Seitosh"
      << std::endl;
  }

} // namespace error
} // namespace tfxx

/* ----- END OF error.cc ----- */
