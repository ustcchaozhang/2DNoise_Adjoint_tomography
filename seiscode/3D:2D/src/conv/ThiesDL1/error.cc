/*! \file error.cc
 * \brief error handling and exceptions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 25/11/2008
 * 
 * error handling and exceptions (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 25/11/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_ERROR_CC_VERSION \
  "TF_ERROR_CC   V1.0   "
#define TF_ERROR_CC_CVSID \
  "$Id$"

#include "error.h"
#include "logger.h"
#include <iostream>

using std::cerr;
using std::endl;

namespace dl1 {

  //! initialize and instantiate
  bool Exception::Mreport_on_construct=true;

  //! construct from nothing
  Exception::Exception(): 
    Mmessage(0), Mfile(0), Mline(0), Mcondition(0)
    { if (Mreport_on_construct) { base_report(); } }

  //! construct with message
  Exception::Exception(const char* message):
    Mmessage(message), Mfile(0), Mline(0), Mcondition(0)
    { if (Mreport_on_construct) { base_report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* condition):
    Mmessage(message), Mfile(0), Mline(0), Mcondition(condition)
    { if (Mreport_on_construct) { base_report(); } }

  //! construct with message and file info
  Exception::Exception(const char* message,
                       const char* file,
                       const int& line):
    Mmessage(message), Mfile(file), Mline(line), Mcondition(0)
    { if (Mreport_on_construct) { base_report(); } }

  //! construct with message and file info and condition
  Exception::Exception(const char* message,
                       const char* file,
                       const int& line,
                       const char* condition):
    Mmessage(message), Mfile(file), Mline(line), Mcondition(condition)
    { if (Mreport_on_construct) { base_report(); } }
      
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
    Logger::beverbose(true);
    Logger::beverboseerr(true);
    Logger(log_alert) << "Exception report:";
    if (Mmessage==0)
    {
      Logger(log_alert)  << "  No message";
    }
    else
    {
      Logger(log_alert)  << "  message: " << Mmessage;
    }
    if (Mfile!=0)
    {
      Logger(log_alert) << "  triggered in \"" << Mfile 
        << "\" at line #" << Mline;
    }
    if (Mcondition!=0)
    {
      Logger(log_alert) << "  by condition:"
        << "    \"" << Mcondition << "\"";
    }
  }

  /*----------------------------------------------------------------------*/

  void ExceptionTimeOut::report() const {
    this->Exception::report();
    this->my_report();
  } // void ExceptionTimeOut::report() const

  /*----------------------------------------------------------------------*/

  void ExceptionTimeOut::my_report() const {
    Logger(log_alert)  << "  time out after " << this->Mtimeout << " s";
  } // void ExceptionTimeOut::my_report() const

  /*----------------------------------------------------------------------*/

  //! report violation of assertion
  void report_violation(const char* message, 
                        const char* file,
                        const int& line,
                        const char* condition)
  {
    Logger::beverbose(true);
    Logger::beverboseerr(true);
    Logger(log_crit) << "VIOLATION of condition: " << condition;
    Logger(log_crit) << "* in " << file  << " at line " << line;
    Logger(log_crit) << "* message: " << message;
  }

} // namespace dl1

/* ----- END OF error.cc ----- */
