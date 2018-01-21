/*! \file logger.cc
 * \brief message loggin module (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 03/12/2008
 * 
 * message loggin module (implementation)
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
 *  - 03/12/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DL_LOGGER_CC_VERSION \
  "DL_LOGGER_CC   V1.0   "
#define DL_LOGGER_CC_CVSID \
  "$Id$"

#include<syslog.h>
#include<iostream>
#include "logger.h"

namespace dl1 {

  const char* Logger::ident="NSP";
  void Logger::setident(const char* ident) { Logger::ident=ident; }
  bool Logger::verbose=false;
  void Logger::beverbose(const bool& flag) { Logger::verbose=flag; }
  bool Logger::verboseerror=false;
  void Logger::beverboseerr(const bool& flag) { Logger::verboseerror=flag; }

  /*----------------------------------------------------------------------*/

  Logger::Logger(const enum loglevel& level): Mlevel(level) 
  { 
    Moss.clear();
  } // Logger(const loglevel& lev): Mlevel(level)

  /*----------------------------------------------------------------------*/

  Logger::~Logger()
  {
    int priority;
    switch(Mlevel) {
      case log_emerg:
        priority=LOG_EMERG;
        break;
      case log_alert:
        priority=LOG_ALERT;
        break;
      case log_crit:
        priority=LOG_CRIT;
        break;
      case log_err:
        priority=LOG_ERR;
        break;
      case log_warning:
        priority=LOG_WARNING;
        break;
      case log_notice:
        priority=LOG_NOTICE;
        break;
      case log_info:
        priority=LOG_INFO;
        break;
      case log_debug:
        priority=LOG_DEBUG;
        break;
      default:
        priority=LOG_INFO;
    }
    openlog(Logger::ident, (LOG_CONS | LOG_PID), LOG_USER);
    syslog(priority, "%s", Moss.str().c_str());
    closelog();
    if (Logger::verbose) 
    { 
      if (Logger::verboseerror)
      {
        std::cerr << Moss.str() << std::endl; 
      }
      else
      {
        std::cout << Moss.str() << std::endl; 
      }
    }
  } // Logger::~Logger()

  /*----------------------------------------------------------------------*/

  void loglistofstring(Logger& logger, const Tlistofstring& list)
  {
    Tlistofstring::const_iterator I=list.begin();
    Tlistofstring::const_iterator E=list.end();
    --E;
    while (I!=E) { Logger() << *I; ++I; }
    logger << *E;
  } // void loglistofstring(Logger& logger, const Tlistofstring& list)

  /*======================================================================*/

}  // namespace dl1  

/* ----- END OF logger.cc ----- */
