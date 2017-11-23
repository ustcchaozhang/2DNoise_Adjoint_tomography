/*! \file logger.h
 * \brief message loggin module (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 03/12/2008
 * 
 * message loggin module (prototypes)
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

// include guard
#ifndef DL_LOGGER_H_VERSION

#define DL_LOGGER_H_VERSION \
  "DL_LOGGER_H   V1.0   "
#define DL_LOGGER_H_CVSID \
  "$Id$"

#include<sstream>
#include<list>
#include<string>

namespace dl1 {

/*! \defgroup group_logger Logger: Class to write to system log and terminal
 */
/** @{ */

  typedef std::list<std::string> Tlistofstring;

  /*! \brief syslog logging levels
   */
  enum loglevel {
    log_emerg,
    log_alert,
    log_crit,
    log_err,
    log_warning,
    log_notice,
    log_info,
    log_debug,
  }; // enum loglevel

  class Logger;
  //! support function to log several lines from a list of strings
  void loglistofstring(Logger& logger, const Tlistofstring& list);

  /*! \brief Logs messages to the system's syslog.
   */
  class Logger {
    public:
      Logger(const enum loglevel& level=log_notice);
      ~Logger();
      //! set ident string for log message
      static void setident(const char* ident);
      //! send message to stdout or stderr too
      static void beverbose(const bool& flag);
      //! send message to stderr rather than stdout if in verbose mode
      static void beverboseerr(const bool& flag);
      //! pass output messages in a list of strings to the logger
      Logger& operator<<(const Tlistofstring& list)
      { 
        loglistofstring(*this, list);
        return(*this);
      }
      //! pass a value to the log message
      template<class T>
        Logger& operator<<(const T& t)
        { 
          Moss << t; 
          return(*this);
        }
    private:
      //! log level
      loglevel Mlevel;
      //! accumulate message here
      std::ostringstream Moss;
      //! indentifier
      static const char* ident;
      //! verbose mode
      static bool verbose;
      //! verbose mode
      static bool verboseerror;
  }; // class Logger

/** @} */

} // namespace dl1

#endif // DL_LOGGER_H_VERSION (includeguard)

/* ----- END OF logger.h ----- */
