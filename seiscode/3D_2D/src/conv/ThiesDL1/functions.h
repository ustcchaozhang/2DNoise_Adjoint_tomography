/*! \file functions.h
 * \brief some functions to be used together with the DL1 data logger (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/03/2014
 * 
 * some functions to be used together with the DL1 data logger (prototypes)
 * 
 * Copyright (c) 2008, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 * This file covers prototypes for functions in
 * functions.cc, func_pathname.cc, and func_writedata.cc
 * 
 * REVISIONS and CHANGES 
 *  - 27/11/2008   V1.0   Thomas Forbriger (thof)
 *  - 24/03/2014 thof:    
 *                        - avoid tsioxx modules entirely
 *                        - use data output based in path patterns
 *                        - provide libdatrwxx output file types
 * 
 * ============================================================================
 */

// include guard
#ifndef DL_FUNCTIONS_H_VERSION

#define DL_FUNCTIONS_H_VERSION \
  "DL_FUNCTIONS_H   2014-03-24"
#define DL_FUNCTIONS_H_CVSID \
  "$Id$"

#include<list>
#include<string>
#include<libtime++.h>
#include "thiesdl1.h"
#include "record.h"

/*! \defgroup group_functions Functions for Thies DL1 access
 */
/** @{ */

namespace dl1 {

  /*! \defgroup group_func_constants Typedefs and constants
   *  */
  /** @{ */

  typedef std::list<std::string> Tlistofstring;

  // standard timeout values (seconds)
  extern const int normaltimeout;
  extern const int longtimeout;

  //! duration of one day
  extern const libtime::TRelativeTime oneday;
  //! duration of two minutes
  extern const libtime::TRelativeTime twominutes;

  //! number of retrys
  extern const int cmaxretries;

  //! CVS IDs
  extern const char* const CVSIDS[];

  /*! \defgroup group_func_const_templates Templates in path patterns
   *
   * Template patterns are stored in character constants to ensure consistency
   * throughout the program. 
   * Values are defined in func_pathname.cc
   */
  /** @{ */
  extern const char* const TPLyear;
  extern const char* const TPLmonth;
  extern const char* const TPLday;
  extern const char* const TPLtype;
  /** @} */ // group_func_const_templates

  /** @} */ // group_func_constants

  /*======================================================================*/
  // helper

  /*! \defgroup group_func_helper Support functions (internal helpers)
   */
  /** @{ */

  //! return list of strings
  Tlistofstring listofstring(const std::ostringstream& oss, 
                             const std::string& eol=DL1::dumpEOL);

  //! extract day part of date
  libtime::TAbsoluteTime dateonly(const libtime::TAbsoluteTime& date);

  /*! \brief replace a template string by a number of a given number of
   * digits.
   *
   * \param p pattern containing templates
   * \param t template to be replaced
   * \param n number to replace template
   * \param d number of digits inclusing heading zeroes
   * \return pattern with template replaced
   */
   std::string patsubstnum(const std::string& p,
                           const std::string& t,
                           const int& n, const int& d);

  /*! \brief check if date templates are present in pattern.
   *
   * \param p pattern containing templates
   * \return true id date patterns are present
   */
   bool datetemplatespresent(const std::string& p);

  /*! \brief replace a templates in pattern by date values.
   *
   * \param p pattern containing templates
   * \param d date to be used as replacement
   * \return pattern with template replaced
   */
   std::string patsubstdate(const std::string& p,
                            const libtime::TAbsoluteTime& date);

  /** @} */ // group_func_helper

  /*======================================================================*/
  // DL1 access

  /*! \defgroup group_func_access Thies DL1 access functions
   *
   * Access functions are used by the \ref DL1logger to control the Thies DL1
   * data logger and to fetch data from it.
   * Each of the functions takes a reference to an instance of dl1::DL1 on
   * which it then operates.
   */
  /** @{ */

  //! initialize logger time and date
  void initializeclock(DL1& dl1);

  //! return current status of logger
  Tlistofstring status(DL1& dl1);

  /*! \brief read record after command has been sent previously.
   *
   * Fill a dl1::Record with data by reading lines from the serial interface
   * line by line, converting the contents and appending then to the record by
   * calling dl1::Record::put().
   */
  void fillrecordfromlogger(DL1& dl1, Record& record);

  /*! \brief read record for specific date.
   *
   * Fill a dl1::Record with data from exactly one day.
   * This function is used to fetch data for completed days which should go to
   * the permanent data base.
   * It makes use of dl1::fillrecordfromlogger().
   */
  Record readrecordfordate(DL1& dl1, const libtime::TAbsoluteTime& date);

  /*! \brief read record since specific date.
   *
   * Fill a dl1::Record with data beginning at a specified time up to the
   * current time.
   * This function is used to fetch recent data for the current day.
   * It makes use of dl1::fillrecordfromlogger().
   */
  Record readrecordsincedate(DL1& dl1, const libtime::TAbsoluteTime& date);

  /*! \brief process one full day.
   *
   * Process data from exactly on day, by fetching from the Thies DL1 logger
   * and writing to appropriate output files.
   *
   * \param dl1 DL1 instance to poll from
   * \param datapath pattern to use for data path
   * \param datatypes list of libdatrwxx file types to use for output
   * \param date completed day for which data shall be polled
   *
   * \sa dl1::dumptofile(), dl1::writedata(), dl1::readrecordfordate()
   */
  void processday(DL1& dl1,
                  const std::string& datapath,
                  const std::string& datatypes,
                  const libtime::TAbsoluteTime& date);

  /** @} */ // group_func_access

  /*======================================================================*/
  // data output

  /*! \defgroup group_func_output Data output functions
   */
  /** @{ */

  //! dump raw ASCII table to file
  void dumptofile(const std::string& filename, const Record& record,
                  const Tlistofstring& info);

  /*! \brief write data to file.
   *
   * \param filename file name to write to
   * \param datatype libdatrwxx data type
   * \param record record to be written
   * \param info info lines for file header
   */
  void writedata(const std::string& filename, 
                 const std::string& datatype,
                 Record& record,
                 const Tlistofstring& info);

  /*! \brief prepare file path from pattern.
   * \param pattern pattern defining the data path
   * \param date date for which the path should be created
   * \param type file type for which the path name should be created
   * \param active true, if data path for uncompleted days should be created
   * \return complete path name to be used for data file
   *
   * Steps taken:
   *  -# check if file type template is present in pattern
   *  -# if not active: check if date template is present in pattern
   *  -# replace templates in pattern
   *  -# create base directories if not yet present
   *  -# if not active: check if file is not yet present and try to find
   *     unique name by adding numbers
   *  -# return path name
   */
  std::string mkpathname(const std::string& pattern,
                         const libtime::TAbsoluteTime& date,
                         const std::string& type,
                         const bool& active);

  /** @} */ // group_func_output

/**@}*/ // group_functions

} // namespace dl1

#endif // DL_FUNCTIONS_H_VERSION (includeguard)

/* ----- END OF functions.h ----- */
