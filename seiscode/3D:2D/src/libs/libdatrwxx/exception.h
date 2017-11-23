/*! \file exception.h
 * \brief libdatrwxx exception class (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/07/2016
 * 
 * libdatrwxx exception class (prototypes)
 *
 * \ingroup group_error
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
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_EXCEPTION_H_VERSION

#define DATRW_EXCEPTION_H_VERSION \
  "DATRW_EXCEPTION_H   V1.0"

#include <string>

namespace datrw {

  /*! \brief Base class for exceptions
   *
   * This is an exception base class. It holds some information about the
   * reason for throwing the exception. The information is printed to cerr
   * through function report(). This function may be overloaded by a derived
   * type. But its functionality is still accessible through base_report().
   *
   * The standard behaviour is to print out the message during object
   * initialization. If you don't like this, call dont_report_on_construct().
   *
   * \ingroup group_error
   * \sa DATRW_Xassert
   * \sa DATRW_assert
   * \sa DATRW_abort
   */
  class Exception 
  {
    public:
      //! Creates exception with no explaning comments
      Exception();
      //! Creates an exception with an explanation message
      Exception(const std::string& message);
      //! Creates an exception with message and failed assertion
      Exception(const std::string& message, 
                const std::string& condition);
      //! Create with message, failed assertion, and code position
      Exception(const std::string& message, 
                const std::string& file,
                const int& line,
                const std::string& condition);
      //! Create with message and code position
      Exception(const std::string& message, 
                const std::string& file,
                const int& line);
      //! provide explicit virtual destructor
      virtual ~Exception() { }
      //! Screen report
      virtual void report() const;
      //! Issue a screen report on construction of exception
      static void report_on_construct();
      //! Issue NO screen report on construction of exception
      static void dont_report_on_construct();
      //! set report on construct flag
      static void report_on_construct_flag(const bool& flag)
      { Mreport_on_construct=flag; }
      //! return report on construct flag
      static bool report_on_construct_flag() { return(Mreport_on_construct); }
    protected:
      //! Screen report
      void base_report() const;
    private:
      //! Shall we print to cerr at construction time?
      static bool Mreport_on_construct;
      //! pointer to message string
      std::string Mmessage;
      //! pointer to file name string
      std::string Mfile;
      //! pointer to line number in source file
      const int& Mline;
      //! pointer to assertion condition text string
      std::string Mcondition;
  }; // class Exception

} // namespace datrw

#endif // DATRW_EXCEPTION_H_VERSION (includeguard)

/* ----- END OF exception.h ----- */
