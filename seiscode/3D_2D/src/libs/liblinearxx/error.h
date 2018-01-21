/*! \file error.h
 * \brief Exception class for this library (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/02/2004
 * 
 * Exception class for this library (prototypes)
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
 *  - 20/11/2007   V1.1   provide virtual destructors
 *  - 18/03/2011   V1.2   avoid libtfxx header file misc.h by presenting
 *                        a debug output macro here
 * 
 * ============================================================================
 */

// include guard
#ifndef LINEAR_ERROR_H_VERSION

#define LINEAR_ERROR_H_VERSION \
  "LINEAR_ERROR_H   V1.2"

namespace linear {

/*! \defgroup group_error Error handling module
 */

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
   * \sa AFF_Xassert
   * \sa AFF_assert
   * \sa AFF_abort
   */
  class Exception 
  {
    public:
      //! Creates exception with no explaning comments
      Exception();
      //! Needs a virtual destructor
      virtual ~Exception() { }
      //! Creates an exception with an explanation message
      Exception(const char* message);
      //! Creates an exception with message and failed assertion
      Exception(const char* message, 
                const char* condition);
      //! Create with message, failed assertion, and code position
      Exception(const char* message, 
                const char* file,
                const int& line,
                const char* condition);
      //! Create with message and code position
      Exception(const char* message, 
                const char* file,
                const int& line);
      //! Screen report
      virtual void report() const;
      //! Issue a screen report on construction of exception
      static void report_on_construct();
      //! Issue NO screen report on construction of exception
      static void dont_report_on_construct();
    protected:
      //! Screen report
      void base_report() const;
    private:
      //! Shall we print to cerr at construction time?
      static bool Mreport_on_construct;
      //! pointer to message string
      const char* Mmessage;
      //! pointer to file name string
      const char* Mfile;
      //! pointer to line number in source file
      const int& Mline;
      //! pointer to assertion condition text string
      const char* Mcondition;
  }; // class Exception

  /*----------------------------------------------------------------------*/

  /*! \brief Exception thrown in case of LAPACK return value error
   *
   * \ingroup group_error
   * \sa aff::Exception
   */
  class LapackException:
    public Exception
  {
    public:
      //! take number of requested elements and their size
      LapackException(const char* message, 
                const char* subroutine,
                const char* file,
                const int& line,
                const char* condition,
                const int& value);
      //! Needs a virtual destructor
      virtual ~LapackException() { }
      //! Screen report
      virtual void report() const;
      //! return INFO value
      int value() const { return(Mvalue); }
      //! return name of LAPACK subroutine 
      const char* subroutine() const { return(Msubroutine); }
    private:
      //! members to remember
      int Mvalue;
      const char* Msubroutine;
      const char* Mmeaning;
  }; // class LapackException

} // namespace linear

/*======================================================================*/
//
// preprocessor macros
// ===================

/*! \brief Check an assertion and report by throwing an exception.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 * \param E exception class to throw
 */
#define LINEAR_Xassert(C,M,E) \
  if (!(C)) { throw( E ( M , __FILE__, __LINE__, #C )); }

/*! \brief Check an assertion for a LAPACK return value and
 *         report by throwing an exception.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param N name of subroutine (of type char*)
 * \param M message of type char*
 * \param V LAPACK return value
 */
#define LINEAR_LAPACK_failure(C,N,M,V) \
  if ((C)) { throw( linear::LapackException ( M , N , __FILE__, \
                                              __LINE__, #C, V )); }

/*! \brief Check an assertion and report by throwing an exception.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 */
#define LINEAR_assert(C,M) LINEAR_Xassert( C , M , linear::Exception )

/*! \brief Abort and give a message.
 *
 * \ingroup group_error
 * \param M message of type char*
 */
#define LINEAR_abort(M) \
  throw( linear::Exception ( M , __FILE__, __LINE__ )) 

/*! \brief produce debug output
 * \ingroup group_error
 *
 * Code which uses this macro has to include <iostream>
 *
 * \param C output will be generated if C == true
 * \param N name of function 
 * \param M message to print
 */
#define LINEAR_debug(C,N,M) \
  if (C) { \
    std::cerr << "DEBUG (" << N << ", " \
      << __FILE__ << " line #" << __LINE__ << "):" << std::endl \
      << "      " << M << std::endl; \
    std::cerr.flush(); \
  }

#endif // LINEAR_ERROR_H_VERSION (includeguard)

/* ----- END OF error.h ----- */
