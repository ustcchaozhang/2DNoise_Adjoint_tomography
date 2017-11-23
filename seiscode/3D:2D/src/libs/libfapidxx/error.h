/*! \file error.h
 * \brief exceptions and error handling macros (prototypes)
 * 
 * \ingroup error_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/11/2010
 * 
 * exceptions and error handling macros (prototypes)
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
 *                        copied from libtfxx.a
 * 
 * ============================================================================
 */

// include guard
#ifndef FAPIDXX_ERROR_H_VERSION

#define FAPIDXX_ERROR_H_VERSION \
  "FAPIDXX_ERROR_H   V1.0"

namespace fapidxx {

/*! \defgroup group_error Error handling module
 */

/*! \brief Interface provided through error.h
 * \defgroup error_h Interface provided through error.h
 * \ingroup group_error
 */

/*! \brief Error handling
 *
 * \ingroup group_error, error_h
 * You may find the exception base class here.
 */
namespace error {

  /*! \brief Base class for exceptions
   *
   * This is an exception base class. It holds some information about the
   * reason for throwing the exception. The information is printed to cerr
   * through function report(). This function may be overloaded by a derived
   * type. But its functionality is still accessible through base_report().
   *
   * The standard behaviour is to print ou the message during object
   * initialization. If you don't like this, call dont_report_on_construct().
   *
   * \ingroup group_error, error_h
   * \sa FAPIDXX_Xassert
   */
  class Exception 
  {
    public:
      //! Creates exception with no explaning comments
      Exception();
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
      //! provide explicit virtual destructor
      virtual ~Exception() { }
      //! Screen report
      virtual void report() const;
      //! Issue a screen report on construction of exception
      static void report_on_construct();
      //! Issue NO screen report on construction of exception
      static void dont_report_on_construct();
    private:
      //! Screen report
      void base_report() const;
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
  }; // class exception

  class FUException: public Exception {
    public:
      //! Create with message, failed assertion, and code position
      FUException(const int& unit,
                  const char* message, 
                  const char* file,
                  const int& line,
                  const char* condition);
      //! provide explicit virtual destructor
      virtual ~FUException() { }
      //! Screen report
      virtual void report() const;
    private:
      //! file unit
      int Munit;
  }; // class FUException

} // namespace error

} // namespace fapidxx

/*======================================================================*/
//
// preprocessor macros
// ===================

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error, error_h
 * \param C assert condition
 * \param M message of type char*
 * \param E exception class to throw
 */
#define FAPIDXX_Xassert(C,M,E) \
  if (!(C)) { throw( E ( M , __FILE__, __LINE__, #C )); }

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error, error_h
 * \param C assert condition
 * \param M message of type char*
 */
#define FAPIDXX_assert(C,M) FAPIDXX_Xassert( C , M , fapidxx::error::Exception )

/*! \brief Abort and give a message
 *
 * \ingroup group_error, error_h
 * \param M message of type char*
 * \param E exception class to throw
 */
#define FAPIDXX_abort(M) \
  throw( fapidxx::error::Exception ( M , __FILE__, __LINE__ )) 

#define FAPIDXX_illegal FAPIDXX_abort("illegal call!")

/*! \brief Check an assertion and report only.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 * \param V any values that should be output (comment)
 *          a sequence of values and output operators
 */
#define FAPIDXX_report_assert(C,M,V) \
  if (!(C)) { \
    fapidxx::report_violation(M, __FILE__, __LINE__, #C); \
    std::cerr << "* comment: " << V << std::endl; \
    std::cerr << std::endl; \
    std::cerr.flush(); \
  }

/*! \brief Macro to distinguish between fatal and non fatal assertions.
 *
 * \ingroup group_error
 * \param F true for non fatal behaviour
 * \param C assert condition
 * \param M message of type char*
 * \param V any values that should be output (comment)
 *          a sequence of values and output operators
 */
#define FAPIDXX_nonfatal_assert(F,C,M,V) \
  if (F) { FAPIDXX_report_assert(C,M,V) } else { FAPIDXX_assert(C,M) }

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error, error_h
 * \param U file unit
 * \param C assert condition
 * \param M message of type char*
 */
#define FAPIDXX_fuassert(C,U,M) \
  if (!(C)) { throw( fapidxx::error::FUException ( U , M , __FILE__, __LINE__, #C )); }

#endif // FAPIDXX_ERROR_H_VERSION (includeguard)

/* ----- END OF error.h ----- */
