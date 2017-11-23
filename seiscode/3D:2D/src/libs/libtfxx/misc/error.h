/*! \file error.h
 * \brief exceptions and error handling macros (prototypes)
 * 
 * \ingroup error_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/11/2002
 * 
 * exceptions and error handling macros (prototypes)
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
 *  - 28/04/2006   V1.1   provide explicit virtual destructor
 *  - 11/0/2007    V1.2   added a few macros
 *  - 10/06/2015   V1.3   add report violation and report deprecated functions
 *                        copied from libdatrwxx
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_ERROR_H_VERSION

#define TF_ERROR_H_VERSION \
  "TF_ERROR_H   V1.3"

#include<stack>

namespace tfxx {

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
   * \sa TFXX_Xassert
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
      //! Restore previous report state
      static void restore_report_state();
    protected:
      bool report_on_construct_is_true() const 
      { return Mreport_on_construct; }
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
      int Mline;
      //! pointer to assertion condition text string
      const char* Mcondition;
      //! a place to store previous report state
      static std::stack<bool> Mprevious_report_state;
  }; // class exception

  /*! \brief report violation of condition
   *
   * \ingroup group_error
   * \param message       message of type char*
   * \param file          name of source code file
   * \param line          source code line number
   * \param condition     assert condition
   */
  void report_violation(const char* message, 
                        const char* file,
                        const int& line,
                        const char* condition);

  /*! \brief report deprecation of a function
   *
   * \ingroup group_error
   * \param function      name of deprecated function
   * \param reason        the reason for deprecating the function
   *                      should finish a sentence which started with "because"
   */
  void report_deprecated(const char* function,
                         const char* reason);

} // namespace error

} // namespace tfxx

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
#define TFXX_Xassert(C,M,E) \
  if (!(C)) { throw( E ( M , __FILE__, __LINE__, #C )); }

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error, error_h
 * \param C assert condition
 * \param M message of type char*
 */
#define TFXX_assert(C,M) TFXX_Xassert( C , M , tfxx::error::Exception )

/*! \brief Abort and give a message
 *
 * \ingroup group_error, error_h
 * \param M message of type char*
 * \param E exception class to throw
 */
#define TFXX_abort(M) \
  throw( tfxx::error::Exception ( M , __FILE__, __LINE__ )) 

#define TFXX_illegal TFXX_abort("illegal call!")

/*! \brief Check an assertion and report only.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 * \param V any values that should be output (comment)
 *          a sequence of values and output operators
 */
#define TFXX_report_assert(C,M,V) \
  if (!(C)) { \
    tfxx::error::report_violation(M, __FILE__, __LINE__, #C); \
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
#define TFXX_nonfatal_assert(F,C,M,V) \
  if (F) { TFXX_report_assert(C,M,V) } else { TFXX_assert(C,M) }

#endif // TF_ERROR_H_VERSION (includeguard)

/* ----- END OF error.h ----- */
