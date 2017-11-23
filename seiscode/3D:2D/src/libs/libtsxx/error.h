/*! \file error.h
 * \brief error handling for libtsxx (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/11/2016
 * 
 * error handling for libtsxx (prototypes)
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
 *  - 22/11/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TSXX_ERROR_H_VERSION

#define TSXX_ERROR_H_VERSION \
  "TSXX_ERROR_H   V1.0"

  /*----------------------------------------------------------------------*/
  // assertions

namespace ts {

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
     * \ingroup group_error
     * \sa TSXX_Xassert
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
        //! provide virtual destructor
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

  } // namespace error

} // namespace ts

/*======================================================================*/
//
// preprocessor macros
// ===================

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 * \param E exception class to throw
 */
#define TSXX_Xassert(C,M,E) \
  if (!(C)) { throw( E ( M , __FILE__, __LINE__, #C )); }

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 */
#define TSXX_assert(C,M) TSXX_Xassert( C , M , ts::error::Exception )

/*! \brief Abort and give a message
 *
 * \ingroup group_error
 * \param M message of type char*
 * \param E exception class to throw
 */
#define TSXX_abort(M) \
  throw( ts::error::Exception ( M , __FILE__, __LINE__ )) 

#endif // TSXX_ERROR_H_VERSION (includeguard)

/* ----- END OF error.h ----- */
