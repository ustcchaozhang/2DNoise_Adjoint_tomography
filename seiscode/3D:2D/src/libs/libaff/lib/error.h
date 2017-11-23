/*! \file error.h
 * \brief exceptions and error handling macros (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/11/2002
 * 
 * exceptions and error handling macros (prototypes)
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
 * \sa aff::Exception
 * \sa \ref group_error
 *
 * \note
 * This file is automatically included through array.h or binarray.h
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 10/12/2002   V1.0   copied from libtfxx
 *  - 16/12/2002   V1.1   (thof)
 *                        - now contains AllocException too
 *  - 28/04/2006   V1.2   (thof)
 *                        - classes with virtual functions require
 *                          the explicit definition of a virtual destructor
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_ERROR_H_VERSION

#define AFF_ERROR_H_VERSION \
  "AFF_ERROR_H   V1.2"

#include<aff/lib/types.h>

namespace aff {

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

  /*! \brief Exception thrown in case of allocation error
   *
   * \ingroup group_error
   * \sa aff::Exception
   */
  class AllocException:
    public Exception
  {
    public:
      //! take number of requested elements and their size
      AllocException(const Tsize& n, const Tsize& size);
      //! provide virtual destructor
      virtual ~AllocException() { }
      //! Screen report
      virtual void report() const;
    private:
      //! members to remember
      Tsize Mn, Msize;
  }; // class AllocException

} // namespace aff

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
#define AFF_Xassert(C,M,E) \
  if (!(C)) { throw( E ( M , __FILE__, __LINE__, #C )); }

/*! \brief Check an assertion and report by throwing an exception
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message of type char*
 */
#define AFF_assert(C,M) AFF_Xassert( C , M , aff::Exception )

/*! \brief Abort and give a message
 *
 * \ingroup group_error
 * \param M message of type char*
 * \param E exception class to throw
 */
#define AFF_abort(M) \
  throw( aff::Exception ( M , __FILE__, __LINE__ )) 

#endif // AFF_ERROR_H_VERSION (includeguard)

/* ----- END OF error.h ----- */
