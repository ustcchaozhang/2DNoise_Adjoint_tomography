/*! \file error.h
 * \brief exception class declaration for libdatrwxx (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * exception class declaration for libdatrwxx (prototypes)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 30/03/2004   V1.0   Thomas Forbriger (thof)
 *  - 28/04/2006   V1.1   provide explicit virtual destructor
 *  - 07/07/2006   V1.2   provide non-fatal behaviour
 *  - 06/09/2011   V1.3   introduced report_deprecated
 *  - 22/07/2014   V1.4   thof: report comments prior to throwing exception
 *                              provide warning macro
 *  - 05/07/2016   V1.5   thof: operate with string containers rather than
 *                              with pointer to character arrays
 *  - 07/07/2016   V1.6   thof: move class exception to separate header
 *  - 08/07/2016   V1.7   thof: 
 *                        - move report functions to separate compilation unit
 *                        - create multiline messages
 *                        - remove specific multiline macros
 *  - 11/07/2016   V1.8   thof:
 *                        - support multi-line abort message
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_ERROR_H_VERSION

#define DATRW_ERROR_H_VERSION \
  "DATRW_ERROR_H   V1.8"

#include <iostream>
#include <sstream>
#include <datrwxx/report.h>
#include <datrwxx/exception.h>

/*! \defgroup group_error Internal utility: Error handling module
 */

/*======================================================================*/
//
// preprocessor macros
// ===================

/*! \brief Check an assertion and report by throwing an exception.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message (may use output operators;
 *          possibly containing newline characters)
 * \param E exception class to throw
 */
#define DATRW_Xassert(C,M,E) \
  if (!(C)) { \
    { \
      std::ostringstream oss; \
      oss << M; \
      throw( E ( oss.str() , __FILE__, __LINE__, #C ));  \
    } \
  }

/*! \brief Check an assertion and report by throwing an exception.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message (may use output operators;
 *          possibly containing newline characters)
 */
#define DATRW_assert(C,M) DATRW_Xassert( C , M , datrw::Exception )

/*! \brief Abort and give a message.
 *
 * \ingroup group_error
 * \param M message (may use output operators;
 *          possibly containing newline characters)
 * \param E exception class to throw
 */
#define DATRW_abort(M) \
 { \
   std::ostringstream oss; \
   oss << M; \
   throw( datrw::Exception ( oss.str() , __FILE__, __LINE__ )); \
 }

#define DATRW_illegal DATRW_abort("Illegal call within the library!\n" \
   "This must be considered a bug. Please report the issue at\n" \
   "https://git.scc.kit.edu/Seitosh/Seitosh")


/*! \brief Check an assertion and report only.
 *
 * \ingroup group_error
 * \param C assert condition
 * \param M message (may use output operators;
 *          possibly containing newline characters)
 */
#define DATRW_report_assert(C,M) \
  if (!(C)) { \
    { \
      std::ostringstream oss; \
      oss << M; \
      datrw::util::report_violation(datrw::util::Fnonfatal, \
                                    oss.str(), __FILE__, __LINE__, #C); \
    } \
  }

/*! \brief Macro to distinguish between fatal and non fatal assertions.
 *
 * \ingroup group_error
 * \param F true for non fatal behaviour
 * \param C assert condition
 * \param M message (may use output operators;
 *          possibly containing newline characters)
 */
#define DATRW_nonfatal_assert(F,C,M) \
  if (F) { DATRW_report_assert(C,M) } else { DATRW_assert(C,M) }

/*! \brief Report a warning.
 *
 * \ingroup group_error
 * \param N name of function 
 * \param M message (may use output operators;
 *          possibly containing newline characters)
 */
#define DATRW_warning(N,M) \
  { \
    std::ostringstream oss; \
    oss << "in function \"" << N << "\":\n" << M; \
    datrw::util::report_violation(datrw::util::Fwarning, \
                                  oss.str(), __FILE__, __LINE__, ""); \
  } 

#endif // DATRW_ERROR_H_VERSION (includeguard)

/* ----- END OF error.h ----- */
