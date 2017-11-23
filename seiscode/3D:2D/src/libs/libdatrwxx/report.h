/*! \file report.h
 * \brief report errors and warnings (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/07/2016
 * 
 * report errors and warnings (prototypes)
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
 *  - 08/07/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_REPORT_H_VERSION

#define DATRW_REPORT_H_VERSION \
  "DATRW_REPORT_H   V1.0"

#include <string>

namespace datrw {

  namespace util {

    /*! \brief report categories
     * \ingroup group_error
     */
    enum Ereport {
      //! just a warning; processing can proceeed
      Fwarning,
      //! an error, but non-fatal; processing will proceed
      Fnonfatal,
      //! a fatal error; processing wil abort as a consequence
      Ffatal
    }; // enum Ereport

  /* ---------------------------------------------------------------------- */

    /*! \brief report violation of condition
     *
     * \ingroup group_error
     * \param message       message of type char*
     * \param file          name of source code file
     * \param line          source code line number
     * \param condition     assert condition
     * \param t             type of incident to be reported
     */
    void report_violation(const Ereport& t,
                          const std::string& message, 
                          const std::string& file,
                          const int& line,
                          const std::string& condition);

  /* ---------------------------------------------------------------------- */

    /*! \brief report deprecation of a function
     *
     * \ingroup group_error
     * \param function      name of deprecated function
     * \param reason        the reason for deprecating the function
     *                      should finish a sentence which started with
     *                      "because"
     */
    void report_deprecated(const std::string& function,
                           const std::string& reason);

  } // namespace util

} // namespace datrw

#endif // DATRW_REPORT_H_VERSION (includeguard)

/* ----- END OF report.h ----- */
