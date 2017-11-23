/*! \file parameterhandler.h
 * \brief handle a parameter configuration string (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * handle a parameter configuration string (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 06/05/2011   V1.0   Thomas Forbriger
 *  - 18/01/2016   V1.1   rename function to secomtospace
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_PARAMETERHANDLER_H_VERSION

#define STFINV_PARAMETERHANDLER_H_VERSION \
  "STFINV_PARAMETERHANDLER_H   V1.1"

#include <string>
#include <map>

namespace stfinv {

  /*! \brief Namespace for internal tools
   * \ingroup group_tools
   */
  namespace tools {

    /*! strip substring
     * \ingroup group_tools
     *
     * Strips off first substring up to given delimiter.
     * The string is passed as a reference and will be modified (i.e. the
     * stripped substring as well as the delimiter will be erased).
     *
     * \param s input string
     * \param delim delimiter to look for
     * \return first part of string up to delimiter
     */
    std::string clipstring(std::string& s, const std::string& delim=":");

    /*----------------------------------------------------------------------*/

    /*! \brief A map to store parameters.
     * \ingroup group_tools
     */
    typedef std::map<std::string,std::string> Tparamap;

    /*----------------------------------------------------------------------*/

    /*! \brief Create a parameter map from a parameter string
     * \ingroup group_tools
     *
     * \param p parameter string
     * \param delimiter delimiter which separates two parameters
     * \param assign symbol seprating key and value
     * \return a multimap created from the parameter string
     */
    Tparamap makeparamap(const std::string& p,
                         const std::string& delimiter=":",
                         const std::string& assign="=");

    /*----------------------------------------------------------------------*/

    /*! replace commas and semicolons by whitespace
     * \ingroup group_tools
     *
     * \param s input string
     * \return input string with all commas replaced by whitespace and
     *         all semicolons replaced by whitespace
     */
    std::string secomtospace(std::string s);

    /*----------------------------------------------------------------------*/

    /*! \brief remove leading and trailing whitespace
     * \ingroup group_tools
     *
     * \param s any string
     * \return value a input string with any leading and trailing whitespace
     *         removed
     */
    std::string trimws(std::string s);

  } // namespace tools

} // namespace stfinv

#endif // STFINV_PARAMETERHANDLER_H_VERSION (includeguard)

/* ----- END OF parameterhandler.h ----- */
