/*! \file asciiformat.cc
 * \brief ASCII format specification (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/10/2011
 * 
 * ASCII format specification (implementation)
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
 *  - 18/10/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_ASCIIFORMAT_CC_VERSION \
  "DATRW_ASCIIFORMAT_CC   V1.0   "

#include<datrwxx/ascii.h>

namespace datrw {

  /*! \brief Format properties
   * \ingroup group_ascii
   * @{
   */
  const bool ascii::isbinary=false;
  const char* const ascii::streamID="ascii";
  /**@}*/

} // namespace datrw

/*----------------------------------------------------------------------*/

/*! \brief I/O module for ASCII data
 *
 * \defgroup group_ascii I/O module for ASCII data
 *
 * Lines of ASCII data files either start with 
 *   - a single hash \c #: these lines provide header data; see keywords below
 * .
 * or
 *   - a double hash \c ##: these lines are pure comments from FREE blocks
 * .
 * or
 *   - no hash: these lines contain sample values
 * .
 * Each file can contain multiple traces.
 */

/* ----- END OF asciiformat.cc ----- */
