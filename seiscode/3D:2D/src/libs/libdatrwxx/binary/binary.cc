/*! \file binary.cc
 * \brief write raw binary data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 24/02/2010
 * 
 * write raw binary data (implementation)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 24/02/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_BINARY_CC_VERSION \
  "DATRW_BINARY_CC   V1.0   "

#include <datrwxx/binary.h>
#include <datrwxx/error.h>

namespace datrw {

  /*! \brief Format properties
   * \ingroup group_binary
   * @{
   */
  const bool binary::isbinary=false;
  const char* const binary::streamID="bin";
  /**@}*/

  namespace binary{

    const char* const magic="TBIN";
    const short version=1;

    /*----------------------------------------------------------------------*/

    void checkfileflags(const char& flags)
    {
      DATRW_assert(!((flags & Fsrce) && (flags & Finfo)),
                   "ERROR: flags indicate INFO and SRCE at the same time");
    }

    /*----------------------------------------------------------------------*/

    void checktraceflags(const char& flags)
    {
      checkfileflags(flags);
      unsigned int ntypes=0;
      if (flags & Fdouble) { ++ntypes; }
      if (flags & Ffloat) { ++ntypes; }
      if (flags & Fint) { ++ntypes; }
      DATRW_assert(ntypes == 1,
                   "ERROR: flags must indicate exactly one data type");
    }

  } // namespace binary

} // namespace datrw

/* ----- END OF binary.cc ----- */
