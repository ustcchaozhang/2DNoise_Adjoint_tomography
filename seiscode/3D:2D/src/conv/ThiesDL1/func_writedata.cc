/*! \file func_writedata.cc
 * \brief function to write data in libdatrwxx formats (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id: $
 * \author Thomas Forbriger
 * \date 23/03/2014
 * 
 * function to write data in libdatrwxx formats (implementation)
 * 
 * Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 23/03/2014   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DL1_FUNC_WRITEDATA_CC_VERSION \
  "DL1_FUNC_WRITEDATA_CC   V1.0   "
#define DL1_FUNC_WRITEDATA_CC_CVSID \
  "$Id: $"

#include <fstream>
#include <datrwxx/writeany.h>
#include "functions.h"

namespace dl1 {

  // write data to file
  void writedata(const std::string& filename, 
                 const std::string& datatype,
                 Record& record,
                 const Tlistofstring& info)
  {
    std::ofstream ofs(filename.c_str());
    DL1_assert(ofs.good(), "could not open output file");
    datrw::oanystream os(ofs, datatype);

    sff::FREE filefree;
    filefree.append(info);
    filefree.append(record.header());
    filefree.append(record.logmessages());
    os << filefree;

    os << record.wid2line();
    os << record.iseries();

    DL1_assert(ofs.good(), "could not write to output file");
  } // void writedata(...)

} // namespace dl1

/* ----- END OF func_writedata.cc ----- */
