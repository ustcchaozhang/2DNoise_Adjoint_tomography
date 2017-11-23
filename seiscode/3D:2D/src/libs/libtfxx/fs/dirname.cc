/*! \file dirname.cc
 * \brief interface to dirname(3) (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/12/2012
 * 
 * interface to dirname(3) (implementation)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 16/12/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_DIRNAME_CC_VERSION \
  "TF_DIRNAME_CC   V1.0"

#include <tfxx/fs.h>
#include <cstring>
#include <libgen.h>

namespace tfxx {

  namespace fs {

    std::string dirname(const std::string& path)
    {
      unsigned int n=path.length()+2;
      char* p=new char[n];
      std::strncpy(p, path.c_str(), n);
      std::string retval(::dirname(p));
      delete[] p;
      return retval;
    }

  } // namespace fs

}

/* ----- END OF dirname.cc ----- */
