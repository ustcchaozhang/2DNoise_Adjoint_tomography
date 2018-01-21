/*! \file exception.cc
 * \brief exception class for file system utilities (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/12/2012
 * 
 * exception class for file system utilities (implementation)
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
#define TF_EXCEPTION_CC_VERSION \
  "TF_EXCEPTION_CC   V1.0"

#include <iostream>
#include <tfxx/fs.h>
#include <tfxx/error.h>
#include <cstring>

namespace tfxx {

  namespace error {

    FSException::FSException(const char* message, 
                    const char* file,
                    const int& line,
                    const int& en):
      TBase(message, file, line, "file system utility returned error"),
      Merrno(en) 
    {
      if (this->report_on_construct_is_true()) { this->fs_report(); }
    }

    void FSException::report() const
    {
      TBase::report();
      this->fs_report();
    }

    void FSException::fs_report() const
    {
      std::cerr << "  error value: " << Merrno << std::endl;
      std::cerr << "  " << strerror(Merrno) << std::endl;
    }

  } // namespace error

}

/* ----- END OF exception.cc ----- */
