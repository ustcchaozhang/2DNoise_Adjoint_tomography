/*! \file mkdir.cc
 * \brief interface to mkdir(2) (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/12/2012
 * 
 * interface to mkdir(2) (implementation)
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
#define TF_MKDIR_CC_VERSION \
  "TF_MKDIR_CC   V1.0   "

#include <tfxx/fs.h>
#include <sys/stat.h>
#include <errno.h>

namespace tfxx {

  namespace fs {

    void mkdir(const std::string& path)
    {
      int status;
      status=::mkdir(path.c_str(), 
                     S_IRUSR | S_IWUSR | S_IXUSR |
                     S_IRGRP | S_IWGRP | S_IXGRP |
                     S_IROTH | S_IWOTH | S_IXOTH);
      int errsv=errno;
      if (status != 0)
      {
        throw tfxx::error::FSException("mkdir() failed", 
                                    __FILE__, __LINE__, errsv);
      }
    }

  } // namespace fs

}

/* ----- END OF mkdir.cc ----- */
