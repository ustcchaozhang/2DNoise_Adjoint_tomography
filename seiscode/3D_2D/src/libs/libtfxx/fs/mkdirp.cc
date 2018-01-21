/*! \file mkdirp.cc
 * \brief create a complete path (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/12/2012
 * 
 * create a complete path (implementation)
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
#define TF_MKDIRP_CC_VERSION \
  "TF_MKDIRP_CC   V1.0   "

#include <tfxx/fs.h>
#include <errno.h>

namespace tfxx {

  namespace fs {

    void mkdirp(const std::string& path)
    {
      tfxx::error::Exception::dont_report_on_construct();
      try {
        tfxx::fs::mkdir(path);
      } catch (const tfxx::error::FSException& e) {
        if (!e.iserrno(ENOENT))
        {
          e.report();
          throw(e);
        }
        std::string dir=tfxx::fs::dirname(path);
        if (dir == path)
        {
          e.report();
          throw(e);
        }
        else
        {
          // std::cerr << "going to create " << dir << std::endl;
          tfxx::fs::mkdirp(dir);
          // std::cerr << "retry to create " << path << std::endl;
          tfxx::fs::mkdir(path);
        }
      } catch (tfxx::error::Exception& e) {
        e.report();
        throw(e);
      }
      tfxx::error::Exception::restore_report_state();
    } // void mkdirp(const std::string& path)

  } // namespace fs

}

/* ----- END OF mkdirp.cc ----- */
