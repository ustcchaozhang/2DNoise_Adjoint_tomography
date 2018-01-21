/*! \file filestatus.cc
 * \brief test status of file (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 04/12/2008
 * 
 * test status of file (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 04/12/2008   V1.0   Thomas Forbriger
 *  - 11/11/2009   V1.1   header cstdlib is required for free
 * 
 * ============================================================================
 */
#define TF_FILESTATUS_CC_VERSION \
  "TF_FILESTATUS_CC   V1.1"

#include<sstream>
#include<string>
#include<cstdlib>
#include <tfxx/filestatus.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <libgen.h>
#include <string.h>

namespace tfxx {

  namespace file {

    /*! true if a file is creatable 
     * the function checks whether the directory is writable
     */
    bool creatable(const char* path)
    {
      char *thepath=strdup(path);
      std::string dir(dirname(thepath));
      bool retval=(access(dir.c_str(), W_OK)==0);
      free(thepath);
      return(retval);
    } // bool creatable(const char* path)

    /*----------------------------------------------------------------------*/

    /*! true if the file exists
     *
     * for me a file exists if I can check its status
     */
    bool exists(const char* path)
    {
      struct stat status;
      bool retval=(stat(path, &status)==0);
      return(retval);
    } // bool exists(const char* path)

    /*----------------------------------------------------------------------*/

    /*! true if the file is writable
     */
    bool writable(const char* path)
    {
      bool retval=(access(path, W_OK)==0);
      return(retval);
    } // bool writable(const char* path)

    /*----------------------------------------------------------------------*/

    /*! true is the file is readable
     */
    bool readable(const char* path)
    {
      bool retval=false;
      if (exists(path)) { retval=(access(path, R_OK)==0); }
      return(retval);
    } // bool readable(const char* path)

    /*----------------------------------------------------------------------*/

    /*! true is the file is a regular file
     */
    bool regular(const char* path)
    {
      struct stat status;
      bool retval=false;
      if (stat(path, &status)==0) { retval=S_ISREG(status.st_mode); }
      return(retval);
    } // bool regular(const char* path)

    /*----------------------------------------------------------------------*/

    std::string uniquenew(const char* path)
    {
      std::string retval(path);
      int i=0;
      while (exists(retval.c_str()))
      {
        ++i;
        std::ostringstream oss;
        oss << path << "_" << i;
        retval=oss.str();
      }
      return(retval);
    } // std::string unique(const char* path)

  } // namespace file

} // namespace tfxx

/* ----- END OF filestatus.cc ----- */
