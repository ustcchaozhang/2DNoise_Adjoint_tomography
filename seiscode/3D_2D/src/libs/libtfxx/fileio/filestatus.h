/*! \file filestatus.h
 * \brief test status of file (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 04/12/2008
 * 
 * test status of file (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FILESTATUS_H_VERSION

#define TF_FILESTATUS_H_VERSION \
  "TF_FILESTATUS_H   V1.0   "

#include<string>

namespace tfxx {

  /*! some components to access files and test file status.
   */
  namespace file {

    /*! true if a file is creatable 
     * the function checks whether the directory is writable.
     */
    bool creatable(const char* path);

    /*! true if the file exists.
     */
    bool exists(const char* path);

    /*! true if the file is writable.
     */
    bool writable(const char* path);

    /*! true is the file is readable.
     */
    bool readable(const char* path);

    /*! true is the file is a regular file.
     */
    bool regular(const char* path);

    /*! return unique file name that does not already exist
     * based on the path passed to the function.
     */
    std::string uniquenew(const char* path);

  } // namespace file

} // namespace tfxx

#endif // TF_FILESTATUS_H_VERSION (includeguard)

/* ----- END OF filestatus.h ----- */
