/*! \file fapid_sff_new.cc
 * \brief Delete existing file (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 25/12/2010
 * 
 * Delete existing file (implementation)
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
 * REVISIONS and CHANGES 
 *  - 25/12/2010   V1.0   Thomas Forbriger
 *  - 14/01/2011   V1.1   only try to remove file if file exists
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_NEW_CC_VERSION \
  "TF_FAPID_SFF_NEW_CC   V1.1"

#include <string>
#include <cstdio>
#include <fapidxx/fapidsff.h>
#include <fapidxx/helper.h>
#include <fapidxx/error.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

using namespace fapidxx;

/*! \brief Delete existing file
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_New(lu, filename, ierr)
c 
c This routine just deletes the file indicated by filename. You need this
c as all WOpen routines refuse to replace an existing file. You will have
c to remove this file first and than open a new one.
c
c input:
c   lu         logical file unit to use for operation
c   filename   name of file to be deleted
c output:
c   ierr       will be 0 if operation was successfull
c
      integer lu, ierr
      character filename*(*)
c
c----------------------------------------------------------------------
 * \endcode
 */
int sff_new__(integer *lu, char *filename, integer *ierr, ftnlen filename_len)
{
  std::string name=stringfromfstring(filename, filename_len);
  struct stat buf;
  int status=lstat(name.c_str(), &buf);
  if (status==0)
  {
    int status=remove(name.c_str());
    if (status==0) { *ierr=0; } else { *ierr=1; }
  }
  else
  {
    *ierr=0;
  }
  int retval=0;
  return(retval);
} // int sff_new__

/* ----- END OF fapid_sff_new.cc ----- */
