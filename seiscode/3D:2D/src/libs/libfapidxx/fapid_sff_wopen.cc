/*! \file fapid_sff_wopen.cc
 * \brief Open file for writing. Write STAT line. (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/07/2015
 * 
 * Open file for writing. Write STAT line. (implementation)
 * 
 * Copyright (c) 2015 by Thomas Forbriger (BFO Schiltach) 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 * ----
 *
 * REVISIONS and CHANGES 
 *  - 31/07/2015   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_WOPEN_CC_VERSION \
  "TF_FAPID_SFF_WOPEN_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <fapidxx/error.h>
#include <sffxx.h>

using namespace fapidxx;

/*! \brief Open file for writing.
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_WOpen(lu, filename, ierr)
c 
c Open file for writing. Write STAT line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c output:
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr
      character filename*(*)
c----------------------------------------------------------------------
 * \endcode
 */
int sff_wopen__(integer *lu, char *filename,
                integer *ierr, ftnlen filename_len)
{
  int retval=0;
  *ierr=0;
  try {
    datrw::oanystream &os=
      ostreammanager.open(static_cast<int>(*lu),
                          stringfromfstring(filename, filename_len));
  }
  catch (...) {
    *ierr=1;
  }
  return(retval);
} // int sff_wopen__

/* ----- END OF fapid_sff_wopen.cc ----- */
