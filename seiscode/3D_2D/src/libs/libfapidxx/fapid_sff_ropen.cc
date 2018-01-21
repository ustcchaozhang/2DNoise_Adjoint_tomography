/*! \file fapid_sff_ropen.cc
 * \brief simply open file (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 02/01/2011
 * 
 * simply open file (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 02/01/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_ROPEN_CC_VERSION \
  "TF_FAPID_SFF_ROPEN_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <sffxx.h>

using namespace fapidxx;

/*! \brief Open SFF file
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_ROpen(lu, filename, 
     &                     version, timestamp, code, ierr)
c 
c Open file for reading. Read STAT line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c ouput:
c   version         version of writing library
c   timestamp       time and date file was written
c   code            indicates optional blocks
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr
      real version
      character timestamp*(*), code *(*)
      character filename*(*)
c----------------------------------------------------------------------
 * \endcode
 */
int sff_ropen__(integer *lu, char *filename, real *version, char *timestamp, 
                char *code, integer *ierr, ftnlen filename_len, 
                ftnlen timestamp_len, ftnlen code_len)
{
  int retval=0;
  *ierr=0;
  try {
    datrw::ianystream &is=
      istreammanager.open(static_cast<int>(*lu),
                          stringfromfstring(filename, filename_len));
    std::string ocode("");
    if (is.hasfree()) { ocode.append("F"); }
    if (is.hassrce()) { ocode.append("S"); }
    // set output
    fillfstring(ocode, code, code_len);
    // timestamp and version got lost
    sff::STAT stat;
    fillfstring(stat.timestamp, timestamp, timestamp_len);
    *version=static_cast<real>(sff::STAT::libversion);
  }
  catch(...) {
    *ierr=1;
  }
  return retval;
} // int sff_ropen__


/* ----- END OF fapid_sff_ropen.cc ----- */
