/*! \file fapid_sff_wopenfs.cc
 * \brief open file for writing with FREE block and SRCE line (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/01/2011
 * 
 * open file for writing with FREE block and SRCE line (implementation)
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
 *  - 14/01/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_WOPENFS_CC_VERSION \
  "TF_FAPID_SFF_WOPENFS_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <fapidxx/error.h>
#include <sffxx.h>

using namespace fapidxx;

/*! \brief Open file for writing and pass FREE block and SRCE line
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_WOpenFS(lu, filename,
     &                      lines, nline,
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for writing. Write STAT line, FREE block and SRCE line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c   nline           number of lines in FREE block
c   lines           lines in FREE block
c   type            type of source (any 20 character string)
c   cs              coordinate system (S: spherical, C: cartesian)
c   c1, c2, c3      coordiantes as defined by SFF
c   time, date      time and date of source signal
c output:
c   ierr            error status (ok: ierr=0)
c
      integer lu, ierr, nline
      character filename*(*), type*(*), cs*1
      real c1, c2, c3
      character time*(*), date*(*)
      character lines(nline)*80
c----------------------------------------------------------------------
 * \endcode
 */
int sff_wopenfs__(integer *lu, char *filename, char *lines, integer *nline,
                  char *type__, char *cs, real *c1, real *c2, real *c3, 
                  char *date, char *time, integer *ierr, ftnlen filename_len,
                  ftnlen lines_len, ftnlen type_len, ftnlen cs_len,
                  ftnlen date_len, ftnlen time_len)
{
  int retval=0;
  *ierr=0;
  try {
    datrw::oanystream &os=
      ostreammanager.open(static_cast<int>(*lu),
                          stringfromfstring(filename, filename_len));
    if (os.handlessrce())
    {
      sff::SRCE srce;
      srce.cs=sff::coosysID(*cs);
      srce.cx=static_cast<double>(*c1);
      srce.cy=static_cast<double>(*c2);
      srce.cz=static_cast<double>(*c3);
      srce.type=stringfromfstring(type__, type_len);
      srce.date=SRCEdate(date, time, date_len, time_len);
      os << srce;
    }
    if (os.handlesfilefree())
    {
      sff::FREE filefree=freeblock(nline, lines, lines_len);
      os << filefree;
    }
  }
  catch (...) {
    *ierr=1;
  }
  return(retval);
} // int sff_wopenfs__

/* ----- END OF fapid_sff_wopenfs.cc ----- */
