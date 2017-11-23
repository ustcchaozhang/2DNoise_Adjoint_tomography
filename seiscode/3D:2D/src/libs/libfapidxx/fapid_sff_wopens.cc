/*! \file fapid_sff_wopens.cc
 * \brief mimic sff_WOpenS function - open for writing, passing SRCE line (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/12/2010
 * 
 * mimic sff_WOpenS function - open for writing, passing SRCE line (implementation)
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
 *  - 23/12/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_WOPENS_CC_VERSION \
  "TF_FAPID_SFF_WOPENS_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <fapidxx/error.h>
#include <sffxx.h>

using namespace fapidxx;

/*! \brief Open file for writing and pass SRCE line
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_WOpenS(lu, filename, 
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for writing. Write STAT line and SRCE line.
c
c input:
c   lu              logical file unit
c   filename        name of file
c   type            type of source (any 20 character string)
c   cs              coordinate system (S: spherical, C: cartesian)
c   c1, c2, c3      coordiantes as defined by SFF
c   time, date      time and date of source signal
c output:
c   ierr            error status (ok: ierr=0)
c
c----------------------------------------------------------------------
 * \endcode
 */
int sff_wopens__(integer *lu, char *filename, char *type__, char *cs, 
                 real *c1, real *c2, real *c3, char *date, char *time,
                 integer *ierr, ftnlen filename_len, ftnlen type_len,
                 ftnlen cs_len, ftnlen date_len, ftnlen time_len)
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
  }
  catch (...) {
    *ierr=1;
  }
  return(retval);
} // int sff_wopens__

/* ----- END OF fapid_sff_wopens.cc ----- */
