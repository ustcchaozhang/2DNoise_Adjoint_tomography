/*! \file fapid_sff_ropens.cc
 * \brief mimic Fortran subroutine sff_ROpenS (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2010
 * 
 * mimic Fortran subroutine sff_ROpenS (implementation)
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
 * 
 * REVISIONS and CHANGES 
 *  - 18/11/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_ROPENS_CC_VERSION \
  "TF_FAPID_SFF_ROPENS_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <sffxx.h>

using namespace fapidxx;

/*! \brief Open SFF file and return source line
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_ROpenS(lu, filename,
     &                      version, timestamp, code,
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for reading. Read STAT line and SRCE line.
c 
c input:
c   lu              logical file unit
c   filename        name of file
c ouput:
c   version         version of writing library
c   timestamp       time and date file was written
c   code            indicates optional blocks
c   ierr            error status (ok: ierr=0)
c   type            type of source
c   cs              coordinate system
c   c1, c2, c3      coordinates of source
c   date, time      date and time of source signal
c
      integer lu, ierr
      real version
      character timestamp*(*), code *(*)
      character filename*(*), type*(*), date*(*), time*(*), cs*1
      real c1, c2, c3
c----------------------------------------------------------------------
 * \endcode
 */
int sff_ropens__(integer *lu, char *filename, real *version, char *timestamp,
                 char *code, char *type__, char *cs, real *c1, real *c2, 
                 real *c3, char *date, char *time, integer *ierr,
                 ftnlen filename_len, ftnlen timestamp_len, ftnlen code_len, 
                 ftnlen type_len, ftnlen cs_len, ftnlen date_len, 
                 ftnlen time_len)
{
  int retval=0;
  *ierr=0;
  try {
    datrw::ianystream &is=
      istreammanager.open(static_cast<int>(*lu),
                          stringfromfstring(filename, filename_len));
    sff::SRCE srce;
    std::string ocode("");
    if (is.hasfree()) { ocode.append("F"); }
    if (is.hassrce()) { ocode.append("S"); }
    if (is.hassrce()) { is >> srce; }
    // set output
    std::string srceline=srce.line();
    fillfstring(ocode, code, code_len);
    fillfstring(srce.type, type__, type_len);
    char thecs=sff::coosysID(srce.cs);
    fillfstring(std::string(&thecs, 1), cs, cs_len);
    *c1=static_cast<real>(srce.cx);
    *c2=static_cast<real>(srce.cy);
    *c3=static_cast<real>(srce.cz);
    fillfstring(srceline.substr(74,6), date, date_len);
    fillfstring(srceline.substr(81,10), time, time_len);
    // timestamp and version got lost
    sff::STAT stat;
    fillfstring(stat.timestamp, timestamp, timestamp_len);
    *version=static_cast<real>(sff::STAT::libversion);
//    std::cerr << "leaving sff_ropens__" << std::endl;
  }
  catch(...) {
    *ierr=1;
  }
  return retval;
} // int sff_ropens__

/* ----- END OF fapid_sff_ropens.cc ----- */
