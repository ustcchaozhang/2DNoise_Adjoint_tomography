/*! \file fapid_sff_ropenfs.cc
 * \brief open file for reading, read FREE block and SRCE line (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/01/2011
 * 
 * open file for reading, read FREE block and SRCE line (implementation)
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
 *  - 17/01/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_ROPENFS_CC_VERSION \
  "TF_FAPID_SFF_ROPENFS_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <sffxx.h>

using namespace fapidxx;

/*! \brief Open SFF file and return source line and FREE block
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_ROpenFS(lu, filename,
     &                      version, timestamp, code,
     &                      nline, lines, lenmax, lindim,
     &                      type, cs, c1, c2, c3, date, time, ierr)
c 
c Open file for reading. Read STAT line, FREE block and SRCE line.
c 
c input:
c   lu              logical file unit
c   filename        name of file
c   lindim          number of elements in FREE block array lines
c ouput:
c   version         version of writing library
c   timestamp       time and date file was written
c   code            indicates optional blocks
c   ierr            error status (ok: ierr=0)
c   nline           number of FREE lines read
c   lines           FREE lines
c   lenmax          length of longest FREE line in array lines
c   type            type of source
c   cs              coordinate system
c   c1, c2, c3      coordinates of source
c   date, time      date and time of source signal
c
      integer lu, ierr, nline, lindim, lenmax
      real version
      character timestamp*(*), code *(*), lines(lindim)*(*)
      character filename*(*), type*(*), date*(*), time*(*), cs*1
      real c1, c2, c3
c----------------------------------------------------------------------
 * \endcode
 */
int sff_ropenfs__(integer *lu, char *filename, real *version, 
                  char *timestamp, char *code,
                  integer *nline, char *lines, integer *lenmax,
                  integer *lindim,
                  char *type__, char *cs, real *c1, real *c2, real *c3,
                  char *date, char *time, integer *ierr,
                  ftnlen filename_len, ftnlen timestamp_len, ftnlen code_len,
                  ftnlen lines_len, ftnlen type_len, ftnlen cs_len,
                  ftnlen date_len, ftnlen time_len)
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
    // set SRCE data
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
    // set free block
    sff::FREE free;
    if (is.hasfree()) { is >> free; }
    freeblock(free, nline, lines, lindim, lenmax, lines_len);
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
} // int sff_ropenfs__

/* ----- END OF fapid_sff_ropenfs.cc ----- */
