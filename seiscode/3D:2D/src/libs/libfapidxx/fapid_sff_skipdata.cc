/*! \file fapid_sff_skipdata.cc
 * \brief mimic Fortran subroutine sff_SkipData (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2010
 * 
 * mimic Fortran subroutine sff_SkipData (implementation)
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
#define TF_FAPID_SFF_SKIPDATA_CC_VERSION \
  "TF_FAPID_SFF_SKIPDATA_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>

using namespace fapidxx;

/*! \brief Skip a complete trace
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c--------------------------------------------------------------------------
      subroutine sff_SkipData(lu,code,last,ierr)
c
c  Skip a data block including DAST, WID2 DAT2, data, CHK2 and optionally
c  appended Free and Info blocks. Close file if last data block
c  is read.
c
c  major changes:
c    22/11/96   T.F.   added variables code and last to parameter list
c                      (is needed to decide whether there will
c                       follow another datablock or not)
c
      integer lu,ierr
      logical last
      character code*(*)
c--------------------------------------------------------------------------
 * \endcode
 */

int sff_skipdata__(integer *lu, char *code, logical *last, integer *ierr,
                   ftnlen code_len)
{
  int retval=0;
  *ierr=0;
  try {
    datrw::ianystream &is=istreammanager(static_cast<int>(*lu));
    std::string ocode("");
    if (is.hasinfo()) 
    {
      ocode="I";
    }
    if (!is.last()) { ocode += "D"; }
    fillfstring(ocode, code, code_len);
    is.skipseries();
    *last = is.last() ? 1 : 0;
    if (is.last()) { istreammanager.close(static_cast<int>(*lu)); }
  }
  catch(...) {
    *ierr=1;
  }
  return retval;
} // int sff_skipdata__

/* ----- END OF fapid_sff_skipdata.cc ----- */
