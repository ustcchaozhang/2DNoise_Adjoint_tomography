/*! \file fapid_sff_wtrace.cc
 * \brief Write one data block starting with DAST line. (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/07/2015
 * 
 * Write one data block starting with DAST line. (implementation)
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
#define TF_FAPID_SFF_WTRACE_CC_VERSION \
  "TF_FAPID_SFF_WTRACE_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <fapidxx/wid2container.h>
#include <fapidxx/error.h>

using namespace fapidxx;

/*! \brief Write one trace of data with INFO line
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_WTrace(lu, wid2line, nsamp, fdata, idata, last, ierr)
c
c Write one data block starting with DAST line.
c The File will be closed after writing the last trace.
c 
c input:
c   lu              logical file unit
c   wid2line        valid WID2 line
c   nsamp           number of samples
c   fdata           data array
c   last            must be true is the trace to be written is the
c                   last one in this file
c ouput:
c   ierr            error status (ok: ierr=0)
c
c workspace:
c   idata           fdata will be converted to idata using sff_f2i
c                   (both array may be in same memory space - see
c                   comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp)
      real fdata(nsamp)
      logical last
      character wid2line*132
c----------------------------------------------------------------------
 * \endcode
 */
int sff_wtrace__(integer *lu, char *wid2line, integer *nsamp, real *fdata,
                 integer *idata, logical *last, integer *ierr, 
                 ftnlen wid2line_len)
{
  int retval=0;
  *ierr=0;
  try {
    datrw::oanystream &os=ostreammanager(static_cast<int>(*lu));
    WID2container wid2c(wid2line, wid2line_len);
    unsigned int nsamples=static_cast<unsigned int>(*nsamp);
    os << wid2c.wid2;
    aff::LinearShape shape(0, nsamples-1, 0);
    datrw::Tfseries series(shape, aff::SharedHeap<real>(fdata, *nsamp));
    os << series;
    if (*last) ostreammanager.close(static_cast<int>(*lu));
  }
  catch (...) {
    *ierr=1;
  }
  return(retval);
} // int sff_wtrace__

/* ----- END OF fapid_sff_wtrace.cc ----- */
