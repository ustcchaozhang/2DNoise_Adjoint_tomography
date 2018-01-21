/*! \file fapid_sff_rtracei.cc
 * \brief mimic Fortran subroutine sff_RTraceI (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2010
 * 
 * mimic Fortran subroutine sff_RTraceI (implementation)
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
#define TF_FAPID_SFF_RTRACEI_CC_VERSION \
  "TF_FAPID_SFF_RTRACEI_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/fileunit.h>
#include <fapidxx/helper.h>
#include <fapidxx/error.h>

using namespace fapidxx;

/*! \brief Read one trace of data and return INFO line additionally
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_RTraceI(lu, tanf, dt,
     &                   wid2line, nsamp, fdata, idata, code, last,
     &                   cs, c1, c2, c3, nstack, ierr)
c
c Read one data block starting with DAST line.
c Read also INFO line.
c The File will be closed after reading the last trace.
c 
c input
c   lu              logical file unit
c   nsamp           array dimension of idata and fdata
c ouput:
c   ierr            error status (ok: ierr=0)
c   code            code indicating optional blocks
c   wid2line        valid WID2 line
c   tanf            time of first sample from midnight
c   dt              sampling interval in seconds
c   nsamp           number of samples
c   fdata           data array
c   last            is true if read trace is the last one in this file
c   cs              coordinate system
c   c1, c2, c3      receiver coordinates
c   nstack          number of stacks
c
c workspace:
c   idata           data will be first read to idata and then converted
c                   to fdata using sff_i2f (both array may be in same memory
c                   space - see comments on sff_f2i)
c
      integer lu, nsamp, idata(nsamp), nstack
      real fdata(nsamp), c1, c2, c3, dt, tanf
      logical last
      character wid2line*132, cs*1, code*(*)
c----------------------------------------------------------------------
 * \endcode
 */
int sff_rtracei__(integer *lu, real *tanf, real *dt, char *wid2line, 
                  integer *nsamp, real *fdata, integer *idata, char *code,
                  logical *last, char *cs, real *c1, real *c2, real *c3, 
                  integer *nstack, integer *ierr, ftnlen wid2line_len, 
                  ftnlen code_len, ftnlen cs_len)
{
  int retval=0;
  *ierr=0;
  try {
    datrw::ianystream &is=istreammanager(static_cast<int>(*lu));
    sff::WID2 wid2;
    sff::INFO info;
    datrw::Tfseries iseries;
    is >> iseries;
    FAPIDXX_fuassert((static_cast<int>(iseries.size())<=(*nsamp)), *lu,
                     "sff_rtracei__: too many samples");
    int nsamples=iseries.size();
    aff::LinearShape shape(0, nsamples-1, 0);
    datrw::Tfseries series(shape, aff::SharedHeap<real>(fdata, *nsamp));
    series.copyin(iseries);
    *last = is.last() ? 1 : 0;
    is >> wid2;
    std::string ocode("");
    if (is.hasfree()) { ocode.append("F"); }
    if (is.hasinfo()) { ocode.append("I"); }
    if (!is.last()) { ocode.append("D"); }
    if (is.hasinfo()) { is >> info; }
    fillfstring(ocode, code, code_len);
    *tanf=static_cast<real>(maketanf(wid2.date));
    *dt=static_cast<real>(wid2.dt);
    fillfstring(wid2.line(), wid2line, wid2line_len);
    *nsamp=series.size();
    char thecs=sff::coosysID(info.cs);
    fillfstring(std::string(&thecs, 1), cs, cs_len);
    *c1=static_cast<real>(info.cx);
    *c2=static_cast<real>(info.cy);
    *c3=static_cast<real>(info.cz);
    *nstack=info.nstacks;
    if (is.last()) { istreammanager.close(static_cast<int>(*lu)); }
  }
  catch(...) {
    *ierr=1;
  }
  return retval;
} // int sff_rtracei__

/* ----- END OF fapid_sff_rtracei.cc ----- */
