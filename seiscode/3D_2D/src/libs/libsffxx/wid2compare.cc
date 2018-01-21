/*! \file wid2compare.cc
 * \brief compare WID2 headers (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2004
 * 
 * compare WID2 headers (implementation)
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 28/01/2004   V1.0   Thomas Forbriger
 *  - 19/07/2010          Daniel Armbruster: deleted header <tfxx/error.h>
 * ============================================================================
 */
#define TF_WID2COMPARE_CC_VERSION \
  "TF_WID2COMPARE_CC   V1.0   "

#include <cmath>
#include <sffxx.h>

namespace sff {

  bool WID2compare::operator()(const WID2& hd1, const WID2& hd2) const
  {
    bool retval=true;
    if (Mflags & Fdate) {
      libtime::TRelativeTime diff=hd2.date-hd1.date;
      retval=retval && (libtime::time2double(diff) 
                        <= ((hd1.dt+hd2.dt)*0.5*Mdatetolerance)); 
    }
    if (Mflags & Fstation) { retval=retval && (hd1.station==hd2.station); }
    if (Mflags & Fchannel) { retval=retval && (hd1.channel==hd2.channel); }
    if (Mflags & Fauxid) { retval=retval && (hd1.auxid==hd2.auxid); }
    if (Mflags & Fnsamples) { retval=retval && (hd1.nsamples==hd2.nsamples); }
    if (Mflags & Fdt) 
    { 
      double diff=hd1.dt-hd2.dt;
      diff = diff >= 0. ? diff : -diff;
      retval=retval && (diff <= (hd1.dt*Mdttolerance)); 
    }
    if (Mflags & Fcalib) { retval=retval && (hd1.calib==hd2.calib); }
    if (Mflags & Fcalper) { retval=retval && (hd1.calper==hd2.calper); }
    if (Mflags & Finstype) { retval=retval && (hd1.instype==hd2.instype); }
    if (Mflags & Fhang) { retval=retval && (hd1.hang==hd2.hang); }
    if (Mflags & Fvang) { retval=retval && (hd1.vang==hd2.vang); }
    return(retval);
  }; // WID2compare::operator()

} // namespace sff

/* ----- END OF wid2compare.cc ----- */
