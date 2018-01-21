/*! \file fapid_sff_modwid2shift.cc
 * \brief shift time of first sample in WID2 line (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 04/07/2014
 * 
 * shift time of first sample in WID2 line (implementation)
 * 
 * Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 04/07/2014   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_MODWID2SHIFT_CC_VERSION \
  "TF_FAPID_SFF_MODWID2SHIFT_CC   V1.0   "
#define TF_FAPID_SFF_MODWID2SHIFT_CC_CVSID \
  "$Id: $"

#include <fapidxx/fapidsff.h>
#include <fapidxx/wid2container.h>
#include <fapidxx/error.h>

/*! \brief shift time of first sample in WID2 line
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_ModWid2shift(wid2line, tmin, tsec)
c
c Change time and date of first sample by adding tmin minutes
c and tsec seconds
c
c input:
c   tmin        minutes to add
c   tsec        seconds to add
c input and output:
c   wid2line    string to change
c----------------------------------------------------------------------
 * \endcode
 */
int sff_modwid2shift__(char *wid2line, real *tmin, real *
	tsec, ftnlen wid2line_len)
{
  fapidxx::WID2container wid2c(wid2line, wid2line_len);
  libtime::TAbsoluteTime date(wid2c.wid2.date);
  wid2c.wid2.date+=libtime::double2time(double(*tsec)+60.*double(*tmin));
  wid2c.encode(wid2line, wid2line_len);
  return 0;
} // int sff_modwid2shift__

/* ----- END OF fapid_sff_modwid2shift.cc ----- */
