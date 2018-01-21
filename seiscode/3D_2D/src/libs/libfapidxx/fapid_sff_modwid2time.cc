/*! \file fapid_sff_modwid2time.cc
 * \brief modify time of first sample in WID2 line (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 04/07/2014
 * 
 * modify time of first sample in WID2 line (implementation)
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
#define TF_FAPID_SFF_MODWID2TIME_CC_VERSION \
  "TF_FAPID_SFF_MODWID2TIME_CC   V1.0   "
#define TF_FAPID_SFF_MODWID2TIME_CC_CVSID \
  "$Id: $"

#include <fapidxx/fapidsff.h>
#include <fapidxx/wid2container.h>
#include <fapidxx/error.h>

/*! \brief modify time of first sample in WID2 line
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
      subroutine sff_ModWid2time(wid2line, hour, minute, second)
c
c Just modify time-entry in WID2 line
c
c input:
c   hour, minute, second   new time to set
c input and output:
c   wid2line               modified WID2 line
c
c no default setting are allowed
c----------------------------------------------------------------------
 * \endcode
 */
int sff_modwid2time__(char *wid2line, integer *hour, integer 
	*minute, real *second, ftnlen wid2line_len)
{
  fapidxx::WID2container wid2c(wid2line, wid2line_len);
  libtime::TAbsoluteTime date(wid2c.wid2.date);
  wid2c.wid2.date=libtime::TAbsoluteTime(date.year(),
                                         date.month(),
                                         date.day());
  wid2c.wid2.date+=libtime::TRelativeTime(0,(*hour),(*minute))
    +libtime::double2time(double(*second));
  wid2c.encode(wid2line, wid2line_len);
  return 0;
} /* sff_modwid2time__ */

/* ----- END OF fapid_sff_modwid2time.cc ----- */
