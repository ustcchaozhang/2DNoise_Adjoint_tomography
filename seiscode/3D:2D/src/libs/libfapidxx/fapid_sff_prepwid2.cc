/*! \file fapid_sff_prepwid2.cc
 * \brief encode WID2 data into a character sequence (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 01/04/2011
 * 
 * encode WID2 data into a character sequence (implementation)
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
 * REVISIONS and CHANGES 
 *  - 01/04/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FAPID_SFF_PREPWID2_CC_VERSION \
  "TF_FAPID_SFF_PREPWID2_CC   V1.0   "

#include <fapidxx/fapidsff.h>
#include <fapidxx/helper.h>
#include <fapidxx/wid2container.h>
#include <fapidxx/error.h>

/*! \brief Encode WID2 data
 *
 * \ingroup implemented_functions
 *
 * Description from stuff.f:
 * \code
c----------------------------------------------------------------------
c
c  Prepare a WID2 line from scratch
c
c  The routine sets defaults for all variables set either to -1
c  for integers, -1. for floats or 'NSP' (not specified) for characters
c  If a component e.g. in the form LHZ is given hang and vang are
c  determined automatically
c
c  Reasonable values must be given for at least 
c      samprat:    sampling rate (= 1./(sampling interval))
c      nsamp:      number of smaples
c      station:    station name
c
c  Defaults are:
c      year:       0
c      month.      0
c      day:        0
c      hour:       0
c      minute:     0
c      second:     10.0
c      comp:       NSP
c      auxid:      NSP     
c      instyp:     NSP
c      calib:      1.
c      calper:     1.
c      hang:       -1.
c      vang:       90.
c 
c  Returns  wid2line
c
c  major changes:
c    22/11/96   T.F.   changed format of calib to e10.2 as defined by GSE2.0
c
      integer nsamp, year, month, day, hour, minute
      character comp*(*), auxid*(*), station*(*), instyp*(*)
      real samprat, second, calib, calper, hang, vang
      character wid2line*(*)
      integer ierr
c----------------------------------------------------------------------
 * \endcode
 */
int sff_prepwid2__(integer *nsamp, real *samprat, char *station, 
                   integer *year, integer *month, integer *day,
                   integer *hour, integer *minute, char *comp,
                   char *auxid, char *instyp, real *second, real *calib,
                   real *calper, real *hang, real *vang, char *wid2line,
                   integer *ierr, ftnlen station_len, ftnlen comp_len,
                   ftnlen auxid_len, ftnlen instyp_len, ftnlen wid2line_len)
{
  ::sff::WID2 wid2;
  wid2.nsamples= *nsamp;
  wid2.dt      = 1./(*samprat);
  wid2.station =fapidxx::stringfromfstring(station, station_len);
  double dsecond=*second;
  libtime::timeint isecond=static_cast<libtime::timeint>(std::floor(dsecond));
  libtime::timeint milsec
    =static_cast<libtime::timeint>(std::floor(1.e3*dsecond));
  libtime::timeint micsec
    =static_cast<libtime::timeint>(std::floor(1.e6*dsecond));
  milsec -= 1000*isecond;
  micsec -= 1000*(milsec+1000*isecond);
  libtime::TAbsoluteTime date(*year, *month, *day, *hour, *minute,
                              isecond, milsec, micsec);
  wid2.date    =date;
  wid2.channel =fapidxx::stringfromfstring(comp, comp_len);
  wid2.auxid   =fapidxx::stringfromfstring(auxid, auxid_len);
  wid2.instype =fapidxx::stringfromfstring(instyp, instyp_len);
  wid2.calib   = *calib;
  wid2.calper  = *calper;
  wid2.hang    = *hang;
  wid2.vang    = *vang;
  fapidxx::WID2container wid2c(wid2);
  wid2c.encode(wid2line, wid2line_len);
  *ierr=0;
  return(0);
} // int sff_prepwid2__

/* ----- END OF fapid_sff_prepwid2.cc ----- */
