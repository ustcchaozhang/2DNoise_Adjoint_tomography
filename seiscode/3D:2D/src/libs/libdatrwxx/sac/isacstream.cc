/*! \file isacstream.cc
 * \brief read SAC files (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2004
 * 
 * read SAC files (implementation)
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 21/12/2004   V1.0   Thomas Forbriger
 *  - 03/05/2010   V1.1   sac provides debugging option
 *  - 23/11/2010   V1.2   introduced static member data
 *  - 18/11/2016   V1.3   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_ISACSTREAM_CC_VERSION \
  "DATRW_ISACSTREAM_CC   V1.3"

#include <string>
#include <sstream>
#include <datrwxx/sac.h>
#include <datrwxx/sacread.h>
#include <datrwxx/error.h>
#include <datrwxx/util.h>
#include <datrwxx/debug.h>

namespace datrw {

  const std::ios_base::openmode
    isacstream::openmode=std::ios_base::in|std::ios_base::binary;

  //@{
  /*! \brief Format properties
   * \ingroup group_sac
   */
  const bool sac::isbinary=true;
  const char* const sac::streamID="sac";
  //@}

  /*----------------------------------------------------------------------*/

  isacstream::isacstream(std::istream& is, const bool& debug):
    Tbase(is, true, true, true, debug)
  { }

  /*----------------------------------------------------------------------*/

  Tdseries isacstream::dseries()
  {
    return(datrw::util::convert<Tfseries, Tdseries>(this->fseries()));
  }

  /*----------------------------------------------------------------------*/

  Tfseries isacstream::fseries()
  {
    this->readheader();
    return(datrw::sac::read_sac_data(this->Mis, this->wid2().nsamples));
  }

  /*----------------------------------------------------------------------*/

  Tiseries isacstream::iseries()
  {
    return(datrw::util::convert<Tfseries, Tiseries>(this->fseries()));
  }

  /*----------------------------------------------------------------------*/

  void isacstream::skipseries()
  {
    DATRW_debug(this->Mdebug, "isacstream::skipseries()",
                  "entered function: call this->readheader()");
    this->readheader();
    DATRW_debug(this->Mdebug, "isacstream::skipseries()",
                  "finished; return from function");
  }

  /*----------------------------------------------------------------------*/

  void isacstream::readheader()
  {
    DATRW_debug(this->Mdebug, "isacstream::readheader()",
                  "start reading header");
    this->newtrace();
    sff::FREE free;
    sff::WID2 wid2;
    sff::INFO info;
    std::ostringstream oss;
    DATRW_debug(this->Mdebug, "isacstream::readheader()",
                  "calling datrw::sac::read_sac_header");
    datrw::sac::SACheader hd=datrw::sac::read_sac_header(this->Mis);
    DATRW_debug(this->Mdebug, "isacstream::readheader()",
                  "returned from datrw::sac::read_sac_header");

    oss << "location: ";
    if (hd.stla>0) { oss << hd.stla << "°N; "; }
    else { oss << -hd.stla << "°S; "; }
    if (hd.stlo>0) { oss << hd.stlo << "°E; "; }
    else { oss << -hd.stlo << "°W"; }
    free.append(oss.str());
    DATRW_debug(this->Mdebug, "isacstream::readheader()", oss.str());
    free.append("values from SAC binary header " 
                "that do not fit into SFF trace header");
    wid2.dt=hd.delta;
    oss.str("");
    oss << "initial index: " << hd.b;
    oss << "; final index: " << hd.e;
    free.append(oss.str());
    info.cs=sff::CS_spherical;
    info.cx=hd.stla;
    info.cy=hd.stlo;
    info.cz=hd.stel;
    oss.str("");
    oss << hd.stdp;
    free.append("station depth: " + oss.str() + " m");
    wid2.hang=hd.cmpaz;
    wid2.vang=hd.cmpinc;
    wid2.date=libtime::TAbsoluteTime(hd.nzyear, 1, 1, hd.nzhour,
                                     hd.nzmin, hd.nzsec, hd.nzmsec);
    wid2.date.setdoy(hd.nzjday);
    wid2.nsamples=hd.npts;
    wid2.station=std::string(hd.kstnm,8);
    wid2.channel=std::string(hd.kcmpnm,8);
    wid2.auxid=std::string(hd.khole,8);
    free.append("network name: " + std::string(hd.knetwk, 8));
    DATRW_assert(hd.iftype==1, "unexpected file type");
    DATRW_assert(hd.leven==1, "uneven sampling");
    this->setwid2(wid2);
    this->setinfo(info);
    this->settracefree(free);
    this->setlast();

    DATRW_debug(this->Mdebug, "isacstream::readheader()",
                  "finished; return from function");
  }

  /*----------------------------------------------------------------------*/

  void isacstream::help(std::ostream& os)
  { datrw::sac::help(os); }

} // namespace datrw

/* ----- END OF isacstream.cc ----- */
