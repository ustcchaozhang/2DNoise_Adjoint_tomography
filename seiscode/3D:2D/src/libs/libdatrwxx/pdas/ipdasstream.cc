/*! \file ipdasstream.cc
 * \brief read PDAS data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read PDAS data (implementation)
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
 *  - 30/03/2004   V1.0   Thomas Forbriger
 *  - 17/09/2004   V1.1   slightly simplified
 *  - 20/10/2004   V1.2   support scaling for floating point output types
 *  - 23/11/2010   V1.3   introduced static member data
 * 
 * ============================================================================
 */
#define DATRW_PDAS_CC_VERSION \
  "DATRW_PDAS_CC   V1.3"

#include <algorithm>
#include <iterator>
#include <datrwxx/pdas.h>
#include <datrwxx/pdasread.h>
#include <aff/seriesoperators.h>

namespace datrw {

  const std::ios_base::openmode
    ipdasstream::openmode=std::ios_base::in|std::ios_base::binary;

  //@{
  /*! \brief Format properties
   * \ingroup group_pdas
   */
  const bool pdas::isbinary=true;
  const char* const pdas::streamID="pdas";
  //@}

  /*----------------------------------------------------------------------*/

  ipdasstream::ipdasstream(std::istream& is)
    : Tbase(is, true, true, true) 
  { }

  /*----------------------------------------------------------------------*/

  Tdseries ipdasstream::dseries()
  {
    readheader();
    datrw::pdas::Tdata data;
    datrw::pdas::readdata(Mis, data, Mtype);
    sff::WID2 wid2(this->wid2());
    wid2.nsamples=data.size();
    this->setwid2(wid2);
    return(datrw::pdas::convertandscale<Tdseries>(data, Mtype));
  }

  /*----------------------------------------------------------------------*/

  Tfseries ipdasstream::fseries()
  {
    readheader();
    datrw::pdas::Tdata data;
    datrw::pdas::readdata(Mis, data, Mtype);
    sff::WID2 wid2(this->wid2());
    wid2.nsamples=data.size();
    this->setwid2(wid2);
    return(datrw::pdas::convertandscale<Tfseries>(data, Mtype));
  }

  /*----------------------------------------------------------------------*/

  Tiseries ipdasstream::iseries()
  {
    readheader();
    datrw::pdas::Tdata data;
    datrw::pdas::readdata(Mis, data, Mtype);
    sff::WID2 wid2(this->wid2());
    wid2.nsamples=data.size();
    this->setwid2(wid2);
    return(datrw::pdas::convert<Tiseries>(data));
  }

  /*----------------------------------------------------------------------*/

  void ipdasstream::skipseries()
  {
    readheader();
    sff::WID2 wid2(this->wid2());
    wid2.nsamples=datrw::pdas::countdata(Mis, Mtype);
    this->setwid2(wid2);
  }

  /*----------------------------------------------------------------------*/

  void ipdasstream::readheader()
  {
    this->newtrace();
    sff::WID2 wid2;
    sff::FREE tracefree;
    datrw::pdas::Header hd=datrw::pdas::readheader(Mis);
    std::copy(hd.lines.begin(), hd.lines.end(),
              std::back_inserter(tracefree.lines));
    wid2.station=hd.dataset.substr(2,3);
    wid2.channel=hd.dataset.substr(0,2);
    wid2.auxid=hd.signal;
    int ipday=hd.date.find('-');
    int ipyear=hd.date.find('-',1+ipday);
    std::string datestring=hd.date.substr(1+ipyear)
      + '/' + hd.date.substr(0,ipday)
      + '/' + hd.date.substr(1+ipday,ipyear-ipday-1);
    int ipspace=hd.time.find(' ');
    datestring += '_' + hd.time.substr(0,ipspace);
    wid2.date=libtime::TAbsoluteTime(datestring.c_str());
    wid2.dt=std::atof(hd.interval.c_str());
    Mtype=hd.type;
    this->settracefree(tracefree);
    this->setwid2(wid2);
    this->setlast();
  }

  /*----------------------------------------------------------------------*/

  void ipdasstream::help(std::ostream& os)
  { datrw::pdas::help(os); }

} // namespace datrw

/* ----- END OF ipdasstream.cc ----- */
