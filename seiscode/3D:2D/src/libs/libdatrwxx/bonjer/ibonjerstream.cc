/*! \file ibonjerstream.cc
 * \brief read bonjers ASCII data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read bonjers ASCII data (implementation)
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
 *  - 31/10/2010   V1.1   pass station value to station header field
 *  - 23/11/2010   V1.2   introduced static members
 * 
 * ============================================================================
 */
#define DATRW_BONJER_CC_VERSION \
  "DATRW_BONJER_CC   V1.2"

#include <datrwxx/bonjer.h>
#include <datrwxx/readbonjer.h>

namespace datrw {

  const std::ios_base::openmode ibonjerstream::openmode=std::ios_base::in;

  //@{
  /*! \brief Format properties
   * \ingroup group_bonjer
   */
  const bool bonjer::isbinary=false;
  const char* const bonjer::streamID="bonjer";
  //@}

  /*----------------------------------------------------------------------*/

  ibonjerstream::ibonjerstream(std::istream& is): Tbase(is, true, true, true) 
  { }

  namespace helper {

    template<class C>
      C readany(std::istream& is, const int& n)
      {
        C retval(n);
        double inval;
        for (int i=0; i<n; ++i)
        {
          DATRW_assert(is.good(),"ERROR (ibonjerstream::?series): "
                         "bad stream!");
          is >> inval;
          retval(i)=typename C::Tvalue(inval);
        }
        return(retval);
      }

  } // namespace helper

  /*----------------------------------------------------------------------*/

  Tdseries ibonjerstream::dseries()
  {
    readheader();
    return(helper::readany<Tdseries>(Mis, this->wid2().nsamples));
  }

  /*----------------------------------------------------------------------*/

  Tfseries ibonjerstream::fseries()
  {
    readheader();
    return(helper::readany<Tfseries>(Mis, this->wid2().nsamples));
  }

  /*----------------------------------------------------------------------*/

  Tiseries ibonjerstream::iseries()
  {
    readheader();
    return(helper::readany<Tiseries>(Mis, this->wid2().nsamples));
  }

  /*----------------------------------------------------------------------*/

  void ibonjerstream::readheader()
  {
    datrw::bonjer::header hd=datrw::bonjer::readheader(Mis);
    sff::FREE tracefree;
    sff::WID2 wid2;
    this->newtrace();
    wid2.station=hd.station;
    tracefree.append("station: " + hd.station);
    wid2.nsamples=hd.nsamples;
    tracefree.append("file: " + hd.filename);
    wid2.auxid=hd.component;
    wid2.channel=hd.component.substr(4);
    tracefree.append("component: " + hd.component);
    wid2.date=hd.date;
    tracefree.append("date: " + hd.date.timestring());
    wid2.dt=1./hd.rate;
    tracefree.append("sensitivity: " + hd.sensitivity);
    tracefree.append("units: " + hd.units);
    this->setlast();
    this->settracefree(tracefree);
    this->setwid2(wid2);
  }

} // namespace datrw

/* ----- END OF ibonjerstream.cc ----- */
