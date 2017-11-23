/*! \file wid2container.cc
 * \brief container for WID2 data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2014
 * 
 * container for WID2 data (implementation)
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
 *  - 05/07/2014   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_WID2CONTAINER_CC_VERSION \
  "TF_WID2CONTAINER_CC   V1.0   "

#include <cstring>
#include <fapidxx/helper.h>
#include <fapidxx/wid2container.h>
#include <fapidxx/error.h>

namespace fapidxx {

  /*! \brief set WID2 data from a string
   *
   * This function sets the data in WID2container::wid2 from an input
   * character string.
   *
   * \note
   * This function does NOT decode the special WID2 string format.
   *
   * \param line WID2 line character sequence (input value)
   */
  void WID2container::setwid2(const std::string& line)
  {
    std::istringstream is(line);
    this->wid2.read(is);
    this->wasascii=true;
  } // void WID2container::setwid2(const std::string& line)

  /*----------------------------------------------------------------------*/

  /*! \brief set WID2 data from a Fortran character sequence
   *
   * This function sets the data in WID2container::wid2 from an input
   * character sequence.
   * If encode uses a binary form to encode WID2 data, this function has to
   * distinguish between both possible representations of WID2 data.
   * This is not yet implemented.
   *
   * \param fstring WID2 line character squence to set WID2 data in
   *        WID2container::wid2 (input value of function)
   * \param slen length of character array fstring
   */
  void WID2container::setwid2(char *fstring, ftnlen slen)
  {
    /*
     * copy content immediately such that we do not rely on the ID string
     * being placed at byte offset 0 in WID2container::WID2struct
     */
    int w2size=sizeof(WID2container::WID2struct);
    FAPIDXX_assert(w2size<=slen,
                   "WID2container::setwid2: string is too short");
    WID2container::WID2struct* ws=new WID2container::WID2struct;
    std::memcpy(ws, fstring, w2size);
    // check for special ID, expect ASCII encoding if not ID does not match
    if (std::strncmp(ws->ID, WID2container::WID2struct::encodeID, 
                     WID2container::WID2struct::idlen)!=0)
    {
      this->setwid2(stringfromfstring(fstring, slen));
      this->wasascii=true;
    }
    else
    {
      /*
       * The byte sequence fstring points to is the content of a
       * WID2container::WID2struct struct. Create a WID2container::WID2struct
       * in memory, copy the memory contents fstring points to into
       * this struct and read the WID2 data from it.
       */
      this->wid2=ws->get();
      this->wasascii=false;
    }
    delete ws;
  } // void WID2container::setwid2(char *fstring, ftnlen slen)

  /*----------------------------------------------------------------------*/

  /*! \brief encode WID2 data in character sequence
   *
   * This function is prepared to encode WID2 data in a character sequence.
   * Whether binary or ascii encoding is used, is determined from the
   * WID2container::wasascii flag.
   *
   * \param fstring WID2 line character sequence produced from WID2 data
   *        in member data WID2container::wid2; this is the return value of
   *        the function (output value)
   * \param slen length of character array fstring
   */
  void WID2container::encode(char *fstring, ftnlen slen) const
  {
    if (this->wasascii)
    {
      this->encodeascii(fstring, slen);
    }
    else
    {
      this->encodebinary(fstring, slen);
    }
  } // void WID2container::encode(char *fstring, ftnlen slen)

  /*----------------------------------------------------------------------*/

  /*! \brief binary encode WID2 data in character sequence
   *
   * This function is prepared to encode WID2 data in binary form
   * in a character sequence. 
   *
   * \param fstring WID2 line character sequence produced from WID2 data
   *        in member data WID2container::wid2; this is the return value of
   *        the function (output value)
   * \param slen length of character array fstring
   */
  void WID2container::encodebinary(char *fstring, ftnlen slen) const
  {
    WID2container::WID2struct ws;
    ws.set(this->wid2);
    int w2size=sizeof(WID2container::WID2struct);
    FAPIDXX_assert(w2size<=slen,
                   "WID2container::encode: struct is too large");
    std::memcpy(fstring, &ws, w2size);
  } // void WID2container::encodebinary(char *fstring, ftnlen slen)

  /*----------------------------------------------------------------------*/

  /*! \brief ascii encode WID2 data in character sequence
   *
   * This function is prepared to encode WID2 data in standard SFF ascii
   * encoding in a character sequence.
   *
   * \param fstring WID2 line character sequence produced from WID2 data
   *        in member data WID2container::wid2; this is the return value of
   *        the function (output value)
   * \param slen length of character array fstring
   */
  void WID2container::encodeascii(char *fstring, ftnlen slen) const
  {
    fapidxx::fillfstring(this->wid2.line(), fstring, slen);
  } // void WID2container::encodeascii(char *fstring, ftnlen slen)

  /*======================================================================*/
  // WID2contsiner::WID2struct

  /*! \brief ID indicating the WID2 is encoded in binary form in the WID2 line
   */
  const char* WID2container::WID2struct::encodeID="WIDY";

  /*----------------------------------------------------------------------*/

  void WID2container::WID2struct::set(const ::sff::WID2& wid2)
  {
    strncpy(this->ID, WID2container::WID2struct::encodeID, idlen);
    strncpy(this->station, wid2.station.c_str(), slen);
    strncpy(this->channel, wid2.channel.c_str(), clen);
    strncpy(this->auxid, wid2.auxid.c_str(), alen);
    strncpy(this->instype, wid2.instype.c_str(), ilen);
    this->station[slen]='\0';
    this->channel[clen]='\0';
    this->auxid[alen]='\0';
    this->instype[ilen]='\0';
    this->hour=wid2.date.hour();
    this->minute=wid2.date.minute();
    this->second=wid2.date.second();
    this->milsec=wid2.date.milsec();
    this->micsec=wid2.date.micsec();
    this->year=wid2.date.year();
    this->month=wid2.date.month();
    this->day=wid2.date.day();
    this->calib=wid2.calib;
    this->calper=wid2.calper;
    this->dt=wid2.dt;
    this->nsamples=wid2.nsamples;
    this->vang=wid2.vang;
    this->hang=wid2.hang;
  } // void WID2container::WID2struct::set(const ::sff::WID2& wid2)

  /*----------------------------------------------------------------------*/

  ::sff::WID2 WID2container::WID2struct::get() const
  {
    ::sff::WID2 retval;
    libtime::TAbsoluteTime date(this->year, this->month, this->day,
                                this->hour, this->minute, this->second,
                                this->milsec, this->micsec);
    retval.date=date;
    retval.station=std::string(this->station);
    retval.channel=std::string(this->channel);
    retval.auxid=std::string(this->auxid);
    retval.instype=std::string(this->instype);
    retval.dt=this->dt;
    retval.nsamples=this->nsamples;
    retval.hang=this->hang;
    retval.vang=this->vang;
    retval.calib=this->calib;
    retval.calper=this->calper;
    return(retval);
  } // ::sff::WID2 WID2container::WID2struct::get() const

} // namespace fapidxx

/* ----- END OF wid2container.cc ----- */
