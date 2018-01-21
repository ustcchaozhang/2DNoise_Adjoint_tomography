/*! \file gsexx_TWID2.cc
 * \brief definition of TWID2 class functions (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 29/03/2002
 * 
 * definition of TWID2 class functions (implementation)
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * libgsexx is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 29/03/2002   V1.0   Thomas Forbriger
 *  - 16/03/2004   V1.1   deal with whitespace in WID2
 *  - 24/07/2006   V1.2   provide millisecond precision
 * 
 * ============================================================================
 */
#define TF_GSEXX_TWID2_CC_VERSION \
  "TF_GSEXX_TWID2_CC   V1.2   "
#define TF_GSEXX_TWID2_CC_CVSID \
  "$Id$"

#include <ctime>
#include <cstdlib>
#include <gsexx.h>
#include <cstdio>

namespace GSE2 {
namespace waveform {

/*! \class TWID2
 *
 * \note
 * The auxid field should only be used if the channel id non-unique.
 */

//! GSE line identifier
const char* const TWID2::GSEID="WID2";
  
//! return subformat string
std::string TWID2::subformat() const
{
  std::string retval("NSP");
  if (Fsubformat == SF_CM6) retval="CM6";
  return(retval);
}
  
//! set subformat from key string
void TWID2::setsubformat(const std::string& key)
{
  if (key.substr(0,3)==std::string("CM6")) { Fsubformat=SF_CM6; }
  else throw
    Terror("ERROR (TWID2): unknown subformat key!");
}
  
//! write the WID2 line
std::string TWID2::line() const
{
  // buffer size
  const int bufsize=135;
  // character buffer
  char charbuf[bufsize];
  std::string datatype=subformat();
  int isec=int(Fmilsec/1000);
  int imsec=Fmilsec-(1000*isec);
  sprintf(charbuf, "%-4s %4.4i/%2.2i/%2.2i %2.2i:%2.2i:%2.2i.%3.3i "
          "%-5s %-3s %-4s %-3s %8i %11.6f %10.2e %7.3f %-6s %5.1f %4.1f\n",
          TWID2::GSEID, Fyear, Fmonth, Fday,
          Fhour, Fminute, isec, imsec,
          Fstation.substr(0,5).c_str(),
          Fchannel.substr(0,3).c_str(),
          Fauxid.substr(0,4).c_str(),
          datatype.substr(0,3).c_str(),
          Fsamps, Fsamprate, Fcalib, Fcalper,
          Finstype.substr(0,6).c_str(),
          Fhang, Fvang);
  std::string retval(charbuf);
  return(retval);
}
  
//! read a WID2 line from a stream
void TWID2::read(std::istream& is)
{
  std::string theline;
  getline(is, theline);

  // check ID
  std::string lineID=theline.substr(0,4);
  if (!GSEIDmatch<TWID2>(lineID)) throw
    Terror("ERROR (TWID2::read): missing WID2 ID!");
  
  Fyear    =atoi(theline.substr( 5,4).c_str());
  Fmonth   =atoi(theline.substr(10,2).c_str());
  Fday     =atoi(theline.substr(13,2).c_str());
  Fhour    =atoi(theline.substr(16,2).c_str());
  Fminute  =atoi(theline.substr(19,2).c_str());
  // Fseconds =atof(theline.substr(22,6).c_str());
  int isec =atoi(theline.substr(22,2).c_str());
  Fmilsec  =atoi(theline.substr(25,3).c_str());
  Fmilsec  += (1000*isec);
  /*
  std::cerr << "TWID2 debug: " 
    << theline.substr(22,6) << " "
    << this->seconds() << " " 
    << isec << " " 
    << Fmilsec 
    << std::endl;
    */
  
  Fstation  =theline.substr(29,5);
  Fchannel  =theline.substr(35,3);
  Fauxid    =theline.substr(39,4);

  this->setsubformat(theline.substr(44,3));

  Fsamps   =atoi(theline.substr(48,8).c_str());

  Fsamprate=atof(theline.substr(57,11).c_str());
  Fcalib   =atof(theline.substr(69,10).c_str());
  Fcalper  =atof(theline.substr(80,7).c_str());
  Finstype  =theline.substr(88,6);
  Fhang    =atof(theline.substr(95,5).c_str());
  Fvang    =atof(theline.substr(101,4).c_str());
}

//! provide seconds in float format
double TWID2::seconds() const
{
  return(double(1.e-3*Fmilsec));
}

//! set seconds from float format
void TWID2::seconds(const double& s)
{
  Fmilsec=int(1000*s);
}
  
//! set the values to defaults
void TWID2::defaults()
{
  std::time_t nowtime=std::time(NULL);
  std::tm *now=std::localtime(&nowtime);
  Fyear=now->tm_year+1900;
  Fmonth=now->tm_mon;
  Fday=now->tm_mday;
  Fhour=now->tm_hour;
  Fminute=now->tm_min;
  // Fseconds=double(now->tm_sec);
  Fmilsec=1000*now->tm_sec;
  Fstation="NSP";
  Fchannel="NSP";
  Fauxid="NSP";
  Fsubformat=SF_CM6;
  Fsamps=-1;
  Fsamprate=-1.;
  Fcalib=-1.;
  Fcalper=-1.;
  Finstype="NSP";
  Fhang=-1.;
  Fvang=-1.;
}

/*----------------------------------------------------------------------*/

} // namespace waveform
} // namespace GSE2

/* ----- END OF gsexx_TWID2.cc ----- */
