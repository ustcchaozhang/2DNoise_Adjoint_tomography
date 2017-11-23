/*! \file osustream.cc
 * \brief SeismicUnix output stream (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 03/12/2010
 * 
 * SeismicUnix output stream (implementation)
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
 * REVISIONS and CHANGES 
 *  - 03/12/2010   V1.0   Thomas Forbriger
 *  - 07/06/2011   V1.1   promise constness of series samples
 *  - 21/01/2012   V1.2   
 *                        - prepared osustream to take modifiers
 *                        - use SUHeaderControl to store format modifier values
 *  - 24/01/2012   V1.3   output modifiers are evaluated in dedicated function
 *  - 18/11/2016   V1.4   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_OSUSTREAM_CC_VERSION \
  "DATRW_OSUSTREAM_CC   V1.4"

#include <datrwxx/su.h>
#include <datrwxx/error.h>
#include <datrwxx/debug.h>
#include <datrwxx/util.h>
#include <datrwxx/suformat.h>
#include <datrwxx/sucomanager.h>
#include <datrwxx/formatmodifier.h>

namespace datrw {

  const std::ios_base::openmode 
    osustream::openmode=std::ios_base::out|std::ios_base::binary;

  /*----------------------------------------------------------------------*/

  osustream::osustream(std::ostream& os, 
                       const std::string& modifier,
                       const bool& debug):
    Tbase(os, Ffloat, false, false, true, true, debug), 
    Mitrace(0)
  {
    DATRW_debug(Mdebug, "osustream::osustream",
                        "new instance established");
    // evaluate format modifiers
    Mheadercontrol=su::outputmodifiers(modifier, Mdebug);
  } // osustream::osustream(std::ostream& os, const bool& debug)

  /*----------------------------------------------------------------------*/

  void osustream::help(std::ostream& os)
  {
    os <<
      std::endl <<
      "SeismicUn*x writing functions" << std::endl <<
      "-----------------------------" << std::endl <<
      DATRW_OSUSTREAM_CC_VERSION << std::endl <<
      std::endl <<
      "This module provides writing of SeismicUn*x binary data files."
      << std::endl;
    os << std::endl;
    datrw::su::SUheader::help(os);
    os << std::endl;
    os <<
      "Valid format modifiers are:\n";
    formatmodifiers::ModifierHelp mh(os, 14);    
    mh(su::subformat::key::forceultrasonic) <<
              "Force writing of ultrasonic data header\n";
      mh() << "The default is to write standard SeismicUn*x\n";
      mh() << "headers for seismic sampling intervals.\n";
    mh(su::subformat::key::forceseismic) <<
              "Force writing of seismic data header even\n";
      mh() << "is sampling interval is too small.\n";
      mh() << "Choosing both modifiers at once is an\n";
      mh() << "inconsistency and causes abort().\n";
    mh(su::subformat::key::scalco, "s") <<
              "\"s\" is used as the preferred scalco and scalel value.\n";
      mh() << "It is used only if it does not cause round-off truncation.\n";
      mh() << "The default is " 
           << su::subformat::key::scalco << "=" 
           << datrw::su::subformat::def::scalco << "\n";
    mh(su::subformat::key::coodigits, "n") <<
              "Limits the maximum number of significant digits in\n";
      mh() << "spatial coordinates to \"n\". Coordinate values with\n";
      mh() << "more digits will be truncated.\n";
      mh() << "The default is " 
           << su::subformat::key::coodigits << "=" 
           << datrw::su::subformat::def::coodigits << "\n";
  } // void osustream::help(std::ostream& os=std::cout)

  /*----------------------------------------------------------------------*/

  /*! \brief Should write file header, but SU does not have one.
   *
   * Silently return from this function.
   * The Seismic Un*x format does not have a file header.
   */
  void osustream::writefileheader()
  {
    DATRW_debug(Mdebug, "osustream::writefileheader()",
                "empty function; must be provided for consistent interface");
  } // void osustream::writefileheader()

  /*----------------------------------------------------------------------*/

  //! write double data by passing samples to float function
  void osustream::writetrace(const Tdseries::Tcoc& series)
  {
    this->writetrace(::datrw::util::convert<Tdseries::Tcoc,Tfseries>(series));
  } // void osustream::writetrace(const Tdseries& series)

  /*----------------------------------------------------------------------*/

  //! write int data by passing samples to float function
  void osustream::writetrace(const Tiseries::Tcoc& series)
  {
    this->writetrace(::datrw::util::convert<Tiseries::Tcoc,Tfseries>(series));
  } // void osustream::writetrace(const Tiseries& series)

  /*----------------------------------------------------------------------*/

  //! actually write trace data
  void osustream::writetrace(const Tfseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "osustream::writetrace(const Tfseries& series)",
                "actually write trace");
    ++Mitrace;
    ::datrw::su::SUheader header(Mheadercontrol, Mdebug);
    ::sff::WID2 wid2=this->wid2();
    wid2.nsamples=series.size();
    header.set(wid2);
    ::sff::SRCE srce=this->srce();
    if (!this->hassrce())
    {
      // prepare SRCE data from wid2 data
      srce.date=wid2.date;
    }
    ::sff::INFO info=this->info();
    if (!this->hasinfo())
    {
      // prepare INFO data from 
      info.cx=static_cast<double>(Mitrace);
    }
    header.set(srce);
    header.set(info);
    header.Mheader.tracr=Mitrace;
    header.write(Mos);
    // write samples to file
    Mos.write(reinterpret_cast<const char *>(series.pointer()), 
              series.size()*sizeof(Tfseries::Tvalue));
    DATRW_debug(Mdebug, "osustream::writetrace(const Tfseries& series)",
                "wrote " << series.size() << " samples of "
                << sizeof(Tfseries::Tvalue) << " bytes size; "
                << "Mos.good() returns " << Mos.good());
  } // void osustream::writetrace(const Tfseries& series)

} // namespace datrw

/* ----- END OF osustream.cc ----- */
