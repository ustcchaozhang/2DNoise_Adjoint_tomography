/*! \file ithiesdl1stream.cc
 * \brief ThiesDL1 input stream (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * ThiesDL1 input stream (implementation)
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
 *  - 13/09/2011   V1.0   Thomas Forbriger
 *  - 18/11/2016   V1.1   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_ITHIESDL1STREAM_CC_VERSION \
  "DATRW_ITHIESDL1STREAM_CC   V1.1"

#include <datrwxx/thiesdl1.h>
#include <datrwxx/formatmodifier.h>

namespace datrw {

  const std::ios_base::openmode
    ithiesdl1stream::openmode=std::ios_base::in;

  /*----------------------------------------------------------------------*/

  ithiesdl1stream::ithiesdl1stream(std::istream& is, 
                                   const std::string& modifier,
                                   const bool& debug):
    Tbase(is, true, true, true, debug), Mmodifier(modifier)
  { 
    this->Mheader=thiesdl1::readheader(Mis);
    this->setfilefree(this->Mheader.lines);
    datrw::Subformat subformat(Mmodifier);
    this->Mfile.tolerateredundant(subformat.isset("tr"));
    this->Mfile.toleratewrongtime(subformat.isset("twt"));
    subformat.notchecked(std::cerr);
  } // ithiesdl1stream::ithiesdl1stream

  /*----------------------------------------------------------------------*/

  void ithiesdl1stream::settraceheader()
  { 
    this->newtrace();
    this->setwid2(this->Mheader.wid2line());
  } // void ithiesdl1stream::settraceheader()

  /*----------------------------------------------------------------------*/

  void ithiesdl1stream::readsamples()
  {
    this->Mfile.read(Mis, this->Mheader);
    this->settracefree(this->Mfile.tracefree());
  }

  /*----------------------------------------------------------------------*/

  Tdseries ithiesdl1stream::dseries()
  {
    this->settraceheader();
    this->readsamples();
    this->setlast();
    return(this->Mfile.dseries());
  } // Tdseries ithiesdl1stream::dseries()

  /*----------------------------------------------------------------------*/

  Tfseries ithiesdl1stream::fseries()
  {
    this->settraceheader();
    this->readsamples();
    this->setlast();
    return(this->Mfile.fseries());
  } // Tfseries ithiesdl1stream::fseries()

  /*----------------------------------------------------------------------*/

  Tiseries ithiesdl1stream::iseries()
  {
    this->settraceheader();
    this->readsamples();
    this->setlast();
    return(this->Mfile.iseries());
  } // Tfseries ithiesdl1stream::iseries()

  /*----------------------------------------------------------------------*/

  void ithiesdl1stream::skipseries()
  {
    this->settraceheader();
    this->setlast();
  } // void ithiesdl1stream::skipseries()

  /*----------------------------------------------------------------------*/

  void ithiesdl1stream::help(std::ostream& os)
  { 
    os <<
      std::endl <<
      "Thies DL1 reading functions" << std::endl <<
      "---------------------------" << std::endl <<
      DATRW_ITHIESDL1STREAM_CC_VERSION << std::endl <<
      std::endl;
    os << 
     "This file format is used to store data of the Thies DL1/N pluviometer\n"
     "at BFO. The files are produced by program DL1logger, which accomplishes\n"
     "data acquisition by controlling the Thies DL1/N data logger and reading\n"
     "the data from the logger.\n"
     "See also:\n"
     "http://www.rz.uni-karlsruhe.de/~bi77/public/paperware/linked/technotes/DL1recording.pdf\n"
      << std::endl;
    os << 
     "The Thies DL1 input stream can be controlled by format modifiers:\n"
     "tr   input stream tolerates redundant samples\n"
     "twt  input stream tolerates samples not fitting in expected time window\n"
     << std::endl;
  } // void ithiesdl1stream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF ithiesdl1stream.cc ----- */
