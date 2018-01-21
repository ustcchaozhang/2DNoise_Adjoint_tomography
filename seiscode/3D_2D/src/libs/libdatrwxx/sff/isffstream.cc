/*! \file isffstream.cc
 * \brief read sff data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read sff data (implementation)
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
 *  - 12/06/2004   V1.1   provides DEBUG output
 *  - 29/06/2007   V1.2   read SRCE line prior to file FREE block
 *  - 03/07/2007   V1.3   read FREE block prior to file SRCE line
 *                        order of elements in file header should not
 *                        be defined by the order of the code characters
 *                        since reading functions in libsffxx do not
 *                        respect this order. libstuff always writes the FREE
 *                        block first in sff_WOpenFS
 *  - 23/11/2010   V1.4   introduced static member data
 *  - 18/11/2016   V1.5   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_ISFF_CC_VERSION \
  "DATRW_ISFF_CC   V1.5"

#include <datrwxx/sff.h>
#include <sffxx.h>

namespace datrw {

  const std::ios_base::openmode isffstream::openmode=std::ios_base::in;

  //@{
  /*! \brief Format properties
   * \ingroup group_sff
   */
  const bool sff::isbinary=false;
  const char* const sff::streamID="sff";
  //@}

  /*----------------------------------------------------------------------*/

  isffstream::isffstream(std::istream& is, const bool& debug):
    Tbase(is, true, true, true, debug)
  { 
    ::sff::FileHeader fileheader(Mis, debug);
    if (fileheader.hasfree()) { this->setfilefree(fileheader.free()); }
    if (fileheader.hassrce()) { this->setsrce(fileheader.srce()); }
  }

  /*----------------------------------------------------------------------*/

  Tdseries isffstream::dseries()
  {
    this->newtrace();
    if (Mdebug) 
    { 
      std::cerr 
      << "DEBUG (isffstream::dseries): read waveform" 
      << std::endl;
    }
    ::sff::InputWaveform<Tdseries> waveform(Mis);
    this->settraceheader(waveform.header());
    if (Mdebug) 
    { std::cerr << "DEBUG (isffstream::dseries): done" << std::endl; }
    return(waveform.series());
  }

  /*----------------------------------------------------------------------*/

  Tfseries isffstream::fseries()
  {
    this->newtrace();
    if (Mdebug) 
    { 
      std::cerr 
      << "DEBUG (isffstream::fseries): read waveform" 
      << std::endl;
    }
    ::sff::InputWaveform<Tfseries> waveform(Mis);
    this->settraceheader(waveform.header());
    if (Mdebug) 
    { std::cerr << "DEBUG (isffstream::fseries): done" << std::endl; }
    return(waveform.series());
  }

  /*----------------------------------------------------------------------*/

  Tiseries isffstream::iseries()
  {
    this->newtrace();
    if (Mdebug)
    { 
      std::cerr 
      << "DEBUG (isffstream::iseries): read waveform" 
      << std::endl;
    }
    ::sff::InputWaveform<Tiseries> waveform(Mis);
    this->settraceheader(waveform.header());
    if (Mdebug) 
    { std::cerr << "DEBUG (isffstream::iseries): done" << std::endl; }
    return(waveform.series());
  }

  /*----------------------------------------------------------------------*/

  void isffstream::skipseries()
  {
    this->newtrace();
    if (Mdebug)
    { 
      std::cerr 
      << "DEBUG (isffstream::skipseries): skipped waveform" 
      << std::endl;
    }
    ::sff::SkipWaveform skip(Mis);
    if (Mdebug) 
    { std::cerr << "DEBUG (isffstream::skipseries): done" << std::endl; }
    this->settraceheader(skip.header());
  }

  /*----------------------------------------------------------------------*/

  void isffstream::settraceheader(const ::sff::TraceHeader& header)
  {
    this->setwid2(header.wid2());
    if (header.hasfree()) { this->settracefree(header.free()); }
    if (header.hasinfo()) { this->setinfo(header.info()); }
    if (header.last()) { this->setlast(); }
  }

} // namespace datrw

/* ----- END OF isffstream.cc ----- */
