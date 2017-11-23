/*! \file osffstream.cc
 * \brief wrapper around SFF output stream (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/04/2006
 * 
 * wrapper around SFF output stream (implementation)
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 11/04/2006   V1.0   Thomas Forbriger
 *  - 17.12.2010   V1.1   
 *                        - bug fix: INFO data was not passed
 *                        - declare SFF data type as single precision
 *                        - declare GSE data type as integer
 *  - 07/06/2011   V1.2   promise constness of series samples
 *  - 21/11/2011   V1.3   introduce format modifiers
 *  - 18/11/2016   V1.4   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_OSFF_CC_VERSION \
  "DATRW_OSFF_CC   V1.4"

#include <datrwxx/sff.h>
#include <datrwxx/debug.h>
#include <datrwxx/sffwriters.h>
#include <datrwxx/formatmodifier.h>

namespace datrw {

  const std::ios_base::openmode osffstream::openmode=std::ios_base::out;

  /*----------------------------------------------------------------------*/

  osffstream::osffstream(std::ostream& os,
                         const std::string& modifier,
                         const bool& debug):
    Tbase(os, Ffloat, true, true, true, true, debug), 
    Mwid2iswaiting(false),
    Mfreeiswaiting(false),
    Minfoiswaiting(false),
    Mnormmode(::sff::NM_maxdyn)
  {
    Mdebug=debug;
    datrw::Subformat subformat(modifier);
    if (subformat.isset("dontscale")) 
    { Mnormmode=::sff::NM_one; }
    else if (subformat.isset("scaleifneeded")) 
    { Mnormmode=::sff::NM_ifneeded; }
    DATRW_assert_modifiers_are_recognized(subformat,
                                          "osffstream");
    DATRW_debug(Mdebug, "osffstream::osffstream",
                   "new instance established");
  } // osffstream::osffstream(std::ostream& os, const bool& debug)

  /*----------------------------------------------------------------------*/

  void osffstream::help(std::ostream& os)
  {
    os <<
      std::endl <<
      "SFF writing functions" << std::endl <<
      "---------------------" << std::endl <<
      DATRW_OSFF_CC_VERSION << std::endl <<
      std::endl;
    os << 
      "This module writes SFF data. SFF (Stuttgart File Format) is\n"
      "based on GSE subformat CM6. GSE is an integer format with\n"
      "a dynamic range of little more than 27 bits. Second differences\n"
      "are saved such that in the worst case two bits are consumed by the\n"
      "second differences and one additional bit for the sign. The\n"
      "underlying libgsexx aborts if numbers larger than 2**26-1 are\n"
      "to be written. For this reason floating point data must be\n"
      "scaled appropriately before being written to SFF data. The\n"
      "default is to scale to maximum dynamic range (i.e. 2**26-1).\n"
      "Two other scaling options are available through format modifiers:"
      << std::endl;
    os <<
      "dontscale        Write data as is. This is appropriate for integer\n"
      "                 data in particlular.\n"
      "scaleifneeded    Scale only if largest value in time series is\n"
      "                 larger than 2**26-1. Sample values smaller than\n"
      "                 will be truncated to zero silently."
      << std::endl;
  } // void osffstream::help(std::ostream& os=std::cout)

  /*----------------------------------------------------------------------*/

  void osffstream::writefileheader()
  {
    DATRW_debug(Mdebug, "osffstream::writefileheader",
                   "write file header to file");
    ::sff::FileHeader fileheader;
    if (this->hassrce()) { fileheader.setsrce(this->srce()); }
    if (this->hasfree()) { fileheader.setfree(this->free()); }
    Mos << fileheader;
  } // void osffstream::writefileheader()

  /*----------------------------------------------------------------------*/

  void osffstream::writetrace(const Tdseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "osffstream::writetrace",
                   "write series of type double to file");
    this->flushwaitingtrace();
    Mserieswaiting=datrw::util::seriesreservoir(series);
  } // void osffstream::writetrace(const Tdseries& series)

  /*----------------------------------------------------------------------*/

  void osffstream::writetrace(const Tfseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "osffstream::writetrace",
                   "write series of type float to file");
    this->flushwaitingtrace();
    Mserieswaiting=datrw::util::seriesreservoir(series);
  } // void osffstream::writetrace(const Tfseries& series)

  /*----------------------------------------------------------------------*/

  void osffstream::writetrace(const Tiseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "osffstream::writetrace",
                   "write series of type integer to file");
    this->flushwaitingtrace();
    Mserieswaiting=datrw::util::seriesreservoir(series);
  } // void osffstream::writetrace(const Tiseries& series)

  /*----------------------------------------------------------------------*/

  void osffstream::flushwaitingtrace(const bool& last)
  {
    DATRW_debug(Mdebug, "osffstream::flushwaitingtrace",
                   "flush previous trace data to file");
    // flush previous trace
    if (Mwid2iswaiting)
    {
      // prepare trace header
      ::sff::TraceHeader traceheader(Mwid2waiting, last);
      if (Mfreeiswaiting) { traceheader.setfree(Mfreewaiting); }
      if (Minfoiswaiting) { traceheader.setinfo(Minfowaiting); }
      datrw::sff::writesfftrace(Mos,
                                   traceheader,
                                   Mserieswaiting,
                                   Mnormmode);
    }
    else
    {
      DATRW_assert((!last), 
                      "file is to be closed and no data was written");
    }
    // check for current trace headers and store them
    if (!last)
    {
      DATRW_assert(this->haswid2(), "missing trace header data");
      Mwid2iswaiting=true;
      Mwid2waiting=this->wid2();
      Mfreeiswaiting=false;
      if (this->hasfree()) { Mfreeiswaiting=true; Mfreewaiting=this->free(); }
      Minfoiswaiting=false;
      if (this->hasinfo()) { Minfoiswaiting=true; Minfowaiting=this->info(); }
    }
  }

  /*======================================================================*/

  ogsestream::ogsestream(std::ostream& os, const bool& debug):
    Tbase(os, "", debug)
  {
    Mnormmode=::sff::NM_one;
    DATRW_debug(Mdebug, "ogsestream::ogsestream",
                   "new instance established");
    this->setdatatype(Fint);
  } // ogsestream::ogsestream(std::ostream& os, const bool& debug)

  /*----------------------------------------------------------------------*/

  void ogsestream::help(std::ostream& os)
  {
    os << "This module writes standard GSE data." << std::endl;
    os << "Essentially SFF data is written but without normalising"
      << std::endl;
    os << "them. Notice that floating point data will by truncated to"
      << std::endl;
    os << "integers." << std::endl;
  } // void ogsestream::help(std::ostream& os=std::cout)

} // namespace datrw

/* ----- END OF osffstream.cc ----- */
