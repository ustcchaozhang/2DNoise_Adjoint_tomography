/*! \file iseifestream.cc
 * \brief reading seife data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/11/2010
 * 
 * reading seife data (implementation)
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
 *  - 30/11/2010   V1.0   Thomas Forbriger
 *  - 18/11/2016   V1.1   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_ISEIFESTREAM_CC_VERSION \
  "DATRW_ISEIFESTREAM_CC   V1.1"

#include <datrwxx/seife.h>
#include <datrwxx/seifeio.h>
#include <datrwxx/util.h>
#include <datrwxx/formatmodifier.h>

namespace datrw {

  const std::ios_base::openmode
    iseifestream::openmode=std::ios_base::in;

  /*----------------------------------------------------------------------*/

  iseifestream::iseifestream(std::istream& is, 
                             const std::string& modifier,
                             const bool& debug):
    Tbase(is, true, true, false, debug), Mmodifier(modifier)
  { 
  } // iseifestream::iseifestream

  /*----------------------------------------------------------------------*/

  void iseifestream::settraceheader()
  { 
    this->newtrace();
    datrw::Subformat subformat(Mmodifier);
    datrw::seife::Header header(Mis); 
    ::sff::FREE free=header.comments();
    datrw::seife::ParameterLine pl=header.parameters(); 
    free.append("input format: "+pl.format());
    ::sff::WID2 wid2;
    wid2.dt=pl.dt();
    libtime::TAbsoluteTime date(subformat.value("date","2000/1/1"));
    wid2.date=libtime::TAbsoluteTime(date.year(), date.month(), date.day());
    wid2.date+=pl.time();
    wid2.nsamples=pl.nsamples();
    wid2.station=subformat.value("station","NSP");
    wid2.channel=subformat.value("channel","NSP");
    wid2.auxid=subformat.value("auxid","NSP");
    wid2.instype=subformat.value("instype","NSP");
    this->settracefree(free);
    this->setwid2(wid2);
    // check for unrecognized modifiers since this is the only place where
    // modifiers are evaluated in this stream
    DATRW_assert_modifiers_are_recognized(subformat, 
                                          "iseifestream::settraceheader()");
  } // void iseifestream::settraceheader()

  /*----------------------------------------------------------------------*/

  Tdseries iseifestream::dseries()
  {
    this->settraceheader();
    this->setlast();
    return(datrw::util::readasciidouble<datrw::Tdseries>(Mis,
                                                    this->wid2().nsamples,
                                                    "iseifestream"));
  } // Tdseries iseifestream::dseries()

  /*----------------------------------------------------------------------*/

  Tfseries iseifestream::fseries()
  {
    this->settraceheader();
    this->setlast();
    return(datrw::util::readasciidouble<datrw::Tfseries>(Mis,
                                                    this->wid2().nsamples,
                                                    "iseifestream"));
  } // Tfseries iseifestream::fseries()

  /*----------------------------------------------------------------------*/

  void iseifestream::skipseries()
  {
    this->settraceheader();
    this->setlast();
  } // void iseifestream::skipseries()

  /*----------------------------------------------------------------------*/

  void iseifestream::help(std::ostream& os)
  { 
    os <<
      std::endl <<
      "seife reading functions" << std::endl <<
      "-----------------------" << std::endl <<
      DATRW_ISEIFESTREAM_CC_VERSION << std::endl <<
      std::endl;
    os << 
      "This module reads seismic time series data in the format\n"
      "used by programs provided by Erhard Wielandt. See his web site\n"
      "http://www.software-for-seismometry.de for details.\n"
      "In its current version, the input stream does not evaluate the\n"
      "Fortran format specification in the file header. It simply expects\n"
      "a sequence of the announced number of floating point values\n"
      "separated by whitespace."
      << std::endl;
    os << 
      "The seife input stream can be controlled by format modifiers:\n"
      "date=2012/7/2   sets the recording date to 2.7.2012\n"
      "station=BFO     sets the recording station to BFO\n"
      "channel=LXX     sets the recording channel to LXX\n"
      "auxid=LOC0      sets the recording auxid to LOC0\n"
      "instype=STS2    sets the recording instrument type to STS2\n"
      "Choose appropriate parameter values for your case."
      << std::endl;
  } // void iseifestream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF iseifestream.cc ----- */
