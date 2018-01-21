/*! \file oseifestream.cc
 * \brief writing seife data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/11/2010
 * 
 * writing seife data (implementation)
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
 *  - 08/07/2016   V1.1   make correct use of new DATRW_report_assert
 *  - 18/11/2016   V1.2   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_OSEIFESTREAM_CC_VERSION \
  "DATRW_OSEIFESTREAM_CC   V1.2"

#include <datrwxx/seife.h>
#include <datrwxx/seifeio.h>
#include <datrwxx/debug.h>
#include <datrwxx/util.h>
#include <datrwxx/formatmodifier.h>

namespace datrw {

  const std::ios_base::openmode 
    oseifestream::openmode=std::ios_base::out;

  /*----------------------------------------------------------------------*/

  oseifestream::oseifestream(std::ostream& os, 
                             const std::string& modifier,
                             const bool& debug):
    Tbase(os, Fdouble, false, true, false, false, debug),
    Mtracewritten(false), Mmodifier(modifier)
  {
    DATRW_debug(Mdebug, "oseifestream::oseifestream",
                        "new instance established");
    DATRW_expect_no_modifier(Fseife,modifier);
  } // oseifestream::oseifestream

  /*----------------------------------------------------------------------*/

  void oseifestream::writefileheader()
  {
    DATRW_debug(Mdebug, "oseifestream::writefileheader",
                        "write file header to file");
    // nothing to be done in the case of a seife file
  } // void oseifestream::writefileheader()

  /*----------------------------------------------------------------------*/

  void oseifestream::writetrace(const Tdseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "oseifestream::writetrace",
                   "write series of type double to file");
    this->writetraceheader(series.size());
    datrw::seife::write_series(this->Mos, series);
  } // void oseifestream::writetrace(const Tdseries& series)

  /*----------------------------------------------------------------------*/

  void oseifestream::writetrace(const Tfseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "oseifestream::writetrace",
                   "write series of type float to file");
    this->writetraceheader(series.size());
    datrw::seife::write_series(this->Mos, 
                          datrw::util::convert<Tfseries, Tdseries>(series));
  } // void oseifestream::writetrace(const Tfseries& series)

  /*----------------------------------------------------------------------*/

  void oseifestream::writetraceheader(const unsigned int& n) 
  {
    DATRW_report_assert(!this->Mtracewritten,
                        "WARNING in (oseifestream::writetraceheader):\n"
                        "More than one trace will be written to the same "
                        "seife file!\n"
                        "seife format only supports single trace files.\n");
    datrw::seife::Header header;
    datrw::seife::ParameterLine parameters;
    if (this->hasfree()) { header.set(this->free()); }
    ::sff::WID2 wid2=this->wid2();
    libtime::TAbsoluteTime theday(wid2.date.year(), wid2.date.month(),
                                  wid2.date.day());
    parameters.time(wid2.date-theday);
    parameters.dt(wid2.dt);
    parameters.nsamples(n);
    parameters.format(datrw::seife::seife_standard_format);
    header.set(parameters);
    header.write(this->Mos);
    this->Mtracewritten=true;
  } // void oseifestream::writetraceheader(const unsigned int& n)

  /*----------------------------------------------------------------------*/

  void oseifestream::help(std::ostream& os)
  { 
    os <<
      std::endl <<
      "seife writing functions" << std::endl <<
      "-----------------------" << std::endl <<
      DATRW_OSEIFESTREAM_CC_VERSION << std::endl <<
      std::endl;
    os << 
      "This module writes seismic time series data in the format\n"
      "used by programs provided by Erhard Wielandt. See his web site\n"
      "http://www.software-for-seismometry.de for details.\n"
      "Notice that this format only supports a single trace per file.\n"
      "The actual number format currently is fixed to the Fortran format\n"
      "specification " << seife::seife_standard_format << "."
      << std::endl;
  } // void oseifestream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF oseifestream.cc ----- */
