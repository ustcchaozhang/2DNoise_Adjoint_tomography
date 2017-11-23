/*! \file datrw.cc
 * \brief generic interface definition (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/04/2006
 * 
 * generic interface definition (implementation)
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
 *  - 01/04/2011   V1.1   do not clear Mhassrce upon writing the file header
 *  - 07/06/2011   V1.2   promise constness of series samples
 *  - 08/07/2016   V1.3   make correct use of new DATRW_report_assert
 *  - 18/11/2016   V1.4   provide debug flag in base class
 *                        and produce debug output
 * 
 * ============================================================================
 */
#define DATRW_DATWRITE_CC_VERSION \
  "DATRW_DATWRITE_CC   V1.4"

#include<fstream>
#include <datrwxx/datwrite.h>
#include <datrwxx/debug.h>

namespace datrw {

  void abort_if_exists(const std::string& filename)
  {
    std::ifstream file(filename.c_str(),std::ios_base::in);
    DATRW_assert((!file.good()),"ERROR: file exists!");
  }

  /*----------------------------------------------------------------------*/

  /*! \brief output stream base class constructor, has to be called from
   * derived class.
   *
   * \param os C++ output file stream; all data will be written to this stream
   * \param datatype indicates the value type used for storing sample values
   *                 in the file; if the file format uses integer values to
   *                 store the data, round-off errors will happen, if float
   *                 data is stored in this format; this parameter is stored
   *                 and returned to the user upon request through
   *                 datrw::odatstream::seriestype()
   * \param handlesfilefree \c true if the format can store the data in a FREE
   *                 block for a data file (i.e. a free format string file
   *                 header); this parameter is stored and returned to the
   *                 user upon request through
   *                 datrw::odatstream::handlesfilefree()
   * \param handlestracefree \c true if the format can store the data in a FREE
   *                 block for a data trace (i.e. a free format string trace
   *                 header); this parameter is stored and returned to the
   *                 user upon request through
   *                 datrw::odatstream::handlestracefree()
   * \param handlessrce \c true if the format can store SRCE data which
   *                 defines a seismic source; this parameter is stored and
   *                 returned to the user upon request through
   *                 datrw::odatstream::handlessrce()
   * \param handlesinfo \c true if the format can store INFO data which
   *                 defines receiver properties; this parameter is stored and
   *                 returned to the user upon request through
   *                 datrw::odatstream::handlesinfo()
   * \param debug    set debug mode on a base class level
   */
  odatstream::odatstream(std::ostream& os,
                         const Edatatype& datatype,
                         const bool& handlesfilefree,
                         const bool& handlestracefree,
                         const bool& handlessrce,
                         const bool& handlesinfo,
                         const bool& debug)
    : Mos(os), Mdebug(debug), Mwid2set(false), Msrceset(false), 
    Minfoset(false), Mfreeset(false), Mheaderflushed(false), 
    Mhandlestracefree(handlestracefree),
    Mhandlesfilefree(handlesfilefree),
    Mhandlesinfo(handlesinfo),
    Mhandlessrce(handlessrce),
    Mdatatype(datatype)
  { 
    DATRW_debug(this->Mdebug,
                "odatstream::odatstream",
                "create new output stream");
    DATRW_assert(os.good(), "output stream is not good!");
  }

  /*----------------------------------------------------------------------*/

  void odatstream::setfree(const sff::FREE& free) 
  {
    DATRW_debug(this->Mdebug, "odatstream::setfree",
                DATRW_value(free.lines.size()));
    if (Mheaderflushed)
    {
      DATRW_report_assert(this->handlestracefree(),
                          "file format cannot handle trace FREE data\n" <<
                          "FREE data will be dropped silently");
    } else {
      DATRW_report_assert(this->handlesfilefree(),
                          "file format cannot handle file FREE data\n" <<
                          "FREE data will be dropped silently");
    }
    Mfreeset=true;
    Mfree=free;
  }

  /*----------------------------------------------------------------------*/

  void odatstream::setwid2(const sff::WID2& wid2) 
  {
    DATRW_debug(this->Mdebug, "odatstream::setwid2",
                DATRW_value(wid2.line().substr(0,50)));
    Mwid2set=true;
    Mwid2=wid2;
    this->flushfileheader();
  }

  /*----------------------------------------------------------------------*/

  void odatstream::setinfo(const sff::INFO& info) 
  {
    DATRW_debug(this->Mdebug, "odatstream::setinfo",
                DATRW_value(info.line().substr(0,50)));
    DATRW_report_assert(this->handlesinfo(),
                        "file format cannot handle INFO data\n" 
                        "INFO data will be dropped silently");
    Minfoset=true;
    Minfo=info;
  }

  /*----------------------------------------------------------------------*/

  void odatstream::setsrce(const sff::SRCE& srce) 
  {
    DATRW_debug(this->Mdebug, "odatstream::setsrce",
                DATRW_value(srce.line().substr(0,50)));
    DATRW_report_assert(this->handlessrce(),
                        "file format cannot handle SRCE data\n"
                        "SRCE data will be dropped silently");
    Msrceset=true;
    Msrce=srce;
  }

  /*----------------------------------------------------------------------*/

  void odatstream::flushfileheader() 
  {
    if (!Mheaderflushed)
    {
      writefileheader();
    }
    Mheaderflushed=true;
    /*
     * 1.4.2011: I see no reason to clear Msrceset here. Keeping this flag
     * provides the trace writing functions with information on the source
     * which relates to the trace. This is of particular value for writer like
     * osustream, which have to calculate delay times.
     */
    // Msrceset=false;
    Mfreeset=false;
  }

  /*----------------------------------------------------------------------*/

  void odatstream::cleartraceheader() 
  {
    Mwid2set=false;
    Mfreeset=false;
    Minfoset=false;
  }

  /*----------------------------------------------------------------------*/

  void odatstream::writeseries(const Tdseries::Tcoc& series) 
  {
    DATRW_debug(this->Mdebug, "odatstream::writeseries (double)",
                DATRW_value(series.f()) << ", " << DATRW_value(series.l()));
    DATRW_assert(this-Mwid2set,"missing WID2 header");
    this->writetrace(series);
    this->cleartraceheader();
  }

  /*----------------------------------------------------------------------*/

  void odatstream::writeseries(const Tfseries::Tcoc& series) 
  {
    DATRW_debug(this->Mdebug, "odatstream::writeseries (float)",
                DATRW_value(series.f()) << ", " << DATRW_value(series.l()));
    DATRW_assert(this-Mwid2set,"missing WID2 header");
    this->writetrace(series);
    this->cleartraceheader();
  }

  /*----------------------------------------------------------------------*/

  void odatstream::writeseries(const Tiseries::Tcoc& series) 
  {
    DATRW_debug(this->Mdebug, "odatstream::writeseries (int)",
                DATRW_value(series.f()) << ", " << DATRW_value(series.l()));
    DATRW_assert(this-Mwid2set,"missing WID2 header");
    this->writetrace(series);
    this->cleartraceheader();
  }
    
  /*----------------------------------------------------------------------*/

  void odatstream::help(std::ostream& os, const char* name)
  {
    os << "Class " << name << " provides no help text." << std::endl;
  }


} // namespace datrw

/* ----- END OF datrw.cc ----- */
