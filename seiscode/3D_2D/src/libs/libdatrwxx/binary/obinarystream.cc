/*! \file obinarystream.cc
 * \brief output raw binary data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/10/2011
 * 
 * output raw binary data (implementation)
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
 * 
 * REVISIONS and CHANGES 
 *  - 18/10/2011   V1.0   Thomas Forbriger
 *  - 18/11/2016   V1.1   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_OBINARYSTREAM_CC_VERSION \
  "DATRW_OBINARYSTREAM_CC   V1.1"

#include<datrwxx/binary.h>
#include<datrwxx/debug.h>

namespace datrw {

  const std::ios_base::openmode
    obinarystream::openmode=std::ios_base::out|std::ios_base::binary;

  /*----------------------------------------------------------------------*/

  obinarystream::obinarystream(std::ostream& os, const bool& debug):
    Tbase(os, Fall, true, true, true, true, debug),
    Mobs(os, ::datrw::binary::magic, Mdebug)
    //Mobs(os, ::datrw::binary::magic, debug)
  {
    DATRW_debug(Mdebug, "obinarystream::obinarystream",
                "new stream");
  } // obinarystream::obinarystream(std::ostream& os, const bool& debug=false)

  /*----------------------------------------------------------------------*/

  obinarystream::~obinarystream()
  {
  } // obinarystream::~obinarystream()

  /*----------------------------------------------------------------------*/

  void obinarystream::writefileheader()
  {
    DATRW_debug(Mdebug, "obinarystream::writefileheader",
                "file version " << ::datrw::binary::version);
    Mobs << ::datrw::binary::version;
    char flags=0;
    if (this->hassrce()) { flags |= binary::Fsrce; }
    if (this->hasfree()) { flags |= binary::Ffree; }
    DATRW_debug(Mdebug, "obinarystream::writefileheader()",
                "flags: " << DATRW_value( int(flags) ));
    Mobs << flags;
    if (this->hassrce()) 
    {
      DATRW_debug(Mdebug, "obinarystream::writefileheader",
                  "writing SRCE line");
      Mobs << this->srce(); 
    }
    if (this->hasfree()) 
    {
      DATRW_debug(Mdebug, "ibinarystream::ibinarystream",
                  "writing file FREE block");
      Mobs << this->free(); 
    }
  } // void obinarystream::writefileheader()

  /*----------------------------------------------------------------------*/

  void obinarystream::writetraceheader(const binary::Eflags& datatype,
                                       const unsigned int& nsamples)
  {
    char flags=datatype;
    if (this->hasinfo()) { flags |= binary::Finfo; }
    if (this->hasfree()) { flags |= binary::Ffree; }
    DATRW_debug(Mdebug, "obinarystream::writetraceheader()",
                "flags: " << DATRW_value( int(flags) ));
    Mobs << flags;
    ::sff::WID2 wid2=this->wid2();
    wid2.nsamples=nsamples;
    Mobs << wid2;
    if (this->hasinfo()) { Mobs << this->info(); }
    if (this->hasfree()) { Mobs << this->free(); }
  } // void obinarystream::writetraceheader()

  /*----------------------------------------------------------------------*/

  void obinarystream::writetrace(const Tdseries::Tcoc& series)
  {
    this->writetraceheader(binary::Fdouble, series.size());
    Mobs.write(series);
  } // void obinarystream::writetrace(const Tdseries& series)

  /*----------------------------------------------------------------------*/

  void obinarystream::writetrace(const Tfseries::Tcoc& series)
  {
    this->writetraceheader(binary::Ffloat, series.size());
    Mobs.write(series);
  } // void obinarystream::writetrace(const Tfseries& series)

  /*----------------------------------------------------------------------*/

  void obinarystream::writetrace(const Tiseries::Tcoc& series)
  {
    this->writetraceheader(binary::Fint, series.size());
    Mobs.write(series);
  }  // oid obinarystream::writetrace(const Tiseries& series)

  /*----------------------------------------------------------------------*/

  void obinarystream::help(std::ostream& os)
  { 
    os <<
      std::endl <<
      "BINARY writing functions" << std::endl <<
      "------------------------" << std::endl <<
      DATRW_OBINARYSTREAM_CC_VERSION << std::endl <<
      std::endl;
    os << 
      "This module writes data in its simple binary representaion.\n"
      "It is provided primarily for input/output efficiency. This format\n"
      "does not require any output or input formatting. It also does not\n"
      "require conversion of sample values. For this reason no round-off\n"
      "will take place, not for the sample values and not for the sampling\n"
      "interval. The format maps memory content to file and is lossless\n"
      "for this reason. The file is identified by a magic number. This\n"
      "allows the input module to perform automatic on-the-fly\n"
      "byte-swapping if required by the CPU type in use.\n"
      << std::endl;
  } // void obinarystream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF obinarystream.cc ----- */
