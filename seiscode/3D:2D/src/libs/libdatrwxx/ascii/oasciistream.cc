/*! \file oasciistream.cc
 * \brief output raw ASCII data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/10/2011
 * 
 * output raw ASCII data (implementation)
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
 *  - 23/08/2012   V1.1   support format modifiers
 *  - 18/11/2016   V1.2   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_OASCIISTREAM_CC_VERSION \
  "DATRW_OASCIISTREAM_CC   V1.2"

#include<datrwxx/ascii.h>
#include<datrwxx/debug.h>
#include<datrwxx/error.h>
#include<datrwxx/formats.h>
#include<datrwxx/formatmodifier.h>
#include<datrwxx/asciiheaderkeys.h>

#define POUT( K, V ) Mos << "# " << K << ": " << V << std::endl;
#define FOUT( V ) Mos << "## " << V << std::endl;

namespace datrw {

  const std::ios_base::openmode 
    oasciistream::openmode=std::ios_base::out;

  /*----------------------------------------------------------------------*/

  oasciistream::oasciistream(std::ostream& os, 
                             const std::string& modifier,
                             const bool& debug):
    Tbase(os, Fall, true, true, true, true, debug)
  {
    DATRW_debug(Mdebug, "oasciistream::oasciistream",
                        "new instance established");
    // format modifiers must be used here to set defaults
    datrw::Subformat subformat(modifier);
    subformat("precision", "7") >> this->Mprecision;
    DATRW_assert_modifiers_are_recognized(subformat, 
                                          "iasciistream");
  } // oasciistream::oasciistream

  /*----------------------------------------------------------------------*/

  void oasciistream::help(std::ostream& os)
  {
    os <<
      std::endl <<
      "ASCII writing functions" << std::endl <<
      "-----------------------" << std::endl <<
      DATRW_OASCIISTREAM_CC_VERSION << std::endl <<
      std::endl;
    os <<
      "Valid format modifiers are:\n";
    formatmodifiers::ModifierHelp mh(os, 17);    
    mh("precision", "n") <<
              "Set number of significant digits to \"n\" for\n";
      mh() << "float and double format output.\n";
  } // void oasciistream::help(std::ostream& os=std::cout)

  /*----------------------------------------------------------------------*/

  void oasciistream::writefileheader()
  {
    DATRW_debug(Mdebug, "oasciistream::writefileheader",
                        "entered function");
    if (this->hasfree())
    {
      const ::sff::FREE& free=this->free();
      ::sff::FREE::Tlines::const_iterator I=free.lines.begin();
      while (I!=free.lines.end())
      {
        DATRW_debug(Mdebug, "oasciistream::writefileheader",
                            "line: " << *I);
        FOUT( *I );
        ++I;
      }
    }
    DATRW_debug(Mdebug, "oasciistream::writefileheader",
                        "file FREE written if present");
    if (this->hassrce())
    {
      ::sff::SRCE srce=this->srce();
      POUT( ascii::keySRCEtype, srce.type );
      POUT( ascii::keySRCEdate, srce.date.hierarchicalstring() );
      POUT( ascii::keySRCEX, srce.cx );
      POUT( ascii::keySRCEY, srce.cy );
      POUT( ascii::keySRCEZ, srce.cz );
      POUT( ascii::keySRCECS, ::sff::coosysID(srce.cs) );
    }
    DATRW_debug(Mdebug, "oasciistream::writefileheader",
                        "SRCE written if present");
  } // void oasciistream::writefileheader()

  /*----------------------------------------------------------------------*/

  void oasciistream::writetrace(const Tdseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "oasciistream::writetrace (double)",
                        DATRW_value(series.f()) << ", "
                        DATRW_value(series.l()) << ", "
                        DATRW_value(Mprecision));
    this->writetraceheader(series.size(),ascii::keydouble);
    for (int i=series.first(); i<=series.last(); ++i)
    {
      Mos.precision(Mprecision);
      Mos << series(i) << "\n";
    }
    Mos.flush();
  } // void oasciistream::writefileheader()

  /*----------------------------------------------------------------------*/

  void oasciistream::writetrace(const Tfseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "oasciistream::writetrace (float)",
                        DATRW_value(series.f()) << ", "
                        DATRW_value(series.l()) << ", "
                        DATRW_value(Mprecision));
    this->writetraceheader(series.size(),ascii::keyfloat);
    for (int i=series.first(); i<=series.last(); ++i)
    {
      Mos.precision(Mprecision);
      Mos << series(i) << "\n";
    }
    Mos.flush();
  } // void oasciistream::writetrace(const Tfseries::Tcoc& series)

  /*----------------------------------------------------------------------*/

  void oasciistream::writetrace(const Tiseries::Tcoc& series)
  {
    DATRW_debug(Mdebug, "oasciistream::writetrace (int)",
                        DATRW_value(series.f()) << ", "
                        DATRW_value(series.l()));
    this->writetraceheader(series.size(),ascii::keyint);
    for (int i=series.first(); i<=series.last(); ++i)
    {
      Mos << series(i) << "\n";
    }
    Mos.flush();
  } // void oasciistream::writetrace(const Tiseries::Tcoc& series)

  /*----------------------------------------------------------------------*/

  void oasciistream::writetraceheader(const unsigned int& n,
                                      const char* type)
  {
    DATRW_debug(Mdebug, "oasciistream::writetraceheader",
                        "entered function");
    ::sff::WID2 wid2=this->wid2();
    wid2.nsamples=n;
    POUT( ascii::keydate, wid2.date.hierarchicalstring() );
    POUT( ascii::keydt, wid2.dt );
    POUT( ascii::keynsamples, wid2.nsamples );
    POUT( ascii::keystation, wid2.station );
    POUT( ascii::keychannel, wid2.channel );
    POUT( ascii::keyauxid, wid2.auxid );
    POUT( ascii::keyinstype, wid2.instype );
    POUT( ascii::keycalib, wid2.calib );
    POUT( ascii::keycalper, wid2.calper );
    POUT( ascii::keyhang, wid2.hang );
    POUT( ascii::keyvang, wid2.vang );
    DATRW_debug(Mdebug, "oasciistream::writetraceheader",
                        "WID2 written");
    if (this->hasinfo())
    {
      ::sff::INFO info=this->info();
      POUT( ascii::keynstacks, info.nstacks );
      POUT( ascii::keyRECVX, info.cx );
      POUT( ascii::keyRECVY, info.cy );
      POUT( ascii::keyRECVZ, info.cz );
      POUT( ascii::keyRECVCS, ::sff::coosysID(info.cs) );
    }
    DATRW_debug(Mdebug, "oasciistream::writetraceheader",
                        "INFO written if present");
    if (this->hasfree())
    {
      const ::sff::FREE& free=this->free();
      ::sff::FREE::Tlines::const_iterator I=free.lines.begin();
      while (I!=free.lines.end())
      {
        FOUT( *I );
        ++I;
      }
    }
    DATRW_debug(Mdebug, "oasciistream::writetraceheader",
                        "trace FREE written if present");
    POUT( ascii::keydata, type);
    DATRW_debug(Mdebug, "oasciistream::writetraceheader",
                        "finished");
  } // void oasciistream::writetraceheader(const unsigned int& n)

} // namespace datrw

/* ----- END OF oasciistream.cc ----- */
