/*! \file itsoftstream.cc
 * \brief actual TSOFT to SFF code (implementation)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/11/2009
 * 
 * actual TSOFT to SFF code (implementation)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 11/11/2009   V1.0   Thomas Forbriger
 *  - 23/11/2010   V1.1   introduced static member data
 *  - 05/12/2011   V1.2   implemented format modifiers to handle gaps
 *  - 07/01/2013   V1.3   fix problem with improper detection of end of file
 *                        for single trace data
 *  - 18/11/2016   V1.4   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_TSOFT_CC_VERSION \
  "DATRW_TSOFT_CC   V1.4"

#include <datrwxx/tsoft.h>
#include <datrwxx/util.h>
#include <datrwxx/channeltranslation.h>
#include <datrwxx/formatmodifier.h>

namespace datrw {

  const std::ios_base::openmode itsoftstream::openmode=std::ios_base::in;
   
  //@{
  /*! \brief Format properties
   * \ingroup group_tsoft
   */
  const bool tsoft::isbinary=false;
  const char* const tsoft::streamID="tsoft";
  //@}

  /*----------------------------------------------------------------------*/

  namespace tsoft {
    //@{
    /*! \brief Format modifier keys
     * \ingroup group_tsoft
     */
    const char* const key_keepundetval="keepundetval";
    const char* const key_replaceundetval="replaceundetval";
    const char* const key_bridgetime="bridgetime";
    const char* const key_flagbridged="flagbridged";
    //@}
  } // namespace tsoft

  /*----------------------------------------------------------------------*/

  itsoftstream::itsoftstream(std::istream& is, const bool& debug):
    Tbase(is, true, true, false, debug)
  { 
    this->read(is); 
  } // itsoftstream::itsoftstream(std::istream& is, const bool& debug)

  /*----------------------------------------------------------------------*/

  itsoftstream::itsoftstream(std::istream& is, 
                             const std::string& modifier,
                             const bool& debug):
    Tbase(is, true, true, false, debug)
  { 
    datrw::Subformat subformat(modifier);
    subformat(tsoft::key_replaceundetval, "9000.") 
      >> this->Mreaderconfig.flagvalue;
    this->Mreaderconfig.setundetval
      =subformat.isset(tsoft::key_replaceundetval);
    this->Mreaderconfig.keepundetval
      =subformat.isset(tsoft::key_keepundetval);
    this->Mreaderconfig.bridgesamples
      =subformat.isset(tsoft::key_bridgetime);
    this->Mreaderconfig.bridgetime
      =libtime::TAbsoluteTime(subformat.value(tsoft::key_bridgetime,
                                              "1/1/1/1/1/1"));
    this->Mreaderconfig.flagbridged
      =subformat.isset(tsoft::key_flagbridged);
    subformat(tsoft::key_flagbridged, "-9000.") 
      >> this->Mreaderconfig.bridgeflagvalue;
    DATRW_assert_modifiers_are_recognized(subformat, "itsoftstream");
    this->read(is); 
  } // itsoftstream::itsoftstream(std::istream& is, const bool& debug)

  /*----------------------------------------------------------------------*/

  void itsoftstream::read(std::istream& is)
  {
    Mfile=datrw::tsoft::readfile(is, Mreaderconfig); 
    Mitrace=this->Mfile.Mtraces.first();
    this->setfilefree(Mfile.Mfree);
    if (Mfile.Mtraces.size()<1) { this->setlast(); }
  } // void itsoftstream::read(std::istream& is)

  /*----------------------------------------------------------------------*/

  Tdseries itsoftstream::dseries()
  {
    this->newtrace();
    DATRW_assert(Mitrace<=this->Mfile.Mtraces.last(),
                   "reading beyond end of file");
    if (Mitrace==this->Mfile.Mtraces.last()) { this->setlast(); }
    datrw::tsoft::Trace trace=Mfile.Mtraces(Mitrace);
    this->setwid2(trace.Mwid2);
    this->settracefree(trace.Mfree);
    ++Mitrace;
    return(trace.Mseries);
  } // Tdseries itsoftstream::dseries()

  /*----------------------------------------------------------------------*/

  Tfseries itsoftstream::fseries()
  { return(datrw::util::convert<Tdseries, Tfseries>(this->dseries())); } 

  /*----------------------------------------------------------------------*/

  void itsoftstream::skipseries()
  { 
    DATRW_assert(Mitrace<=this->Mfile.Mtraces.last(),
                   "reading beyond end of file");
    if (Mitrace==this->Mfile.Mtraces.last()) { this->setlast(); }
    datrw::tsoft::Trace trace=Mfile.Mtraces(Mitrace);
    this->setwid2(trace.Mwid2);
    this->settracefree(trace.Mfree);
    ++Mitrace;
  } // void itsoftstream::skipseries()

  /*----------------------------------------------------------------------*/

  void itsoftstream::help(std::ostream& os)
  {
    os << std::endl;
    os << "TSOFT data reading functions" << std::endl;
    os << "----------------------------" << std::endl;
    os << DATRW_TSOFT_CC_VERSION << std::endl;
    os << DATRW_TSOFT_H_VERSION << std::endl;
    os << "TSOFT format is defined by the TSOFT software:\n"
      << "  http://seismologie.oma.be/TSOFT/tsoft.html" << std::endl;
    os << "It is the standard data storage format of the UIPC " << std::endl
      << "data acquisition of GWR superconducting gravimeters." << std::endl;
    os << "This reading module is tailored to handle data recorded\n"
      << "for the SG056 at BFO.\n";
    os << std::endl;
    datrw::tsoft::reporttranslation(os);
    os << std::endl;
    datrw::tsoft::reportdatacomments(os);
    DATRW_assert(datrw::tsoft::translationisunique(),
                   "TSOFT translation table is not unique");
    os << std::endl;
    os 
      << "The TSOFT reading module supports format modifiers to handle\n"
      << "gaps in the otherwise continuous input data:\n";
    os << "  " << tsoft::key_keepundetval << "\n";
    os 
      << "    Samples of the value defined as \"UNDETVAL\" in the TSOFT\n"
      << "    header are usually regarded as non-existent and for this\n"
      << "    reason produce a gap in the data. If this flag is set,\n"
      << "    these sample values are simply passed to the output.\n";
    os << "  " << tsoft::key_replaceundetval << "=value\n";
    os
      << "    Samples of the value defined as \"UNDETVAL\" in the TSOFT\n"
      << "    header are set to \"value\".\n";
    os << "  " << tsoft::key_bridgetime << "=YYYY/MM/DD/hh/mm/ss\n";
    os
      << "    The UIPC data acquisition system of SG056 occasionally\n"
      << "    produces data lines with unreasonable data times. The\n"
      << "    typical time on these lines is\n"
      << "      0001 01 01 01 01 01\n"
      << "    These entries produce gaps in the output data. If this\n"
      << "    modifier is passed and the line is not the first line\n"
      << "    in the input data line, lines with sample time defined\n"
      << "    by the modifier parameter are replaced by the expected\n"
      << "    time for this line and sample values are included in\n"
      << "    the output data stream.\n";
    os << "  " << tsoft::key_flagbridged << "=value\n";
    os 
      << "    If this flag is set, sample values of lines with originally\n"
      << "    unreasonable sample time being bridge by the reader are\n"
      << "    flagged with sample value being replaced by \"value\".\n";
  } // void itsoftstream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF itsoftstream.cc ----- */
