/*! \file isustream.cc
 * \brief read Seismic Unix data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/11/2010
 * 
 * read Seismic Unix data (implementation)
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
 *  - 19/11/2010   V1.0   Thomas Forbriger
 *  - 23.12.2010   V1.1   use stream buffer seekg() function to skip series
 *  - 21/01/2012   V1.2   
 *                        - prepared isustream to take modifiers
 *                        - use SUHeaderControl to store format modifier values
 *                        - pass control parameters to SUheader
 *                        - bestrict is bundled with spatial sampling control
 *                          parameters
 *  - 18/11/2016   V1.3   use debug flag in base class
 * 
 * ============================================================================
 */
#define DATRW_SU_CC_VERSION \
  "DATRW_SU_CC   V1.3"

#include <datrwxx/su.h>
#include <datrwxx/util.h>
#include <datrwxx/error.h>
#include <datrwxx/debug.h>
#include <datrwxx/suformat.h>
#include <datrwxx/formatmodifier.h>

namespace datrw  {

  const std::ios_base::openmode
    isustream::openmode=std::ios_base::in|std::ios_base::binary;

  /*----------------------------------------------------------------------*/

  isustream::isustream(std::istream& is, 
                       const std::string& modifier,
                       const bool& debug):
    Tbase(is, true, true, false, debug),
    Mnextheader(Mheadercontrol, debug)
  { 
    DATRW_debug(Mdebug, "isustream::isustream","entering constructor");
    // evaluate format modifiers
    Mheadercontrol=su::inputmodifiers(modifier, Mdebug);

    // read first (next) trace header
    DATRW_assert(is.good(),
                 "ERROR (isustream::isustream): input stream is bad");
    Mnextheader.set(Mheadercontrol);
    Mnextheader.read(is);
    this->setsrce(Mnextheader.srce());
    DATRW_debug(Mdebug, "isustream::isustream", "finished constructor");
  }

  /*----------------------------------------------------------------------*/

  Tdseries isustream::dseries()
  {
    return(datrw::util::convert<Tfseries, Tdseries>(this->fseries()));
  } // Tdseries isustream::dseries()

  /*----------------------------------------------------------------------*/

  Tfseries isustream::fseries()
  {
    DATRW_debug(Mdebug, "isustream::fseries","entering function");
    Tfseries series(Mnextheader.Mheader.ns);
    char *ipointer=reinterpret_cast<char *>(series.pointer());
    DATRW_Xassert(Mis.read(ipointer, series.size()*sizeof(float)),
                  "ERROR (isustream::fseries): reading SU samples",
                  ::datrw::su::SUReadException);
    DATRW_debug(Mdebug, "isustream::fseries",
                "read " << series.size() << " samples"
                << " which are " << (series.size()*sizeof(float))
                << " characters");
    this->readheader();
    DATRW_debug(Mdebug, "isustream::fseries",
                this->wid2().line() << "\n" << this->info().line());
    DATRW_debug(Mdebug, "isustream::fseries","leaving function");
    return(series);
  } // Tiseries isustream::fseries()

  /*----------------------------------------------------------------------*/

  void isustream::skipseries()
  {
    Mis.seekg(sizeof(float)*Mnextheader.Mheader.ns,
              std::ios_base::cur);
    DATRW_Xassert(Mis.good(),
                  "ERROR (isustream::skipseries): calling seekg()",
                  ::datrw::su::SUReadException);
    this->readheader();
  } // void isustream::skipseries()

  /*----------------------------------------------------------------------*/

  void isustream::readheader()
  {
    DATRW_debug(Mdebug, "isustream::readheader","entering function");
    DATRW_assert(this->good(),
                 "ERROR (isustream::readheader): reached end of file");
    this->newtrace();
    this->setwid2(Mnextheader.wid2());
    this->setinfo(Mnextheader.info());
    DATRW_assert(((this->srce().date == Mnextheader.srce().date)
                  && (this->srce().cs == Mnextheader.srce().cs)
                  && (this->srce().cx == Mnextheader.srce().cx)
                  && (this->srce().cy == Mnextheader.srce().cy)
                  && (this->srce().cz == Mnextheader.srce().cz)),
                 "ERROR (isustream::readheader): "
                 "current source differs from previous");
    bool rocflag=datrw::Exception::report_on_construct_flag();
    datrw::Exception::dont_report_on_construct();
    try {
      Mnextheader.read(Mis);
    } catch (datrw::su::SUReadException)
    {
      this->setlast();
    } catch (datrw::Exception E) 
    {
      datrw::Exception::report_on_construct_flag(rocflag);
      E.report();
      throw(E);
    }
    datrw::Exception::report_on_construct_flag(rocflag);
    DATRW_debug(Mdebug, "isustream::readheader","leave function");
  } // void isustream::readheader()

  /*----------------------------------------------------------------------*/

  void isustream::help(std::ostream& os)
  { 
    os <<
      std::endl <<
      "SeismicUn*x reading functions" << std::endl <<
      "-----------------------------" << std::endl <<
      DATRW_SU_CC_VERSION << std::endl <<
      std::endl <<
      "This module provides reading of SeismicUn*x binary data files."
      << std::endl;
    os << std::endl;
    datrw::su::SUheader::help(os);
    os << std::endl;
    os <<
      "Valid format modifiers are:\n";
    formatmodifiers::ModifierHelp mh(os, 14);    
    mh(su::subformat::key::strict) <<
              "Abort if scalco violates the definition of SeismicUn*x.\n";
      mh() << "The default is to tolerate spatial sampling\n";
      mh() << "scaling factors being zero or being small\n";
      mh() << "powers of ten.\n";
  } // void isustream::help(std::ostream& os)

} // namespace datrw

/* ----- END OF isustream.cc ----- */
