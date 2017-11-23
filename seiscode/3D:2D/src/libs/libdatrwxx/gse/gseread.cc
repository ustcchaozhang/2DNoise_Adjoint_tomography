/*! \file gseread.cc
 * \brief raw GSE reading module (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/09/2007
 * 
 * raw GSE reading module (implementation)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/09/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_GSEREAD_CC_VERSION \
  "DATRW_GSEREAD_CC   V1.0   "

#include <datrwxx/gseread.h>
#include <datrwxx/error.h>
#include <sffxx.h>
#include <gsexx.h>
#include <sstream>

namespace datrw {

  namespace gse {
      
    // find next WID2 line
    sff::WID2 next_wid2(std::istream& is)
    { 
      sff::WID2 retval;
      std::string theline;
      bool hot=true;
      while (hot)
      {
        DATRW_assert(is.good(), "input stream is not good");
        std::getline(is, theline);
        std::istringstream iss(theline);
        hot=false;
        try 
        {
          retval.read(iss);
        }
        catch (...)
        {
          hot=true;
        }
      }
      return(retval);
    } // sff::WID2 next_wid2(std::istream& is)

    /*----------------------------------------------------------------------*/

    // print info about GSE reading
    void help(std::ostream& os)
    {
      os <<
        std::endl <<
        "GSE reading functions" << std::endl <<
        "---------------------" << std::endl <<
        DATRW_GSEREAD_CC_VERSION << std::endl <<
        std::endl <<
        "This module is designed to read raw GSE files." << std::endl <<
        "Only one trace per file can be handled." << std::endl <<
        "All additional line (prior to WID2 and after CHK2) are ignored." 
          << std::endl;
    } // void help(std::ostream& os)

    /*----------------------------------------------------------------------*/

    //! read samples from file
    Tiseries read_gse_data(std::istream& is, const int& nsamples)
    {
      Tiseries retval;
      typedef Tiseries::Tvalue Tvalue;
      try 
      {
        retval=Tiseries(nsamples);
      }
      catch(...) 
      {
        std::cerr << "ERROR (datrw::gse::read_gse_data): "
                  << "allocating series for " 
                  << nsamples << " samples!" << std::endl;
        throw;
      }
      // WID2 reading checks for CM6 subformat 
      // (only subformat supported so far)
      GSE2::waveform::TDAT2readCM6 freader(nsamples);
      for(aff::Iterator<Tiseries> i(retval); i.valid(); ++i)
      { (*i) = Tvalue(freader(is)); }
      return(retval);
    }

  } // namespace gse

} // namespace datrw

/* ----- END OF gseread.cc ----- */
