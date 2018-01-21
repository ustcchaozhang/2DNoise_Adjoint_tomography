/*! \file sacread.cc
 * \brief decode sac files (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2004
 * 
 * decode sac files (implementation)
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
 *  - 21/12/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_SACREAD_CC_VERSION \
  "DATRW_SACREAD_CC   V1.0   "

#include <datrwxx/sacread.h>
#include <datrwxx/error.h>

namespace datrw {

  namespace sac {
      
    // read SAC header from stream
    SACheader read_sac_header(std::istream& is)
    { 
      SACheader retval;
      typedef char* Pchar;
      is.read(Pchar(&retval), sizeof(SACheader)); 
      return(retval);
    } // SACheader read_sac_header(std::istream& is)

    /*----------------------------------------------------------------------*/

    // print info about SAC reading
    void help(std::ostream& os)
    {
      os <<
        std::endl <<
        "SAC reading functions" << std::endl <<
        "---------------------" << std::endl <<
        DATRW_SACREAD_CC_VERSION << std::endl <<
        std::endl <<
        "These functions are tuned to read binary SAC files as produced by" 
        << std::endl <<
        "rdseed from IRIS SEED volumes." << std::endl;
    } // void help(std::ostream& os)

    /*----------------------------------------------------------------------*/

    //! read samples from file
    Tseries read_sac_data(std::istream& is, const int& nsamples)
    {
      Tseries series(nsamples);
      DATRW_assert(is.good(), 
                     "read_sac_data: input stream is not good before reading");
      typedef char* Pchar;
      is.read(Pchar(series.pointer()), nsamples*sizeof(Tvalue));
      DATRW_assert(is.good(), 
                     "read_sac_data: input stream is not good after reading");
      return(series);
    }

  } // namespace sac

} // namespace datrw

/* ----- END OF sacread.cc ----- */
