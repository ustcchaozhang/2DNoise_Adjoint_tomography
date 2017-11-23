/*! \file mseedread_mseedrecord_skipdata.cc
 * \brief member function mseed::MiniSEEDRecord::skipdata (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/06/2016
 * 
 * member function mseed::MiniSEEDRecord::skipdata (implementation)
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
 * 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.

 * ----
 *
 * REVISIONS and CHANGES 
 *  - 23/06/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define DATRW_MSEEDREAD_MSEEDRECORD_SKIPDATA_CC_VERSION \
  "DATRW_MSEEDREAD_MSEEDRECORD_SKIPDATA_CC   V1.0"

#include <datrwxx/mseedread.h>

namespace datrw {

  namespace mseed {

    /*----------------------------------------------------------------------*/
    // small effort of skipping a MiniSEED record
    
    /*!
     * \note
     *   Apparently this function is not used at all.
     *   It probably is useless, because datrw::mseed::imseedstream::read
     *   requires to extract block data in order to control whether data is
     *   contiguous.
     *   With some data the amount of actual samples and the amount of data
     *   blocks required to store them is only well established after decoding
     *   all data frames.
     *
     * \deprecated
     *   This function appears obsolete and might be removed in the future.
     */
    void MiniSEEDRecord::skipdata(std::istream& is)
    {
      // create block to read MiniSEED
      MiniSEEDblock block=this->readheader(is);
      // count blocks
      int iblock=0;

      if (Mvalid)
      {
        // data header is present
        ++iblock;

        // size of record in bytes
        int reclen=Mblockette1000.reclenbytes();
        // number of blocks to read for full record
        int nblocks=reclen/block.bytesize();
        Mdata=Tseries(0,1); 

        // extract samples
        // ---------------
        while (iblock < nblocks)
        {
          is >> block; 
          if (is.bad())
          {
            std::cerr << "WARNING (reading MiniSEED record): "
              << "input stream is bad!" << std::endl;
          }
          ++iblock;
        }

        // finished successfully
        if (is.good()) { Mvalid=true; }
      }
    } // void MiniSEEDRecord::skipdata(std::istream& is)

  } // namespace mseed

} // namespace datrw

/* ----- END OF mseedread_mseedrecord_skipdata.cc ----- */
