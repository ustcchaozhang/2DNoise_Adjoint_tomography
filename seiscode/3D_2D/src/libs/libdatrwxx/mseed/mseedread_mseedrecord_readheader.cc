/*! \file mseedread_mseedrecord_readheader.cc
 * \brief MiniSEEDRecord::readheader read and parse information contained therein (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/07/2014
 * 
 * MiniSEEDRecord::readheader read and parse information contained therein 
 * (implementation)
 *
 * Copyright (c) 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 22/07/2014   V1.0   Thomas Forbriger
 *                        copied from mseed/mseedread.cc
 * 
 * ============================================================================
 */
#define TF_MSEEDREAD_MSEEDRECORD_READHEADER_CC_VERSION \
  "TF_MSEEDREAD_MSEEDRECORD_READHEADER_CC   V1.0"

#include <string>
#include <cstring>
#include <string.h>
#include <datrwxx/mseedread.h>
#include <datrwxx/mseed.h>
#include<datrwxx/bytesex.h>
#include<aff/subarray.h>

namespace datrw {

  namespace mseed {

    /*----------------------------------------------------------------------*/
    // extract header from MiniSEED data
    
    MiniSEEDblock MiniSEEDRecord::readheader(std::istream& is)
    {
      // create block to read MiniSEED
      MiniSEEDblock block;
      // count blocks
      int iblock=0;
      // reset blockette indicators
      Mhasblockette1000=false;
      Mhasblockette1001=false;

      /* read first block and extract essential header fields
       * ----------------------------------------------------
       * Look for data control header
       * This initial sequence is required to skip the telemetry volume header
       * if present.
       *
       * See chapter 8, page 108: "Fixed Section of Data Header (48 bytes)"
       * in recent SEED V2.4 Manual for changes in the type field definition.
       *
       * Data header/quality indicator. Previously, this field was only
       * allowed to be “D” and was only used to indicate that this is a data
       * header. As of SEED version 2.4 the meaning of this field has been
       * extended to also indicate the level of quality control that has been
       * applied to the record.
       *
       * With SEED version 2.4 the type field may contain the following
       * indicators:
       *
       * D — The state of quality control of the data is indeterminate.
       * R — Raw Waveform Data with no Quality Control
       * Q — Quality Controlled Data, some processes have been applied to the
       *     data.
       * M — Data center modified, time-series values have not been changed.
       */
      bool nodataheader=true;
      while (nodataheader && is.good()) {
        is >> block;
        SEED::ControlHeader controlheader(block.block());
        if ((controlheader.type == 'D')
         || (controlheader.type == 'R')
         || (controlheader.type == 'Q')
         || (controlheader.type == 'M'))
            { nodataheader=false; }
      }

      if (nodataheader)
      {
        // trapped if EOF is reached before a valid controlheader
        // of type 'D' is found
        Mvalid=false;
      }
      else
      {
        // Flag which will be set true, if we have met a header in front
        // of valid sample block from which time series data can be extracted.
        // Alternative blocks are ASCII log data block, which will be skipped.
        bool seriesheader=false;
        while ((!seriesheader) && is.good())
        {
          // data header is present
          ++iblock;
          // SEED defines Motorola (big-endian) to be the standard byte order
          // for all headers
          // check our CPU type
          datrw::util::Ecpu_type mysex=datrw::util::cpu();
          DATRW_assert((mysex != datrw::util::cpu_unknown),
                           "ERROR (reading MiniSEED record): "
                           "cannot identify CPU type");
          bool doswap = (mysex == datrw::util::cpu_Intel);
          Mrecordheader=SEED::FixedDataRecordHeader(block.block(), doswap);
          // extract essential header fields
          // Blockette1000 is essential to determine bytesex
          if (Mrecordheader.fblock > block.bytesize())
          {
            // try it the other way
            doswap = !doswap;
            Mrecordheader=SEED::FixedDataRecordHeader(block.block(), doswap);
            DATRW_assert((Mrecordheader.fblock < block.bytesize()),
                           "ERROR (reading MiniSEED record): "
                           "cannot find first blockette");
          }
          // read Blockettes
          unsigned int blocketteadr=Mrecordheader.fblock;
          for (int i=0; i<Mrecordheader.numblock; i++)
          {
            SEED::DataRecordBlocketteHeader 
              bh(block.block(blocketteadr), doswap);
            if (bh.type == 1000)
            {
              Mhasblockette1000=true;
              Mblockette1000=
                SEED::DataOnlySEEDBlockette(block.block(blocketteadr), doswap);
            }
            else if (bh.type == 1001)
            {
              Mhasblockette1001=true;
              Mblockette1001=
                SEED::DataExtensionBlockette(block.block(blocketteadr), doswap);
            }
            blocketteadr=bh.next;
          }
          DATRW_assert(Mhasblockette1000,
                         "ERROR (reading MiniSEED record): " 
                         "Data Only SEED Blockette is essential");
          // check bytesex integrity
          DATRW_assert((needswap(Mblockette1000.bytesex)==doswap),
                         "ERROR (reading MiniSEED record): "
                         "swapping is not consistent");
          // check for possible ASCII data and skip ASCII data
          if (Mblockette1000.format == datrw::mseed::SEED::ascii)
          {
            unsigned int totalreclen=Mblockette1000.reclenbytes();
            unsigned int bytecount=Mrecordheader.dbeg;
            unsigned int pframe=Mrecordheader.dbeg;
            if (Mdebug.report_ascii_data_to_stdout)
            {
              std::cout << this->date().hierarchicalstring() << " ASCII data block:"
                << std::endl;
              std::cout << ">>";
            }
            while ((bytecount < totalreclen) && is.good())
            {
              if (pframe >= block.bytesize())
              {
                is >> block;
                pframe=0;
                iblock++;
              }
              while (pframe < block.bytesize())
              {
                char c=block[pframe];
                if (isprint(c))
                {
                  if (Mdebug.report_ascii_data_to_stdout)
                  {
                    std::cout << c;
                  }
                }
                else if (c == 0x0d)
                {
                  if (Mdebug.report_ascii_data_to_stdout)
                  {
                    std::cout << std::endl << ">>";
                  }
                }
                ++pframe;
                ++bytecount;
              } // while (pframe < block.bytesize())
            } // while ((bytecount < totalreclen) && is.good())
            if (Mdebug.report_ascii_data_to_stdout)
            {
              std::cout << std::endl;
            }
            is >> block;
            iblock++;
          } // if (Mblockette1000.format == datrw::mseed::SEED::ascii)
          else
          {
            // we expect to have met a valid series header
            seriesheader=true;
          }
        } // while ((!seriesheader) && is.good())
        // finished successfully
        if (is.good()) { Mvalid=true; }
      }
      return(block);
    } // MiniSEEDblock MiniSEEDRecord::readheader(std::istream& is)

  } // namespace mseed

} // namespace datrw

/* ----- END OF mseedread_mseedrecord_readheader.cc ----- */
