/*! \file mseedread_mseedrecord_read.cc
 * \brief MiniSEEDRecord::read read complete MiniSEED record and decode sample data (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/07/2014
 * 
 * MiniSEEDRecord::read read complete MiniSEED record and decode sample 
 * data (implementation)
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
 *  - 22/07/2014   V1.0   Thomas Forbriger (thof)
 *                        - copied from mseed/mseedread.cc
 *                        - issue warning if frame count is unreasonable
 *                        - support new format modifier: estimateNframes
 *  - 27/06/2016   V1.1b  thof
 *                        - fix reading of incompletely filled records
 *                 V1.2   thof
 *                        - disable check for number of frames, if number of
 *                          frames is guessed
 *  - 08/07/2016   V1.3   thof: 
 *                        - make correct use of new DATRW_nonfatal_assert
 *  - 11/07/2016   V1.4   thof:
 *                        - improve error messages in consistency checks
 *  - 12/07/2016   V1.5   thof:
 *                        - add consistency check for usec value
 * 
 * ============================================================================
 */
#define DATRW_MSEEDREAD_MSEEDRECORD_READ_CC_VERSION \
  "DATRW_MSEEDREAD_MSEEDRECORD_READ_CC   V1.5"

#include <string>
#include <cstring>
#include <string.h>
#include <datrwxx/mseedread.h>
#include <datrwxx/mseed.h>
#include <datrwxx/bytesex.h>
#include <datrwxx/debug.h>
#include <datrwxx/mseed_keywords.h>
#include <aff/subarray.h>

namespace datrw {

  namespace mseed {

    /*! \brief Big effort of decoding a MiniSEED record.
     *
     * Read all blocks belonging to one record in sequence. Extract all frames
     * in all blocks in sequence (within the outer loop of reading blocks).
     * Reading stops, if all blocks of the record are read or all frames (as
     * indicated in the header of the record) are extracted are all expected
     * samples are extracted. Extraction of samples stops when all samples (as
     * indicated by the record header) are extracted. 
     *
     * It may appear in normal operation that the capacity of a data record is
     * not completely used to store samples.
     */
    void MiniSEEDRecord::read(std::istream& is)
    {
      // create block to read MiniSEED
      MiniSEEDblock block=this->readheader(is);
      // count blocks
      int iblock=0;
      // count frames
      int iframe=0;

      if (Mvalid)
      {
        // data header is present
        ++iblock;

        // For SEED data (not header fields) the byte order my differ from
        // file to file. Use the byte order defined in the Data Only SEED
        // Blockette.
        bool doswap=needswap(Mblockette1000.bytesex);

        // extract essential header data
        // -----------------------------
        //
        // number of samples to extract
        int nsamples=Mrecordheader.nsamp;
        // beginning of first frame
        int pdata=Mrecordheader.dbeg;
        // size of record in bytes
        int reclen=Mblockette1000.reclenbytes();
        // number of blocks to read for full record
        int nblocks=reclen/block.bytesize();
        // number of frames to be expected
        int nframes=(reclen-pdata)/SEED::SteimFrame::blocksize;
        // is number of frames read from file?
        bool filetellsnframes=false;
        if (Mhasblockette1001 && (!MestimateNframes))
        {
          filetellsnframes=true;
          nframes=Mblockette1001.ifcount();
          if (nframes < 1)
          {
            DATRW_warning("MiniSEEDRecord::read",
              "unreasonable number of frames (" 
              << nframes << 
              ") given in blockette1001\n"
              << "      consider to use format modifier \"estimateNframes\"");
          }
        }
        // check Steim 1 encoding
        DATRW_assert(((Mblockette1000.iformat() == SEED::steim1) ||
                        (Mblockette1000.iformat() == SEED::steim2)),
                       "ERROR (reading MiniSEED record): " 
                       "Can only decode Steim (1) or Steim (2) compression");
        // check size of sample buffer
        if (int(Mdata.size())<nsamples)
        { Mdata=Tseries(nsamples); }
        int isample=0;
        // x0, xn, d1
        int x0, xn, d1, sum;
        // x0 and xn are not read yet
        bool waitingforxn=true;

        // extract samples
        // ---------------
        // loop over blocks in record
        while ((iblock <= nblocks) &&
               (isample < nsamples) &&
               ((iframe < nframes) || (!filetellsnframes)))
        {
          // extract 
          // (loop over frames in block)
          while (pdata < int(block.bytesize()) &&
                 (isample < nsamples) &&
                 ((iframe < nframes) || (!filetellsnframes)))
          {
            SEED::SteimFrame* pframe;
            switch (Mblockette1000.iformat())
            {
              case SEED::steim1:
                pframe= new SEED::Steim1Frame(block.block(pdata), doswap);
                break;
              case SEED::steim2:
                pframe= new SEED::Steim2Frame(block.block(pdata), doswap);
                break;
              default:
                DATRW_abort("ERROR (reading MiniSEED record): "
                              "compression format not supported");
            }
            ++iframe;
            SEED::SteimFrame& rframe(*pframe);

            if (waitingforxn) {
              DATRW_assert((rframe.ctrl()==SEED::SteimFrame::Fspecial),
                             "ERROR (reading MiniSEED record): "
                             "missing X0");
              x0=rframe.diff();
              rframe.next();
              DATRW_assert((rframe.ctrl()==SEED::SteimFrame::Fspecial),
                             "ERROR (reading MiniSEED record): "
                             "missing XN");
              xn=rframe.diff();
              rframe.next();
              d1=rframe.diff();
              rframe.next();
              waitingforxn=false;
              Mdata(isample)=x0;
              ++isample;
              sum=x0;
              /*
               * Calculate value of last sample of previous record in order to
               * provide means for a continuous stream of sample values.
               */
              Mxm1=x0-d1;
            }
            while (rframe.valid() && (isample < nsamples))
            {
              if (rframe.ctrl() != SEED::SteimFrame::Fspecial)
              {
                sum += rframe.diff();
                Mdata(isample)=sum;
                ++isample;
              }
              rframe.next();
            }
            pdata += SEED::SteimFrame::blocksize;
            delete pframe;
          } // while (pdata < int(block.bytesize()))

          // read next block
          if (iblock < nblocks) 
          { 
            is >> block; 
            if (is.bad())
            {
              DATRW_warning("MiniSEEDRecord::read",
                "input stream is bad!");
            }
            pdata=0;
            ++iblock;
          }
        } // while ((iblock <= nblocks) &&
          //        (isample < nsamples) &&
          //        ((iframe < nframes) || (!filetellsnframes)))
        // skip rest of blocks in record
        while (iblock < nblocks)
        {
          // read next block
          is >> block; 
          if (is.bad())
          {
            DATRW_warning("MiniSEEDRecord::read",
                          "input stream is bad!");
          }
          ++iblock;
        }

        /* consistency checks
         * ==================
         * Check for data integrity, consistency, plausibility,...
         */

        /* string constant containing hint to format modifiers 
         * (this is a very local constant, defined here becausing being used
         * three times)
         */
        const std::string CHintToFormatModifiers(
          "consider to use format modifiers \""
          + std::string(key::nonfatal) + "\" or \"" 
          + std::string(key::skipcheck) + "\"\n"
          "if you like to ignore this inconsistency");

        /* Check for usec field being in specified range
         * ---------------------------------------------
         *
         * On page 124 of "SEED Reference Manual, Standard for the Exchange of
         * Earthquake Data, SEED Format Version 2.4, August, 2012" with
         * respect to [1001] Data Extension Blockette (8 bytes):
         *
         * > field 4: BYTE: µsec has the data start time down to the
         * >   microsecond. The SEED format handles down to 100µsecs. This
         * >   field is an offset from that value. The recommended value is
         * >   from -50 to +49µsecs. At the users option, this value may be
         * >   from 0 to +99µsecs. 
         */
        if (Mhasblockette1001 && Mchecks.usec.docheck)
        {
          DATRW_nonfatal_assert(Mdebug.inconsistencies_are_not_fatal
                                || (!Mchecks.usec.fatal),
                                (Mblockette1001.iusec()>=-50)
                                && (Mblockette1001.iusec()<=99),
            "usec-value in MiniSEED record is out of specified range\n"
            "consistency check \"" << key::nsamples << "\" complains:\n"
            "usec-value in [1001] Data Extension Blockette: " 
            << DATRW_value(Mblockette1001.iusec()) << "\n"
            "specified range SEED Reference Manual, "
            "Version 2.4, August, 2012 (page 124):\n" 
            "The recommended value is from -50 to +49 usecs.\n"
            "At the users option, this value may be from 0 to +99 usecs.\n"
            << CHintToFormatModifiers);
        }

        /* Check for consistent number of samples.
         * ---------------------------------------
         *
         * Number of samples to be expected is provided in the header in
         * the fixed section data header. This value will be compared against
         * the number of samples actually provided in the logical record.
         */
        if (Mchecks.nsamples.docheck)
        {
          DATRW_nonfatal_assert(Mdebug.inconsistencies_are_not_fatal
                                || (!Mchecks.nsamples.fatal),
                                (isample==nsamples),
            "number of samples in MiniSEED record is inconsistent\n"
            "consistency check \"" << key::nsamples << "\" complains:\n"
            "number of samples announced in header: " 
            << DATRW_value(nsamples) << "\n"
            "number of samples actually used: " 
            << DATRW_value(isample) << "\n"
            << CHintToFormatModifiers);
        }

        /* Check for consistent data values.
         * ---------------------------------
         *
         * Page 142 of "SEED Reference Manual, Standard for the Exchange of
         * Earthquake Data, SEED Format Version 2.4, August, 2012":
         *
         * > The reverse integrating constant also provides for a quick data
         * > integrity check when compared with the last computed
         * > sample. A discrepancy indicates that the contents of the data are
         * > garbled.
         *
         * The sample value of the last sample in the record is compared against
         * the value of the reverse integration constant as provided in the
         * first data frame of the record.
         */
        if (Mchecks.data.docheck)
        {
          DATRW_nonfatal_assert(Mdebug.inconsistencies_are_not_fatal
                                || (!Mchecks.data.fatal),
                                (Mdata(isample-1)==xn),
            "data in MiniSEED record are inconsistent\n"
            "consistency check \"" << key::data << "\" complains:\n"
            "expected value of last sample "
            "(i.e. reverse integration constant): " 
            << DATRW_value(xn) << "\n"
            "value of last sample ("
            << DATRW_value(isample) << ") actually read: " 
            << DATRW_value(Mdata(isample-1)) << "\n"
            << CHintToFormatModifiers);
        }

        /* Check for consistent number of frames.
         * --------------------------------------
         *
         * Number of frames to be expected is provided in the header in
         * Blockette 1001. This value will be compared against the number of
         * frames actually used to store data.
         *
         * Skip this test if data file does not specify number of frames. If
         * data records are not completely filled with samples, there is no
         * reason, why we should have guessed the correct number of frames.
         */
        if (filetellsnframes && Mchecks.nframes.docheck)
        {
          DATRW_nonfatal_assert(Mdebug.inconsistencies_are_not_fatal
                                || (!Mchecks.nframes.fatal),
                                (iframe==nframes),
            "number of frames in MiniSEED record is inconsistent\n"
            "consistency check \"" << key::nframes << "\" complains:\n"
            "number of frames announced in header: " 
            << DATRW_value(nframes) << "\n"
            "number of frames actually used: " 
            << DATRW_value(iframe) << "\n"
            << CHintToFormatModifiers);
        }

        // finished successfully
        if (is.good()) { Mvalid=true; }
      }
    } // void MiniSEEDRecord::read(std::istream& is)

  } // namespace mseed

} // namespace datrw

/* ----- END OF mseedread_mseedrecord_read.cc ----- */
