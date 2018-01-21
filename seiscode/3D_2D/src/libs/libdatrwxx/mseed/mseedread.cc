/*! \file mseedread.cc
 * \brief read mini-SEED files (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/07/2004
 * 
 * read mini-SEED files (implementation)
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
 *  - 15/07/2004   V1.0   Thomas Forbriger (thof)
 *  - 04/04/2006   V1.1   use Motorola byte order for header fields as
 *                        standard byte order
 *  - 28/04/2006   V1.2   - long int appears to be of 
 *                          implementation dependent size
 *                        - provide explicit conversion of BTIME fields
 *  - 09/05/2006   V1.3   use general Steim 1 and 2 code
 *  - 07/07/2006   V1.4   provide debug function when reading MiniSEED record
 *  - 10/07/2006   V1.5   skip special words within data stream
 *  - 29/06/2007   V1.6   no longer use libtfxx
 *  - 11/09/2007   V1.7   added help function
 *  - 11/11/2009   V1.8   requires header cstring
 *  - 13/09/2011   V1.9   silently skip ASCII blocks
 *  - 07/05/2014   V1.10  accept all type field indicators in the fixed
 *                        section of data header as defined by SEED V2.4
 *  - 08/05/2014   V1.11  add format modifier ttolerance to help
 *                        check for consistent number of frames
 *  - 12/05/2014   V1.12  do not read more than the indicated number of frames
 *                        or samples
 *  - 22/07/2014   V1.13  thof: 
 *                        - move member fucntions
 *                          MiniSEEDRecord::readheader and MiniSEEDRecord::read
 *                          to dedicated compilation units
 *                        - support new format modifier: estimateNframes
 *  - 23/06/2016   V1.14  thof:
 *                        - move member functions of MiniSEEDRecord to
 *                          separate compilation units
 *  - 05/07/2016   V1.15  thof:
 *                        - move help function to separate compilation unit
 *                        - do not provide stand-alone help function in
 *                          namespace mseed
 * 
 * ============================================================================
 */
#define DATRW_MSEEDREAD_CC_VERSION \
  "DATRW_MSEEDREAD_CC   V1.15"

#include <string>
#include <cstring>
#include <string.h>
#include <datrwxx/mseedread.h>
#include <datrwxx/mseed.h>
#include <datrwxx/bytesex.h>

namespace datrw {

    /*! 
     * \section sec_mseed_concept Concept of reading MiniSEED files
     *
     * This module is designed to read data from SeisComP an there from the
     * old version used in the GRSN (with comserv client datalog) as well as
     * the new version (using seedlink). Further it will read EDL (Earth Data
     * Logger) MiniSEED files. Their formats differ in block size and MiniSEED
     * data files that are written by comserv contain telemetry volume headers
     * additionally.
     *
     * If we provide a stream like class, that is constructed from an istream
     * object and returns a full data block at once (containing fixed data
     * header, blockette 1000 and blockette 1001 and time series samples; the
     * latter preferably in an STL vector), we can hide all bytesex and
     * blocksize stuff inside this class.
     *
     * This stream class has to skip the telemetry volume header if the file
     * starts with one. And it has to check the bytesex of the data, which is
     * coded therein, but which is only available from blockette 1000, which
     * is only available after some reading. The data record length is also
     * only available from blockette 1000. Thus reading this informations
     * would already require to know this information. Thus there will be some
     * guessing and testing be involved and in particular some strong
     * assumptions about the blockette structure used in the file.
     * Fortunately the SEED manual defines on page 12, that the common byte
     * order for all SEED header fields is big-endian (Motorola). Only SEED
     * data fields my by different. Their byte order is specified in the Data
     * Only SEED Blockette in the case of MiniSEED data.
     *
     * Notice: Data written by EarthDataLoggers is not consistent with the
     * definition of the byte order of header fields in SEED data. EDL prefer
     * to write everything in Intel byte order :-( The code has to check for
     * this, which is done in member function 
     *   datrw::mseed::MiniSEEDRecord::readheader(std::istream& is)
     * as well as in the test program provided in file mseedtest.cc
     *
     * We will use the basic structures defined in namespace SEED to hold the
     * data. However, it will be necessary to put some envelope around these
     * structure in particular to indicate if a blockette was actually read
     * from a dataset or not (because it will be present in the returned data
     * block structure in any case).
     *
     * \section sec_mseed_ascii Howto handle ASCII data
     * In data streamed directly from Q330HR data acquisition systems,
     * there may ASCII blocks be present containing system logs.
     * This ASCII data is handled in 
     *   datrw::mseed::MiniSEEDRecord::readheader(std::istream& is)
     * which just skips the ASCII block and reads until the next binary data
     * block.
     * If it reaches EOF datrw::mseed::MiniSEEDRecord::Mvalid is turned to
     * false as usual with all other reading operations.
     *
     * \date 13.9.2011
     */
  namespace mseed {

    namespace util {

      /*! \brief Return a maximum of \p l characters.
       *
       * \param[in] s input string (character array)
       * \param[in] l maximum number of characters to return
       * \return string object containing not more than \p l characters of
       *         the input sequence 
       */
      std::string extractstring(const char* s, const int& l)
      {
        int slen=std::strlen(s);
        slen = slen < 0 ? l : slen;
        slen = slen < l ? slen : l;
        return(std::string(s, slen));
      } // std::string extractstring(const char* s, const int& l)

    } // namespace util

    /*! contructor of MiniSEEDblock allocates memory of requested block size.
     * In case this fails, an exception is thrown.
     */
    MiniSEEDblock::MiniSEEDblock(const unsigned int& blocksize):
      Mblocksize(blocksize) 
      {
        try { Mblock=Theap(blocksize); }
        catch (std::bad_alloc) {
          DATRW_abort("MiniSEEDblock: could not allocate memory!");
        }
      } // MiniSEEDblock::MiniSEEDblock

    /*----------------------------------------------------------------------*/
      
    //! convert BTIME structure to libtime structure
    libtime::TAbsoluteTime convert(const SEED::BTIME& t)
    {
      int milsec=int(int(t.tmilsec)/10);
      int micsec=100*(int(t.tmilsec)-milsec*10);
      libtime::TAbsoluteTime retval(int(t.year), 1, 1, 
                                    int(t.hour), int(t.min), int(t.sec),
                                    int(milsec), int(micsec));
      retval.setdoy(int(t.doy));
      return(retval);
    } // libtime::TAbsoluteTime convert(const SEED::BTIME& t)

    /*----------------------------------------------------------------------*/
      
    //! calculate sampling interval from srate and smult
    double samplinginterval(const short int& srate,
                            const short int& srmult)
    {
      double fac1 = srate < 0 ? -double(srate) : 1./double(srate);
      double fac2 = srmult < 0 ? -double(srmult) : 1./double(srmult);
      return(fac1*fac2);
    } // double samplinginterval(const short int& srate,
      //                         const short int& smult);

    /*----------------------------------------------------------------------*/
      
    //! check bytesex
    bool needswap(const unsigned char& bytesex)
    {
      // my CPU type
      datrw::util::Ecpu_type mysex=datrw::util::cpu();
      // file type
      datrw::util::Ecpu_type filesex=datrw::util::cpu_unknown;
      // file type
      SEED::EByteOrder byteorder=SEED::EByteOrder(int(bytesex));
      if (byteorder == SEED::vax8086)
      { filesex=datrw::util::cpu_Intel; }
      else if (byteorder == SEED::sparc68000)
      { filesex=datrw::util::cpu_Motorola; }
      bool retval=(filesex != mysex);
      return(retval);
    } // needswap(const unsigned char& bytesex)

  } // namespace mseed

} // namespace datrw

/* ----- END OF mseedread.cc ----- */
