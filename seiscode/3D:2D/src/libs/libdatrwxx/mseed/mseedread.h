/*! \file mseedread.h
 * \brief read mini-SEED files (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/07/2004
 * 
 * read mini-SEED files (prototypes)
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
 *  - 07/07/2006   V1.1   provide debug function when reading MiniSEED record
 *  - 22/07/2014   V1.2   thof: support new format modifier: estimateNframes
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_MSEEDREAD_H_VERSION

#define DATRW_MSEEDREAD_H_VERSION \
  "DATRW_MSEEDREAD_H   V1.2"

#include<new>
#include<datrwxx/seedstructs.h>
#include<libtime++.h>
#include<aff/series.h>
#include<datrwxx/mseed.h>

namespace datrw {

  /*! \brief all the stuff to read mini-SEED data
   *
   * \defgroup group_mseed Reading module for: mini-SEED data
   *
   * \sa
   *   \ref sec_mseed_concept,
   *   \ref sec_mseed_ascii
   */

  /*! \brief all the stuff to read mini-SEED data
   *
   * This module is predominantly coded to extract data from a SeisComP or
   * COMSERV system.
   *
   * \ingroup group_mseed
   */
  namespace mseed {

/*======================================================================*/
// reading classes

      /*! MiniSEED input block
       *
       * This class just encapsulates memory handling for data blocks of
       * different size.
       */
      class MiniSEEDblock {
        public:
          //! type of data block value
          typedef char Tvalue;
          //! use aff shared heap to store and copy block
          typedef aff::Series<Tvalue> Theap;
          //! standard block size for mini-SEED files (in SeisComP)
          const static int standard_block_size=0x200;
          // constructor allocates new memory
          MiniSEEDblock(const unsigned int& blocksize=standard_block_size);
          // return size of block
          unsigned int blocksize() const { return(Mblocksize); }
          // return byte size of block
          unsigned int bytesize() const 
          { return(this->blocksize()*sizeof(Tvalue)); }
          // return pointer to first data value
          Tvalue* block() const { return(Mblock.pointer()); }
          // return pointer to first data value
          Tvalue* block(const unsigned int& i) const
          { return(Mblock.pointer()+i); }
          // return pointer to any data value
          Tvalue operator[](const unsigned int& i) { return(Mblock(i)); }
        private:
          // pointer to block
          Theap Mblock;
          // size of block
          unsigned int Mblocksize;
      }; // MiniSEEDblock

      /*! The input operator for MiniSEEDblock allows to read data into a
       * MiniSEEdblock instance from any istream.
       */
      inline std::istream& operator >> (std::istream& is, MiniSEEDblock& mb)
      { is.read(mb.block(), mb.bytesize()); return(is); }

/*----------------------------------------------------------------------*/
      namespace util {

        std::string extractstring(const char* s, const int& l);

      } // namespace util
/*----------------------------------------------------------------------*/
      /*! debug modes
       *
       * These debug modes control the behaviour of the MiniSEEDRecord class.
       * They are meant to be usefull, when testing corrupt MiniSEED files. By
       * setting the flags it is possible to scan the files by preventing
       * inconsitencies from being fatal.
       */
      struct Debug {
        //! constructor sets default values
        Debug(const bool& nonfatalflag=false,
              const bool& raflag=false):
          inconsistencies_are_not_fatal(nonfatalflag) ,
          report_ascii_data_to_stdout(raflag) 
          { }
        //! make inconsistencies non fatal
        bool inconsistencies_are_not_fatal;
        //! report ASCII lines
        bool report_ascii_data_to_stdout;
      }; // struct Debug

/*----------------------------------------------------------------------*/

      /*! MiniSEED class for decoded data
       *
       * This class provides the interface to read one MiniSEED record
       * including all data sample frames (if requested).
       *
       * The read or the skipdata function have to be called after
       * initialization of the record object. Prior to reading data the
       * object's contents are not valid, which is indicated by the return
       * value of the valid function.
       *
       * If debug modes are requested, they have to be passed on the instance
       * of creation.
       */
      class MiniSEEDRecord {
        public:
          //! type of sample data
          typedef int Tvalue;
          //! type of container for sample data
          typedef aff::Series<Tvalue> Tseries;
          MiniSEEDRecord(const Debug& d=Debug(false, false)):
            Mdebug(d),
            Mvalid(false),
            Mhasblockette1000(false),
            Mhasblockette1001(false),
            MestimateNframes(false),
            Mchecks(true, true) { }
          //! read a full MiniSEED record including data samples
          void read(std::istream& is);
          //! only read header and blockettes, no data
          void skipdata(std::istream& is);
          //! true if Data Only SEED Blockette is present
          bool hasblockette1000() const { return(Mhasblockette1000); }
          //! true if Data Extension Blockette is present
          bool hasblockette1001() const { return(Mhasblockette1001); }
          //! returns Fixed Data Record Header
          const SEED::FixedDataRecordHeader&
            recordheader() const { return(Mrecordheader); }
          //! returns Data Only SEED Blockette
          const SEED::DataOnlySEEDBlockette&
            blockette1000() const { return(Mblockette1000); }
          //! returns Data Extension Blockette
          const SEED::DataExtensionBlockette&
            blockette1001() const { return(Mblockette1001); }
          //! return last sample of previous record
          int xm1() const { return(Mxm1); }
          //! true if record was successfully read
          bool valid() const { return(Mvalid); }
          /*! return data samples (meaningless after skipdata)
           *
           * Notice that a refernce to the array is passed, that will be
           * reused by the instance to read the next block.
           */
          Tseries data() const;
          //! return time of first sample
          libtime::TAbsoluteTime date() const;
          //! number of samples
          unsigned int nsamples() const
          { return(this->recordheader().nsamp); }
          //! return sampling interval
          double dt() const;
          //! return station code
          std::string station() const
          { return(util::extractstring(this->recordheader().stat, 5)); }
          //! return channel code
          std::string channel() const
          { return(util::extractstring(this->recordheader().chan, 3)); }
          //! return location code
          std::string location() const
          { return(util::extractstring(this->recordheader().loc, 2)); }
          //! return network code
          std::string network() const
          { return(util::extractstring(this->recordheader().net, 2)); }
          //! return reference to debug flags
          Debug& debug() { return(Mdebug); }
          //! adjust flag for estimation of frame count
          void estimateNframes(const bool& flag)
          { MestimateNframes=flag; }
          //! adjust flags consistency checks
          void checks(const ConsistencyChecks& flags)
          { Mchecks=flags; }
        private:
          //! read header and return block
          MiniSEEDblock readheader(std::istream& is);
          //! debug options
          Debug Mdebug;
          //! contains valid data
          bool Mvalid;
          //! Data Only SEED Blockette is present
          bool Mhasblockette1000;
          //! Data Extension Blockette is present
          bool Mhasblockette1001;
          //! estimate number of frames
          bool MestimateNframes;
          //! flags controlling consistency checks
          ConsistencyChecks Mchecks;
          //! Fixed Data Record Header
          SEED::FixedDataRecordHeader Mrecordheader;
          //! Data Only SEED Blockette
          SEED::DataOnlySEEDBlockette Mblockette1000;
          //! Data Extension Blockette
          SEED::DataExtensionBlockette Mblockette1001;
          //! Expected value of last sample in previous block
          int Mxm1;
          //! Container for sample data
          Tseries Mdata;
      }; // MiniSEEDRecord

      /*! read a MiniSEED record from a stream
       */
      inline std::istream& operator >> (std::istream& is, MiniSEEDRecord& mr)
      { mr.read(is); return(is); }
      
/*======================================================================*/
// functions and operators

      //! convert BTIME structure to libtime structure
      libtime::TAbsoluteTime convert(const SEED::BTIME& t);

      //! check for bytesec
      bool needswap(const unsigned char& bytesex);

      //! calculate sampling interval from srate and smult
      double samplinginterval(const short int& srate,
                              const short int& srmult);

      //! function to print online help
      void help(std::ostream& os=std::cout);

  } // namespace mseed

} // namespace datrw

#endif // DATRW_MSEEDREAD_H_VERSION (includeguard)

/* ----- END OF mseedread.h ----- */
