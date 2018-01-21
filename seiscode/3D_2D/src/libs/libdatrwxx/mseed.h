/*! \file mseed.h
 * \brief provide mini-SEED data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/07/2004
 * 
 * provide mini-SEED data (prototypes)
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
 *  - 23/11/2010   V1.1   introduced static members
 *  - 13/09/2011   V1.2   support format modifiers for ASCII dump
 *  - 08/05/2014   V1.3   Add field for timing tolerance value
 *  - 22/07/2014   V1.4   thof: support new format modifier: estimateNframes
 *  - 05/07/2016   V1.5   thof: provide consistency check control flags
 *  - 12/07/2016   V1.6   thof: provide usec consistency check
 *  - 18/11/2016   V1.7   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_MSEED_H_VERSION

#define DATRW_MSEED_H_VERSION \
  "DATRW_MSEED_H   V1.7"

#include<datrwxx/datread.h>

namespace datrw {

  namespace mseed {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace mseed 
  
  namespace mseed {
    
    /*! Container for flags to control consistency checks
     *
     * \ingroup group_mseed
     */
    struct ConsistencyCheckControl {
      ConsistencyCheckControl(const bool& fatal, const bool& check):
        docheck(fatal), fatal(check) { }
      /*! \brief Activation of check.
       *
       * If true, the respective check shall be applied.
       */
      bool docheck;
      /*! \brief Consequences of check.
       *
       * If true, an exception is thrown, if the respective check fails.
       */
      bool fatal;
    }; // struct ConsistencyCheckControl

  /*----------------------------------------------------------------------*/
    
    /*! Container for flags to control consistency checks
     * \ingroup group_mseed
     */
    struct ConsistencyChecks {
      ConsistencyChecks(const bool& fatal, const bool& check):
        nframes(fatal, check), 
        nsamples(fatal, check), 
        data(fatal, check),
        usec(fatal, check) { }
      /*! \brief Check for consistent number of frames.
       *
       * Number of frames to be expected is provided in the header in
       * \ref page_mseed_blockette1001
       * This value will be compared against the number of frames actually
       * used to store data.
       */
      ConsistencyCheckControl nframes;
      /*! \brief Check for consistent number of samples.
       *
       * Number of samples to be expected is provided in the header in
       * \ref page_mseed_FixedSectionDataHeader
       * This value will be compared against the number of samples actually
       * provided in the logical record.
       */
      ConsistencyCheckControl nsamples;
      /*! \brief Check for consistent data values.
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
      ConsistencyCheckControl data;
      /*! \brief Check for usec field being in specified range.
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
      ConsistencyCheckControl usec;
    }; // struct ConsistencyChecks

    /*----------------------------------------------------------------------*/

    /*! structure to hold one MiniSEEDRecord in SFF format
     *
     * \ingroup group_mseed
     */
    struct Record {
      ::sff::WID2 wid2;
      ::sff::FREE free;
      Tiseries data;
      int xm1;
      bool valid;
      void read(std::istream& is,
                const bool& dumpascii=false,
                const bool& estimateNframes=false,
                const ConsistencyChecks& checks=ConsistencyChecks(true, true));
    }; // struct Record

  } // namespace mseed

  /*----------------------------------------------------------------------*/

  /*! \brief class to read mini-SEED data
   *
   * \ingroup group_mseed
   */
  class imseedstream: public idatstream {
    public:
      typedef idatstream Tbase;
      imseedstream(std::istream& is,
                   const std::string& modifier="",
                   const bool& debug=false);
      virtual ~imseedstream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      //! read file
      Tiseries read(const bool& skipdata=false);
      //! data read ahead
      datrw::mseed::Record Mrecord;
      std::string Mmodifier; //!< format modifier
      bool Mdumpascii; //!< dump ASCII data if true
      double Mttolerance; //!< timing tolerance in microseconds
      bool MestimateNframes; //!< estimate frame count
      //! flags controlling consistency checks
      datrw::mseed::ConsistencyChecks Mchecks;
  }; // class imseedstream

} // namespace datrw

#endif // DATRW_MSEED_H_VERSION (includeguard)

/* ----- END OF mseed.h ----- */
