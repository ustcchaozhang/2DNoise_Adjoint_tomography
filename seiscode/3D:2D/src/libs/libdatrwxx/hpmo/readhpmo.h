/*! \file readhpmo.h
 * \brief read data from Walter Grossmann file format (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/03/2004
 * 
 * read data from Walter Grossmann file format (prototypes)
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
 *  - 31/03/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_READHPMO_H_VERSION

#define DATRW_READHPMO_H_VERSION \
  "DATRW_READHPMO_H   V1.0   "

#include<iostream>
#include<string>
#include<vector>
#include<libtime++.h>
#include<sffxx.h>
#include<datrwxx/datread.h>
#include<datrwxx/hpmodata.h>

namespace datrw {

  /*! \brief All classes and functions to extract data from HPMO files
   *
   * \defgroup group_hpmo Reading module for: HPMO data 
   */

  /*! \brief All classes and functions to extract data from HPMO files
   *
   * \ingroup group_hpmo
   */
  namespace hpmo {

    /*! \brief within the inner reading functions, we use vector to hold
     * minute blocks
     *
     * \ingroup group_hpmo
     */
    typedef std::vector<MinuteBlock> Tvecofblocks;

/*----------------------------------------------------------------------*/

    /*! \brief this excpetion will be thrown by the Header reading function,
     * in case it does not find something that look like a header (i.e. a line
     * with more or less than 44 characters)
     *
     * \ingroup group_hpmo
     */
    class NoHeaderException: public datrw::Exception {
      public:
        NoHeaderException(): Exception("no header found!") { }
    }; // class NoHeaderException

/*----------------------------------------------------------------------*/

    /*! \brief Read, hold and handle one minute block header line
     *
     * \ingroup group_hpmo
     */
    class Header {
      public:
        Header(): Merrorflag(16) { }
        Header(std::istream& is, const bool& verbose=false) 
          { this->readheader(is, verbose); }
        void readheader(std::istream& is, const bool& verbose=false);
        libtime::TAbsoluteTime time() const { return(Mtime); }
        int errorflag() const { return(Merrorflag); }
        std::string errorstring() const;
        std::string headerline() const { return (Mline); }
        void dump(std::ostream& os) const;
      private:
        libtime::TAbsoluteTime Mtime;
        int Merrorflag;
        std::string Mline;
    }; // class Header

/*----------------------------------------------------------------------*/

    /*! \brief read one minute block of samples
     *  \ingroup group_hpmo
     */
    SampleBlock readdata(std::istream& is, const bool& verbose=false);

    //! dump one block of samples
    void dump(std::ostream& os, const SampleBlock& data);

/*----------------------------------------------------------------------*/

    /*! \brief dump one MinueBlock
     *  \ingroup group_hpmo
     */
    void dump(std::ostream& os, const MinuteBlock& block);

/*----------------------------------------------------------------------*/

    //! read a full data file
    Tvecofblocks readfile(std::istream& is, const bool& verbose=false);

/*----------------------------------------------------------------------*/
/* some general functions */

    //! return sampling interval of HPMO data acquisition (i.e. 5 sec)
    libtime::TRelativeTime dt();

    //! return time offset for channel \p ichannel (due to multiplexer)
    libtime::TRelativeTime toffset(const int& ichannel);

    //! check if channel number is valid
    void check_channel_no(const int& ichannel);

    //! return meaning of quality flag
    std::string quality(const int& flag);

    //! prepare a report on unusual quality reports
    sff::FREE qualityreports(const MinuteBlock* blocks, const int& nblocks);

/*----------------------------------------------------------------------*/

    //! read header from C++ stream
    inline std::istream& operator >> (std::istream& is, Header& hd)
    { hd.readheader(is); return(is); }

    //! read a sample block from C++ stream
    inline std::istream& operator >> (std::istream& is, SampleBlock& data)
    { data=readdata(is); return(is); }

    //! read a full minute block from C++ stream
    std::istream& operator >> (std::istream& is, MinuteBlock& block);

/*----------------------------------------------------------------------*/

    //! dump values of a sample block
    inline std::ostream& operator << (std::ostream& os, 
                                      const SampleBlock& data)
    { dump(os, data); return(os); }

    //! dump header values
    inline std::ostream& operator << (std::ostream& os, const Header& hd)
    { hd.dump(os); return(os); }

    //! dump a minute block
    inline std::ostream& operator << (std::ostream& os, 
                                      const MinuteBlock& block)
    { dump(os, block); return(os); }

  } // namespace hpmo

} // namespace datrw

#endif // DATRW_READHPMO_H_VERSION (includeguard)

/* ----- END OF readhpmo.h ----- */
