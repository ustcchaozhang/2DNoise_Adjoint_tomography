/*! \file wid2container.h
 * \brief container for WID2 data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2014
 * 
 * container for WID2 data (prototypes)
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
 *  - 05/07/2014   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_WID2CONTAINER_H_VERSION

#define TF_WID2CONTAINER_H_VERSION \
  "TF_WID2CONTAINER_H   V1.0   "

#include<libtime++.h>
#include<sffxx.h>

namespace fapidxx {

  /*! \brief This struct is used to pass WID2 line data within libfapidxx.
   *
   * The WID2 line character string used in libsff is not able to hold the
   * full content of an ::sff::WID2 structure without loss of information.
   * Since libfapidxx provides interfaces to other data formats than SFF which
   * are able to represent the full ::sff::WID2 structure content in their
   * trace headers, we use the fapidxx::WID2container to pass WID2 data
   * internally.
   *
   * fapidxx::WID2container supports handling (reading, writing, encoding,
   * decoding of WID2). 
   * Instructions on application of fapidxx::WID2container are given in
   * section \ref sec_comments_wid2
   *
   * \sa \ref sec_comments_prepwid2 and \ref sec_comments_wid2
   */
  struct WID2container {

    /*! \brief This struct ist used by fapidxx::WID2container to store WID2 line data.
     *
     * The WID2::struct member data is written in binary for (memcpy) to the
     * WID2 line Fortran string.
     *
     * \sa \ref sec_comments_wid2
     * 
     * The WID2struct member data comprises:
     * <TABLE>
     *  <TR>
     *    <TH>item</TH>
     *    <TH>size</TH>
     *    <TH>total size</TH>
     *  </TR><TR>
     *   <TD>ID</TD><TD>4 bytes</TD><TD>4 bytes</TD>
     *  </TR><TR>
     *   <TD>date</TD><TD>8*4 bytes</TD><TD>32 bytes</TD>
     *  </TR><TR>
     *   <TD>1 int</TD><TD>4 bytes</TD><TD>4 bytes</TD>
     *  </TR><TR>
     *   <TD>5 double</TD><TD>5*4 bytes</TD><TD>20 bytes</TD>
     *  </TR><TR>
     *   <TD>character sequences</TD><TD> </TD><TD>22 bytes</TD>
     *  </TR><TR>
     *   <TD>station</TD><TD>6 bytes</TD><TD> </TD>
     *  </TR><TR>
     *   <TD>channel</TD><TD>4 bytes</TD><TD> </TD>
     *  </TR><TR>
     *   <TD>auxid</TD><TD>5 bytes</TD><TD> </TD>
     *  </TR><TR>
     *   <TD>instype</TD><TD>7 bytes</TD><TD> </TD>
     *  </TR><TR>
     *   <TD>total</TD><TD> </TD><TD>82 bytes</TD>
     *  </TR>
     * </TABLE>
     *
     * Two member functions are provided to fill data member field with WID2
     * data from an ::sff::WID2 struct or to pass WID2 data from this struct
     * to an ::sff::WID2 struct:
     *  - WID2struct::set sets the fields by reading an ::sff::WID2 struct
     *  - WID2struct::get returns data enclosed in an ::sff::WID2 struct
     */
    struct WID2struct {
      // static constants
      static const char* encodeID;
      static const unsigned short idlen=4;
      static const unsigned short slen=5;
      static const unsigned short clen=3;
      static const unsigned short alen=4;
      static const unsigned short ilen=6;
      // data fields
      char             ID[idlen];     //!< ID string
      libtime::timeint year;          //!< date of first sample (year value)
      libtime::timeint month;         //!< date of first sample (month value)
      libtime::timeint day;           //!< date of first sample (day value)
      libtime::timeint hour;          //!< time of first sample (hour)
      libtime::timeint minute;        //!< time of first sample (minute)
      libtime::timeint second;        //!< time of first sample (second)
      libtime::timeint milsec;        //!< time of first sample (millisecond)
      libtime::timeint micsec;        //!< time of first sample (microsecond)
      char             station[slen+1]; //!< Station code
      char             channel[clen+1]; //!< FDSN channel code
      char             auxid[alen+1];   //!< Auxiliary identification code
      int              nsamples;      //!< number of samples
      double           dt;            //!< sampling interval (sec)
      double           calib;         //!< calibration factor
      double           calper;        //!< calibration reference period
      char             instype[ilen+1]; //!< instrument type
      double           hang;          //!< horizontal orientation
      double           vang;          //!< veritcal orientation
      // member functions
      //! set values from WID2 
      void set(const ::sff::WID2& wid2);
      //! return values to WID2
      ::sff::WID2 get() const;
    }; // struct WID2struct
    //! \brief construct from C++ string
    WID2container(const std::string& line) 
    { setwid2(line); wasascii=true; }
    //! \brief construct from Fortran character sequence
    WID2container(char *fstring, ftnlen slen) 
    { setwid2(fstring, slen); }
    //! \brief construct from an SFF WID2 container
    WID2container(const ::sff::WID2& iwid2): 
      wid2(iwid2), wasascii(false) { }
    //! \brief fill data fields from a conventional WID2 line
    void setwid2(const std::string& line);
    //! \brief fill data fields from a character string
    void setwid2(char *fstring, ftnlen slen);
    //! \brief encode data into a binary character string
    void encodebinary(char *fstring, ftnlen slen) const;
    //! \brief encode data into a standard SFF character string
    void encodeascii(char *fstring, ftnlen slen) const;
    //! \brief encode data into a character string depending on wasascii flag
    void encode(char *fstring, ftnlen slen) const;
    //! \brief WID2 data
    sff::WID2 wid2;
    //! \brief true, if original data was ASCII encoded
    bool wasascii;
  }; // struct WID2container {

} // namespace fapidxx

#endif // TF_WID2CONTAINER_H_VERSION (includeguard)

/* ----- END OF wid2container.h ----- */
