/*! \file suheader.h
 * \brief handle a Seismic Unix trace header (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/11/2010
 * 
 * handle a Seismic Unix trace header (prototypes)
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/11/2010   V1.0   Thomas Forbriger
 *  - 21/01/2012   V1.1   provide online help regarding header fields
 *  - 22/01/2012   V1.2
 *                        - indicate ultrasonic data
 *                        - renamed absdelay -> absdelrt
 *  - 23/01/2012   V1.3
 *                        - added member data Mdt and some comments
 *  - 08/07/2016   V1.4
 *                        - make exception argument types match base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SUHEADER_H_VERSION

#define DATRW_SUHEADER_H_VERSION \
  "DATRW_SUHEADER_H   V1.4"

#include<iostream>
#include<datrwxx/suformat.h>
#include<datrwxx/suheaderstruct.h>
#include<datrwxx/error.h>
#include<sffxx.h>
#include<libtime++.h>

namespace datrw  {


  /*! \brief All classes and functions for Seismic Unix file data I/O
   * 
   * Seismic Un*x is available from
   * http://www.cwp.mines.edu/cwpcodes/
   *
   * \defgroup group_su I/O module for Seismic Uni*x data
   *
   * \section sec_su_for_TOAST Special format definition within TOAST
   *
   *  Small sampling interval (smaller than 1 microsecond) as used in
   *  ultrasonic recordings are to be stored in SeismicUn*x data format
   *  within TOAST as follows:
   *
   *   -# Time values for ultrasonic data will be given in nanoseconds for dt
   *      and microseconds for delrt.
   *   -# This applies to fields
   *      -# dt (byte# 117-118): sample interval
   *      -# delrt (byte# 109-110): delay recording time
   *   .
   *   -# Time units other than microseconds or nanoseconds for dt and
   *      milliseconds or microseconds for delrt are not allowed.
   *   -# Field d1 (byte# 181-184) in TOAST data will be
   *      - either zero, indicating standard seismic data with a micro seconds
   *        time scale
   *      - or provide the sampling interval in seconds, thus indicating
   *         - either standard seismic data, if d1 in seconds matches dt if
   *           the latter is taken in microseconds
   *         - or ultrasonic data, if d1 in seconds matches dt if the latter
   *           is taken in nanoseconds and delrt is taken in microseconds
   *
   *  This way it will be possible to use SU tools to sort traces or similar
   *  or even waveform filters.
   *  The user just has to take care to pass filter frequencies in kHz rather
   *  than Hz in the case of ultrasonic data.
   *
   */

  /*! \brief All classes and functions to extract data from Seismic Unix files
   *
   * \ingroup group_su
   */
  namespace su {

    /*!
     * \ingroup group_su
     */
    class SUReadException: public datrw::Exception
    {
      public:
        //! Create with message, failed assertion, and code position
        SUReadException(const std::string message, 
                        const std::string file,
                        const int& line,
                        const std::string condition)
          : ::datrw::Exception(message, file, line, condition) { }
        //! provide explicit virtual destructor
        virtual ~SUReadException() { }
    }; // class SUReadException

    /*----------------------------------------------------------------------*/

    /*! \brief C++ class to handle Seismic Un*x header struct
     * \ingroup group_su
     */
    class SUheader {
      public:
        //! constructor
        SUheader(const datrw::su::options::SUHeaderControl& ch,
                 const bool& debug=false);
        //! set control parameters
        void set(const datrw::su::options::SUHeaderControl& hc)
        { Mheadercontrol=hc; }
        //! clear header struct
        void clear();
        //! set defaults to struct
        void setdefaults();

        /*! \name in/out functions
         */
        ///@{
        //! read struct from file
        void read(std::istream& is);
        //! write struct to file
        void write(std::ostream& os) const;
        ///@}

        /*! \name query functions
         */
        ///@{
        //! return sampling interval
        double dt() const;
        //! true if header defines sampling of ultrasonic data
        bool isultrasonic() const;
        //! return factor defined by scalel
        double scalelf() const;
        //! return factor defined by scalco
        double scalcof() const;
        //! return date of first sample
        libtime::TAbsoluteTime dateoffirstsample() const;
        //! recording delay
        libtime::TRelativeTime delay() const;
        //! is delay positive recording delay
        bool delayispositive() const;
        //! absolute (always positive) delay value
        int absdelrt() const;
        //! return date of shot
        libtime::TAbsoluteTime dateofshot() const;
        //! return SRCE line
        ::sff::SRCE srce() const;
        //! return INFO line
        ::sff::INFO info() const;
        //! return WID2 line
        ::sff::WID2 wid2() const;
        ///@}

        /*! \name set functions
         *
         * Some calculations have to be redone when new information is added
         * to the header.
         * For this reason the set functions place part of the data in a local
         * member data store area (see the store area ind the member data
         * section).
         * The function SUheader::settimes() then is called at the end of each
         * set function body in order to coherently evaluate values collected
         * from SRCE line and WID2 line header components.
         */
        ///@{
        //! set values from SRCE line
        void set(const ::sff::SRCE &srce);
        //! set values from INFO line
        void set(const ::sff::INFO &info);
        //! set values from WID2 line
        void set(const ::sff::WID2 &wid2);
        //! set date
        void set(const libtime::TAbsoluteTime &date);
        //! set time values correctly
        void settimes();
        ///@}
          
        //! print online help regarding header fields and TOAST data
        static void help(std::ostream& os);
      private:
        //! all options taken from the user
        datrw::su::options::SUHeaderControl Mheadercontrol;
        //! be verbose
        bool Mdebug;

        /*! \name set functions store area
         *
         * Set functions receive header data from SFF header components like
         * WID2, SRCE, and INFO.
         * The set functions have to scale dt and delrt appropriatly for
         * seismic and ultrasonic data, respectively.
         * Further they have to derive the recording delay from the time
         * interval between source and receiver time, the first being
         * presented in SRCE, the latter being presented in WID2.
         * Once new information is added by calling one of the set functions,
         * some of the calculations must be redone. 
         * The original values are kept in this store area for this purpose.
         *
         * \note
         * This store area is only used for setting values in the header.
         * If header is read from file, the store area values are undefined.
         */
        ///@{
        //! we received srce data
        bool Mhassrce;
        //! we received wid2 data
        bool Mhaswid2;
        //! we received info data
        bool Mhasinfo;
        //! date from SRCE line
        libtime::TAbsoluteTime Msrcedate;
        //! date from WID2 line
        libtime::TAbsoluteTime Mwid2date;
        //! intermediately store sampling interval from wid2 line
        double Mwid2dt;
        ///@}
      public:
        //! the actual data fields are provided for public access
        TraceHeaderStruct Mheader;
    }; // class SUheader

  } // namespace su

} // namespace datrw

#endif // DATRW_SUHEADER_H_VERSION (includeguard)

/* ----- END OF suheader.h ----- */
