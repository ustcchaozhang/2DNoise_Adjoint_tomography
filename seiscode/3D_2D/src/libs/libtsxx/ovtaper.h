/*! \file ovtaper.h
 * \brief offset variable taper (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/01/2012
 * 
 * offset variable taper (prototypes)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 26/01/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TSXX_OVTAPER_H_VERSION

#define TSXX_OVTAPER_H_VERSION \
  "TSXX_OVTAPER_H   V1.0   "

#include <iostream>
#include <list>
#include <tsxx/tsxx.h>
#include <tsxx/tapers.h>
#include <tsxx/dttimeseries.h>

namespace ts {

  namespace tapers {

    /*! \brief bits and pieces required by OffsetVariableTaper
     */
    namespace ovtaper {

      //! a single pick
      struct Pick {
        Pick(): t(0), x(0) { }
        Pick(const double& offset): t(0), x(offset) { }
        double t; //!< time
        double x; //!< offset
      }; // struct Pick

      /*----------------------------------------------------------------------*/

      inline
      bool operator<(const Pick& p1, const Pick& p2) { return (p1.x<p2.x); }
      inline
      bool operator>(const Pick& p1, const Pick& p2) { return (p1.x>p2.x); }

      /*----------------------------------------------------------------------*/

      //! a sequence of picks
      class Picks {
        public:
          typedef std::list<Pick> Tlistofpick;
          Picks(const bool& debug): Mdebug(debug) { }
          //! read from file in refract taper file format
          void read(std::istream& is);
          //! return interpolated pick for given offset
          Pick pick(const double& offset) const;
          //! return time for interpolated pick at given offset
          double time(const double& offset) const;
        private:
          //! produce debug output if true
          bool Mdebug;
          //! picks
          Tlistofpick Mpicks;
      }; // class Picks

    } // namespace ovtaper

    /*----------------------------------------------------------------------*/

    /*! \brief Offset variable taper (refract taper).
     *
     * This class handles FourPoint tapers varying with receiver offset as can
     * be defined by refract.
     */
    class OffsetVariableTaper {
      public:
        //! type of sample values
        typedef double Tvalue;
        //! type of series container
        typedef ts::TDtimeseries Ttimeseries;
        typedef Ttimeseries::Tseries Tseries;
        typedef Ttimeseries::Theader Theader;
        OffsetVariableTaper(const bool& debug=false)
          : Mdebug(debug), Mvalid(false),
            Mt1(debug), Mt2(debug), Mt3(debug), Mt4(debug)
      { }

        /*! return specific four point taper
         *
         * \param offset create specific taper for receiver at offset
         * \param T0 time of first sample with respect to source time
         * \param T duration of time series to be tapered
         * \return four point taper for this offset and time series
         */
        ts::tapers::FourPoint taper(const double& offset,
                                    const double& T0,
                                    const double& T) const;

        /*! read taper definition from input stream is
         */
        void read(std::istream& is);

        /*! read taper definition from file
         */
        void read(const std::string& filename);

        ovtaper::Picks t1() const { return Mt1; }
        ovtaper::Picks t2() const { return Mt2; }
        ovtaper::Picks t3() const { return Mt3; }
        ovtaper::Picks t4() const { return Mt4; }
      private:
        //! produce debug output if true
        bool Mdebug;
        //! true if taper definition is present
        bool Mvalid;
        //! taper picks
        ovtaper::Picks Mt1, Mt2, Mt3, Mt4;
    }; // class OffsetVariableTaper

  } // namespace tapers

} // namespace ts

#endif // TSXX_OVTAPER_H_VERSION (includeguard)

/* ----- END OF ovtaper.h ----- */
