/*! \file ipo.h
 * \brief interpolation interface (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/11/2017
 * 
 * interpolation interface (prototypes)
 * 
 * Copyright (c) 2005, 2017 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 13/07/2005   V1.0   Thomas Forbriger
 *  - 28/07/2005   V1.1   indicate wrong time window by specific exception
 *  - 07/11/2017   V1.2   
 *                        - provide new exception to indicate empty time window
 *                        - provide option for automatic adjustment of time
 *                          window in function resample
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_IPO_H_VERSION

#define TF_IPO_H_VERSION \
  "TF_IPO_H   V1.2"

#include<tsxx/tsxx.h>
#include<tsxx/error.h>
#include<tsxx/wid2timeseries.h>

namespace ts {

  /*! \brief time series interpolation
   *
   * \defgroup ipo Time series interpolation
   */

  /*! \brief time series interpolation
   *
   * This namespace provides tools for time series interpolation
   *
   * \ingroup ipo
   */
  namespace ipo {

    /*! \brief Exception in case time windows are not overlapping
     *
     * \ingroup ipo
     */
    class ExceptionTimeWindowOutside:
      public ts::error::Exception
      {
        public:
          ExceptionTimeWindowOutside(const char* message, 
                                     const char* file,
                                     const int& line,
                                     const char* condition);
      }; // class ExceptionTimeWindowOutside

    /*! \brief Exception in case resulting time window is empty
     *
     * This execption indicates that after the time window was shrinked, no
     * sample from the input time series is available within the defined time
     * window.
     *
     * \ingroup ipo
     */
    class ExceptionTimeWindowEmpty:
      public ts::error::Exception
      {
        public:
          ExceptionTimeWindowEmpty(const char* message, 
                                   const char* file,
                                   const int& line,
                                   const char* condition);
      }; // class ExceptionTimeWindowEmpty

    /*! \brief Interface to time series interpolator
     *
     * This is the interpolator base class. It is a virtual base class in that
     * the base class itself provides not functionality. It only defines the
     * common API for interpolators. Derived classes provide actual
     * interpolation through the virtual member operator
     * Interpolator::operator()(const libtime::TAbsoluteTime& t) which must be
     * implemented in the derived class.
     *
     * The interpolator holds the input time series as member data. In that
     * sense it can be considered a browser into the time series data. The
     * member operator can be used to read sample values for any time value
     * for which an interpolated sample value is desired.
     *
     * \ingroup ipo
     */
    class Interpolator {
      public:
        typedef ts::TDsfftimeseries Ttimeseries;
        typedef Ttimeseries::Tconsttimeseries Tconst_timeseries;
        typedef Tconst_timeseries::Tseries Tconst_series;
        typedef Tconst_timeseries::Theader Theader;
        typedef Ttimeseries::Tvalue Tvalue;
        virtual ~Interpolator() { }
        virtual Tvalue operator()(const libtime::TAbsoluteTime& t) const =0;
        Theader header() const { return(Mts.header); }
        Tconst_series series() const { return(Mts); }
        Tconst_timeseries timeseries() const { return(Mts); }
        //! indicate whether interpolator is in debug mode
        bool debug() const { return(Mdebug); }
      protected:
	  Interpolator(Tconst_timeseries ts, const bool& debug=false):
          Mts(ts), Mdebug(debug) { }
        //! here we hold a copy
        Tconst_timeseries Mts;
      private:
        //! produce debug output
        bool Mdebug;
    }; // class Interpolator

    /*! \brief function to resample data
     *
     * This function provides resampling on the base of a reference to an
     * instance of an interpolator class (containing the input time series)
     * and a definition of sampling in terms of the time of the first sample,
     * a sampling interval and the desired number of samples.
     *
     * Optionally this function adjusts the time window for which resampling
     * is done to a time span inside the time window for which input data is
     * available. In this case the time of the first sample is adjusted such
     * that it is offset from the originally requested time by an integer
     * number of sampling intervals.
     *
     * If the option \p shrink is not used, exception
     * ipo::ExceptionTimeWindowOutside will be thrown if not all of the
     * requested sample values are within the time window of the original
     * series.
     *
     * \param ip reference to a waveform interpolator
     * \param first time of first sample in new time series
     * \param dt sampling interval of new time series
     * \param n number of samples in new time series
     * \param shrink if true, shrink time window such, that input data is
     *          available for the whole window
     * \return resampled time series with time header fields adjusted
     *
     * \ingroup ipo
     */
    ts::TDsfftimeseries resample(const Interpolator& ip,
                                 const libtime::TAbsoluteTime& first,
                                 const libtime::TRelativeTime& dt,
                                 const int& n,
                                 const bool& shrink=false);

  } // namespace ipo

} // namespace ts

#endif // TF_IPO_H_VERSION (includeguard)

/* ----- END OF ipo.h ----- */
