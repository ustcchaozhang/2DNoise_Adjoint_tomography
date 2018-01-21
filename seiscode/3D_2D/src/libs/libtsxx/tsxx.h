/*! \file tsxx.h
 * \brief basic modules of time series library in C++ (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2003
 * 
 * basic modules of time series library in C++ (prototypes)
 * 
 * Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/12/2003   V1.0   Thomas Forbriger
 *  - 10/02/2004   V1.1   
 *                        - deleted all old code
 *                        - started from scratch with TimeSeries
 *  - 13/07/2005   V1.2   added exception
 *  - 28/04/2006   V1.3   added virtual destructor
 *  - 03/12/2008   V1.4   provide integer time series
 *  - 22/11/2016   V1.5   provide copy constructor and copy operator for 
 *                        TimeSeries
 *                 V1.6   move error handling code to error.h
 *                        move specific type definitions to
 *                        dttimeseries.h and wid2timeseries.h
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_TSXX_H_VERSION

#define TF_TSXX_H_VERSION \
  "TF_TSXX_H   V1.6"

#include<aff/series.h>

/*! \brief All stuff in this library will be placed within namespace ts
 */
namespace ts {

  /*! \brief Structure to hold the data samples of a series together with
   *         header information to form a time series.
   *
   * Relevant information to be contained in the header:
   *   - date and time of first sample
   *   - sampling interval
   *   - station, channel, etc. identifiers
   *
   * The first two are necessary for time series processing, like filtering or
   * application of tapers and windows.
   * The third is relevant for keeping track of the data source.
   * The most natural choice for the header structure appears to be an
   * sff::WID2 header.
   * Consequently we provide typdefs for these combinations of samples and
   * header.
   *
   * \note
   * ts::TimeSeries is derived from the series base class. 
   * Thus TimesSeries \b is a full series and can directly be fed to functions
   * that take series as their arguments.
   *
   * There are no hidden (private) parts of this class.
   * Consequently it is defined to be a struct.
   */
  template<class S, class H>
    struct TimeSeries: public S
    {
      public:
        /*! \name Type definitions.
         *
         * Header data and series shape can be declared invariable by
         * application of the const qualifier. This is not the case for
         * samples, being addressed through the series with handle semantics.
         */
        //@{
        typedef S Tseries;
        typedef H Theader;
        typedef typename Tseries::Tvalue Tvalue;
        typedef TimeSeries<Tseries, Theader> Ttimeseries;
        typedef TimeSeries<typename Tseries::Tcoc, Theader> Tconsttimeseries;
        //@}

        /*! \name Constructors.
         *
         * Conversion constructors are declared explicit, to avoid confusion,
         * in mutual assignments of different class derived from this
         * template. They all are a valid Tseries by definition.
         */
        //@{
        TimeSeries() { }
        TimeSeries(const Tseries& s, const Theader& h):
          Tseries(s), header(h) { }
        TimeSeries(const Ttimeseries& s):
          Tseries(s), header(s.header) { }
        //@}

        /*! \name Assignment operators.
         */
        //@{
        //! assign series to time series
        Ttimeseries& operator=(const Tseries& s) 
          { this->Tseries::operator=(s); return(*this); }
        //! set values of series in time series
        Ttimeseries& operator=(const Tvalue& v) 
          { this->Tseries::operator=(v); return(*this); }
        //@}

        /*! \name Type conversion.
         */
        //@{
        //Tseries& series() { return(*this); }
        operator Tconsttimeseries() const
          { return(Tconsttimeseries(*this, header)); }
        //@}
      public:
        //! data header fields
        Theader header;
    }; // struct TimeSeries

} // namespace ts

#endif // TF_TSXX_H_VERSION (includeguard)

/* ----- END OF tsxx.h ----- */
