/*! \file filterbase.h
 * \brief base class for all filter classes (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2005
 * 
 * base class for all filter classes (prototypes)
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 05/07/2005   V1.0   Thomas Forbriger
 *  - 18/12/2007   V1.1   - support debugging
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FILTERBASE_H_VERSION

#define TF_FILTERBASE_H_VERSION \
  "TF_FILTERBASE_H   V1.1"

#include<list>
#include<string>
#include<tsxx/dttimeseries.h>
#include<tfxx/handle.h>
#include<tfxx/error.h>

namespace ts {

  namespace filter {

    //! we always work in double precision
    typedef double Tvalue;

    //! use double precision time series
    typedef ts::TDtimeseries Ttimeseries;

    /*! base class for any filter 
     *
     * Filters derived from that class will have the following properties:
     * - They do neither need the time of first sample nor will they alter it
     * - They will not change the number of samples
     * - They will not change the sampling interval
     *
     * The only other filter that changes the number of samples and the
     * interval and that might fit into this interface is a decimation filter. 
     * I do not like to abandon this specification for just one filter, since
     * we have to define a second interface for window filters and time
     * shifting filters anyway, which need the full SFF header.
     */
    class BasicFilter {
      public:
        //! type of sample values
        typedef double Tvalue;
        //! type of series container
        typedef ts::filter::Ttimeseries Ttimeseries;
        typedef Ttimeseries::Tseries Tseries;
        typedef Ttimeseries::Theader Theader;
        virtual ~BasicFilter();
        virtual Ttimeseries operator()(const Ttimeseries& s,
                                       const bool& debug=false) const =0;
        Ttimeseries operator()(const Tseries& s, const Tvalue& dt) const
        { return this->operator()(Ttimeseries(s, Theader(dt))); }
        template <class H>
        Ttimeseries operator()(const ts::TimeSeries<Tseries, H>& s) const
        { return this->operator()(Ttimeseries(s, Theader(s.header.dt))); }
      protected:
        //! Do not allow to use the baseclass alone
        BasicFilter();
    }; // class BasicFilter

    //! handle to pass filters
    typedef tfxx::Handle<BasicFilter> Tfilterhandle;

    /*! filter collection
     */
    class FilterCollection: public std::list<Tfilterhandle> {
      public: 
        typedef ts::filter::Tfilterhandle Tfilterhandle;
        typedef Tfilterhandle::Tobject Tfilter;
        typedef std::list<Tfilterhandle> Tfilterlist;
        typedef Tfilterlist Tbase;
        typedef Ttimeseries::Tseries Tseries;
        typedef Ttimeseries::Theader Theader;
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const;
        Ttimeseries operator()(const Tseries& s, const Tvalue& dt) const
        { return this->operator()(Ttimeseries(s, Theader(dt))); }
        template <class H>
        Ttimeseries operator()(const ts::TimeSeries<Tseries, H>& s) const
        { return this->operator()(Ttimeseries(s, Theader(s.header.dt))); }
    }; // class FilterCollection

    //! no-operation filter
    class Noop: public BasicFilter {
      public:
        Ttimeseries operator()(const Ttimeseries& s,
                               const bool& debug=false) const { return s; }
    }; // class Noop

    //! exception class
    class Exception: public tfxx::error::Exception {
      public:
        typedef tfxx::error::Exception Tbase;
        Exception(const char* message, 
                  const char* file,
                  const int& line,
                  const char* condition): 
          Tbase(message, file, line, condition) { }
        virtual void report() const;
    }; // class Exception

    //! unknown filter exception
    class UnknownFilterException: public Exception {
      public: 
        typedef Exception Tbase;
        UnknownFilterException(const char* message, 
                               const char* file,
                               const int& line,
                               const std::string& filter):
          Tbase(message, file, line, "filter type is unknown"),
          Mfilter(filter) { }
        virtual ~UnknownFilterException() { }
        virtual void report() const;
      private:
        std::string Mfilter;
    }; // UnknownFilterException

  } // namespace filter

} // namespace ts

#define TSXX_UnknownFilterAbort(M, F) \
  throw( ts::filter::UnknownFilterException ( M , __FILE__, __LINE__, F )) 

#endif // TF_FILTERBASE_H_VERSION (includeguard)

/* ----- END OF filterbase.h ----- */
