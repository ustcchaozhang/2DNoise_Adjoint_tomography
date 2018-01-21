/*! \file sfftimeseries.h
 * \brief libtsxx TimeSeries class template with SFF trace header (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 18/07/2005
 * \date 30/01/2014
 * 
 * libtsxx TimeSeries class template with SFF trace header (prototypes)
 * 
 * Copyright (c) 2005-2007, 2012, 2014 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 30/01/2014   V1.0   Thomas Forbriger (thof):
 *                        copied from sffheaders.h
 *  - 22/11/2016   V1.1   reorganize public interface to SFFTimeSeries
 *                        required to provide reliable type conversion
 * 
 * ============================================================================
 */

// include guard
#ifndef TSIO_SFFTIMESERIES_H_VERSION

#define TSIO_SFFTIMESERIES_H_VERSION \
  "TF_SFFTIMESERIES_H   2016/11/22"

#include<sffxx.h>
#include<tsxx/tsxx.h>
#include<tsxx/dttimeseries.h>

/*! \brief Outer namespace
 *
 * All mosules are placed within this namespace, which is the namespace of
 * modules from libtsxx.
 */
namespace ts {

  /*! \brief Namespace for libtsioxx modules
   *
   * To distinguish the modules in libtsioxx and to avoid future conflicts,
   * all modules in libtsioxx are placed in this namespace.
   */
  namespace sff {

    /*----------------------------------------------------------------------*/

    /*! \brief SFF TimeSeries class
     * \defgroup group_sfftimeseries SFF TimeSeries class
     *
     * This module is presented through sfftimeseries.h
     *
     * @{
     */

    /*! \brief hold information for a complete SFF trace header
     */
    class TraceHeader {
      public:
        TraceHeader(): Mhasfree(false), Mhasinfo(false) { }
        void info(const ::sff::INFO& s);
        void free(const ::sff::FREE& f);
        void wid2(const ::sff::WID2& w) { Mwid2=w; }
        void append(const ::sff::FREE& f);
        ::sff::FREE free() const { return Mfree; }
        ::sff::INFO info() const { return Minfo; }
        ::sff::WID2 wid2() const { return Mwid2; }
        bool hasfree() const { return Mhasfree; }
        bool hasinfo() const { return Mhasinfo; }
        void read(std::istream& is, const bool& verbose=false);
      private:
        ::sff::FREE Mfree;
        ::sff::INFO Minfo;
        ::sff::WID2 Mwid2;
        bool Mhasfree;
        bool Mhasinfo;
    }; // class TraceHeader

    /*----------------------------------------------------------------------*/

    /*! \brief hold a full SFF trace and provide conversion
     *
     * This \b is a ts::TimeSeries<C,ts::sff::TraceHeader> class, due to
     * public inheritance. However, it is not just a type definition, because
     * we provide additional member data.
     */
    template<class C>
      class SFFTimeSeries: 
      public ts::TimeSeries<C,ts::sff::TraceHeader> {
        public:
          /*! \name Type definitions.
           *
           * Base class type definitions must be repeated, because typedefs
           * are not inherited by default.
           */
          //@{
          typedef ts::TimeSeries<C,ts::sff::TraceHeader> Tbase;
          typedef typename Tbase::Tvalue Tvalue;
          typedef ts::TimeSeries<C,::sff::WID2> Twid2timeseries;
          typedef ts::TimeSeries<C,ts::DTHeader<Tvalue> > Tdttimeseries;
          typedef typename Tbase::Tseries Tseries;
          typedef typename Tbase::Theader Theader;
          typedef typename Tbase::Ttimeseries Ttimeseries;
          typedef typename Tbase::Tconsttimeseries Tconsttimeseries;
          //@}

          /*! \name Constructors.
           *
           * Conversion constructors are declared explicit, to avoid
           * confusion, in mutual assignments of different class derived from
           * template ts::TimeSeries. They all are a valid Tseries by
           * definition.
           */
          //@{
          //! default constructor
          SFFTimeSeries(const int& i=-1): Tbase(), Mtraceindex(-1) { }
          SFFTimeSeries(const Tseries& s, const Theader& h, const int& i=-1): 
            Tbase(s, h), Mtraceindex(i) { }
          SFFTimeSeries(const Tbase& s, const int& i=-1): 
            Tbase(s), Mtraceindex(i) { }
          SFFTimeSeries(const SFFTimeSeries& s): 
            Tbase(s), Mtraceindex(s.Mtraceindex) { }
          //@}
            
          /*! \name Type conversion and assignment operators.
           *
           * The member data contains everything required to define a proper
           * Twid2timeseries or a proper Tdttimeseries.
           */
          //@{
          //! Convert to Twid2timeseries with sff::WID2 header
          operator Twid2timeseries() const
          { return Twid2timeseries(Tseries(*this), this->header.wid2()); }
          //! Convert to Tdttimeseries with ts::DTHeader<Tvalue> header
          operator Tdttimeseries() const
          { return Tdttimeseries(Tseries(*this), this->header.wid2().dt); }
          //! Take contents of base class.
          SFFTimeSeries& operator=(const Tseries& s)
          { this->Tseries::operator=(s); return(*this); }
          //@}

          //! read data from input stream
          void read(std::istream& is, const bool& verbose=false);
          //! return trace index member data
          int traceindex() const { return Mtraceindex; }
          //! set trace index member data
          void settraceindex(int ti) { Mtraceindex=ti; }
        private:
          int Mtraceindex; //!< trace index in data file
      }; // class SFFTimeSeries

    /*----------------------------------------------------------------------*/

    /*!
     * @}
     */

  } // namespace sff
} // namespace ts
    
#endif // TSIO_SFFTIMESERIES_H_VERSION (includeguard)

/* ----- END OF sfftimeseries.h ----- */
