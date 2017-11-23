/*! \file sfftsfile.h
 * \brief data file container for SFF TimeSeries (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 18/07/2005
 * \date 30/01/2014
 * 
 * data file container for SFF TimeSeries (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef TSIO_SFFTSFILE_H_VERSION

#define TSIO_SFFTSFILE_H_VERSION \
  "TF_SFFTSFILE_H   2014/01/30"

#include<tsioxx/sfftimeseries.h>
#include<datrwxx/datread.h>
#include<tfxx/rangelist.h>
#include<vector>

namespace ts {

  namespace sff {

    /*! \brief SFF File class
     * \defgroup group_sfftsfile SFF File class
     *
     * This module is presented through sfftsfile.h
     *
     * @{
     */

    /*! \brief hold information for a complete SFF file header
     */
    class FileHeader {
      public:
        FileHeader(): Mhasfree(false), Mhassrce(false) { }
        void srce(const ::sff::SRCE& s);
        void free(const ::sff::FREE& f);
        void append(const ::sff::FREE& f);
        ::sff::FREE free() const { return Mfree; }
        ::sff::SRCE srce() const { return Msrce; }
        bool hasfree() const { return Mhasfree; }
        bool hassrce() const { return Mhassrce; }
        void read(std::istream& is, const bool& verbose=false);
      private:
        ::sff::FREE Mfree;
        ::sff::SRCE Msrce;
        bool Mhasfree;
        bool Mhassrce;
    }; // struct FileHeader

    /*----------------------------------------------------------------------*/

    /*! \brief hold SFF traces with full header information
     */
    template<class C>
      class TraceVector: public std::vector<SFFTimeSeries<C> > {
        public:
          typedef std::vector<SFFTimeSeries<C> > Tbase;
          typedef SFFTimeSeries<C> Ttimeseries;
      }; // class TraceVector

    /*----------------------------------------------------------------------*/

    /*! \brief hold all information contained in an SFF data file
     *
     * Store the complete information contained in a data file
     */
    template<class C>
      struct File: public TraceVector<C> {
        public:
          typedef TraceVector<C> Tbase;
          typedef Tbase Ttracevector;
          typedef typename Tbase::Ttimeseries Ttimeseries;
          typedef tfxx::RangeList<int> Trangelist;
          void read(datrw::idatstream& is, 
                    const bool& verbose=false);
          void read(datrw::idatstream& is, 
                    const Trangelist& rl,
                    const bool& verbose=false);
          FileHeader fileheader;
      }; // struct File

    /*!
     * @}
     */
    

    /*======================================================================*/
    // member functions
    
    /*! \brief Read a complete file with all traces.
     */
    template<class C>
    inline void File<C>::read(datrw::idatstream& is, const bool& verbose)
    {
      Trangelist rl;
      this->read(is, rl, verbose);
    } // TraceVector<C>::read
    
    /*----------------------------------------------------------------------*/

    
    /*! \brief Read a data file an extract selected traces.
     */
    template<class C>
      inline void File<C>::read(datrw::idatstream& is, 
                                const Trangelist& rl,
                                const bool& verbose)
    {
      is >> this->fileheader;
      int itrace=0;
      while (is.good())
      {
        ++itrace;
        if ((rl.size()==0) || rl.contains(itrace))
        {
          if (verbose)
          { std::cout << "  * read trace #" << itrace << std::endl; }
          Ttimeseries timeseries;
          is >> timeseries;
          timeseries.settraceindex(itrace);
          this->push_back(timeseries);
        }
        else
        {
          if (verbose)
          { std::cout << "    skip trace #" << itrace << std::endl; }
          is.skipseries();
        }
      }
    } // TraceVector<C>::read

  } // namespace sff
} // namespace ts
    
#endif // TSIO_SFFTSFILE_H_VERSION (includeguard)

/* ----- END OF sfftsfile.h ----- */
