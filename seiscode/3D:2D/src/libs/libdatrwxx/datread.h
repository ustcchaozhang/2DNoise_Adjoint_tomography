/*! \file datrw.h
 * \brief abstract base class to read seismic data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * abstract base class to read seismic data (prototypes)
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
 *  - 30/03/2004   V1.0   Thomas Forbriger
 *  - 21/04/2004   V1.1   some improvements - provide definition of use
 *  - 17/09/2004   V1.2   make most fields private
 *  - 23/12/2004   V1.3   user of datstream::good() does not want to know
 *                        anything about Mis state
 *  - 23/11/2010   V1.4   introduced isbinary flag
 *  - 29/07/2011   V1.5   support property query
 *  - 18/11/2016   V1.6   provide debug flag in base class
 *                        do not define input operators inline
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_DATREAD_H_VERSION

#define DATRW_DATREAD_H_VERSION \
  "DATRW_DATREAD_H   V1.6"

#include<iostream>
#include<sffxx.h>
#include<aff/series.h>
#include<datrwxx/error.h>
#include<datrwxx/properties.h>
#include<datrwxx/types.h>

namespace datrw {

  /*! input stream to read seismic data (abstract base)
   *
   * The concept is based on SFF data contents and we will make use of SFF
   * structures.
   *
   * \note
   * The convention is that a call to dseries(), fseries(), iseries() or
   * skipseries() will scan the next trace in the input file and will fill all
   * header structures. While dseries(), fseries() and iseries() return
   * samples, skipseries() simply discards them. Thus use the time series
   * input operator first for each trace. Read out header information
   * afterwards.
   *
   * \note 
   * For this reason only the member functions dseries(), fseries(), iseries()
   * and skipseries() have to be virtual. They are responsible for filling the
   * header data structures with appropriate values upon scanning/reading a
   * time series.
   *
   * \note
   * All derived classes are expected to provide static members
   * openmode and isbinary
   */
  class idatstream {
    public:
      virtual ~idatstream() { }
      virtual Tdseries dseries() { DATRW_illegal; }
      virtual Tfseries fseries() { DATRW_illegal; }
      virtual Tiseries iseries() { DATRW_illegal; }
      virtual void skipseries() { DATRW_illegal; }
      bool hasfree() const;
      bool hassrce() const { return(Msrceset); }
      bool hasinfo() const { return(Minfoset); }
      bool providesd() const { return(Mprovidesd); }
      bool providesf() const { return(Mprovidesf); }
      bool providesi() const { return(Mprovidesi); }
      bool last() const { return(Mlast); }
      bool good() const { return(!Mlast); }
      Properties properties() const;
      /*! return FREE block
       *
       * if no WID2 ist set (prior to reading first trace)
       * the file FREE block is returned
       *
       * after reading the first trace, the trace FREE block is returned.
       *
       * This mechanism is required to provide easy to use input operators,
       * that cannot distinguish between file FREE blocks and trace FREE
       * blocks from just looking at their arguments.
       */
      sff::FREE free() const;
      sff::SRCE srce() const { return(Msrce); }
      sff::INFO info() const { return(Minfo); }
      sff::WID2 wid2() const { return(Mwid2); }
      //! print some info about data conversion.
      static void help(std::ostream& os=std::cout,
                       const char* name="idatsream");
      //! indicate debug mode
      bool debug() { return Mdebug; }
      //! set debug mode
      void debug(const bool& debug) { Mdebug=debug; }
    protected:
      // member functions
      idatstream(std::istream& is,
                 const bool& providesd=false,
                 const bool& providesf=false,
                 const bool& providesi=false,
                 const bool& debug=false);
      std::istream& Mis;
      void setfilefree(const sff::FREE& free);
      void settracefree(const sff::FREE& free);
      void setwid2(const sff::WID2& wid2);
      void setsrce(const sff::SRCE& srce);
      void setinfo(const sff::INFO& info);
      void setlast();
      void newtrace();
      // member data
      bool Mdebug;
    private:
      sff::WID2 Mwid2;
      sff::SRCE Msrce;
      sff::INFO Minfo;
      sff::FREE Mtracefree;
      sff::FREE Mfilefree;
      bool Mwid2set, Msrceset, Minfoset, Mtracefreeset, Mfilefreeset;
      bool Mlast;
      bool Mprovidesd, Mprovidesf, Mprovidesi;
  }; // class idatstream

  /*----------------------------------------------------------------------*/

  idatstream& operator>>(idatstream& is, sff::WID2& wid2);

  idatstream& operator>>(idatstream& is, sff::SRCE& srce);

  idatstream& operator>>(idatstream& is, sff::INFO& info);

  idatstream& operator>>(idatstream& is, sff::FREE& free);

  idatstream& operator>>(idatstream& is, Tdseries& series);

  idatstream& operator>>(idatstream& is, Tfseries& series);

  idatstream& operator>>(idatstream& is, Tiseries& series);

} // namespace datrw

#endif // DATRW_DATREAD_H_VERSION (includeguard)

/* ----- END OF datrw.h ----- */
