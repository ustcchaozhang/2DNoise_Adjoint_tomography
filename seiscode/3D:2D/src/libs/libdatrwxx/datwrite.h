/*! \file datwrite.h
 * \brief generic interface definition (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/04/2006
 * 
 * generic interface definition (prototypes)
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 11/04/2006   V1.0   Thomas Forbriger
 *  - 07/06/2011   V1.1   promise constness of series samples
 *  - 29/07/2011   V1.2   support property query
 *  - 18/11/2016   V1.3   provide debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_DATWRITE_H_VERSION

#define DATRW_DATWRITE_H_VERSION \
  "DATRW_DATWRITE_H   V1.3"

#include<string>
#include<iostream>
#include<aff/series.h>
#include<sffxx.h>
#include<datrwxx/error.h>
#include<datrwxx/formats.h>
#include<datrwxx/properties.h>
#include<datrwxx/types.h>

namespace datrw {

  /*----------------------------------------------------------------------*/

  /*! check for existing output fil
   *
   * aborts, if file with given filename exists
   */
  void abort_if_exists(const std::string& filename);

  /*----------------------------------------------------------------------*/

  /*! output stream to write seismic data (abstract base)
   *
   * The concept is based on SFF data contents and we will make use of SFF
   * structures.
   *
   * \note
   * Design considerations:
   * For most data types it would be convenient to flush data traces to file
   * upon a call to odatstream::writeseries().
   * For this reason trace data fill be collected by setwid2, setfree, and
   * setinfo and will be flushed to file upon a call to writeseries.
   * Prior to writing the first trace functions odatstream::setsrce() and
   * odatstream::setfree() can be used to write file specific data.
   * This file header will be flushed to file upon calling
   * odatstream::flushfileheader() or upon the first call to
   * odatstream::setwid2().
   * No file header fields can be written after the first call to
   * odatstream::setwid2().
   * odatstream::setwid2() must be called for each trace.
   * The other fields are optional.
   *
   * \note
   * Design considerations:
   * Functions handlesfilefree(), handlessrce(), handlestracefree(),
   * handlesinfo(), seriestype() contain constant and static
   * functionality only. They could replaced by a static const member, like is
   * done with openmode in derived classes. However, since it is useful to
   * have these functions inherintance transparent, such that we cann call
   * them from oanystream, they are implemented as ordinary members and
   * provide data through a mechanism like is used for idatream::providesd().
   *
   * \note
   * All derived classes have to provide an openmode field.
   * This cannot be provided through the function interface, since the field
   * is requested prior to creating an instance of the class.
   *
   * \note
   * Due to the design descision just made, a copy of the series will be kept
   * until the next trace will be written, since trace data is flushed in
   * libsffxx upon writing the wid2 data for the next trace.
   */
  class odatstream {
    public:
      virtual ~odatstream() { }
      void setfree(const sff::FREE& free);
      void setwid2(const sff::WID2& wid2);
      void setsrce(const sff::SRCE& srce);
      void setinfo(const sff::INFO& info);
      //! write double data
      void writeseries(const Tdseries::Tcoc& series);
      //! write single precision float data
      void writeseries(const Tfseries::Tcoc& series);
      //! write integer data
      void writeseries(const Tiseries::Tcoc& series);
      //! flush file header to file
      void flushfileheader();
      //! true if file FREE block can be handled
      bool handlesfilefree() const { return(Mhandlesfilefree); }
      //! true if SRCE data can be handled
      bool handlessrce() const { return(Mhandlessrce); }
      //! true if trace FREE block can be handled
      bool handlestracefree() const { return(Mhandlestracefree); }
      //! true if INFO data can be handled
      bool handlesinfo() const { return(Mhandlesinfo); }
      /*! indicate type of series data
       * \deprecated
       * The use of Edatatype flags is deprecated and will be replaced by
       * a more verbose class or struct which can be extended in the future
       */
      Edatatype seriestype() const { return(Mdatatype); }

      //! \brief query properties
      Properties properties() const;

      //! print some info about data conversion.
      static void help(std::ostream& os=std::cout,
                       const char* name="idatsream");
        
      //! indicate debug mode
      bool debug() { return Mdebug; }
      //! set debug mode
      void debug(const bool& debug) { Mdebug=debug; }
    protected:
      //! constructor is protected: do not create an instance of this class
      odatstream(std::ostream& os,
                 const Edatatype& datatype,
                 const bool& handlesfilefree=false,
                 const bool& handlestracefree=false,
                 const bool& handlessrce=false,
                 const bool& handlesinfo=false,
                 const bool& debug=false);
      //! write double data
      virtual void writetrace(const Tdseries::Tcoc& series) { DATRW_illegal; }
      //! write single precision float data
      virtual void writetrace(const Tfseries::Tcoc& series) { DATRW_illegal; }
      //! write integer data
      virtual void writetrace(const Tiseries::Tcoc& series) { DATRW_illegal; }
      //! actually write the file header
      virtual void writefileheader() { DATRW_illegal; }
      //! clear trace header flags
      void cleartraceheader();

      //! return WID2 data
      sff::WID2 wid2() const { return(Mwid2); }
      //! return SRCE data
      sff::SRCE srce() const { return(Msrce); }
      //! return SRCE data
      sff::INFO info() const { return(Minfo); }
      //! return FREE data
      sff::FREE free() const { return(Mfree); }

      //! wid2 is available
      bool haswid2() const { return(Mwid2set); }
      //! srce is available
      bool hassrce() const { return(Msrceset); }
      //! info is available
      bool hasinfo() const { return(Minfoset); }
      //! free is available
      bool hasfree() const { return(Mfreeset); }
        
      /*! tell the specific data type used
       * \deprecated
       * The use of Edatatype flags is deprecated and will be replaced by
       * a more verbose class or struct which can be extended in the future
       */
      void setdatatype(const Edatatype& daty) { Mdatatype=daty; }

      //! output stream to be used by this class
      std::ostream& Mos;

      //! global debug flag
      bool Mdebug;
    private:
      bool Mwid2set, Msrceset, Minfoset, Mfreeset;
      bool Mheaderflushed;
      bool Mhandlestracefree, Mhandlesfilefree;
      bool Mhandlesinfo, Mhandlessrce;
      /*! the specific internal data type
       * \deprecated
       * The use of Edatatype flags is deprecated and will be replaced by
       * a more verbose class or struct which can be extended in the future
       */
      Edatatype Mdatatype;
      sff::WID2 Mwid2;
      sff::SRCE Msrce;
      sff::INFO Minfo;
      sff::FREE Mfree;
  }; // class odatstream

  /*----------------------------------------------------------------------*/

  inline odatstream& operator<<(odatstream& os, const sff::WID2& wid2)
  { os.setwid2(wid2); return(os); }

  inline odatstream& operator<<(odatstream& os, const sff::SRCE& srce)
  { os.setsrce(srce); return(os); }

  inline odatstream& operator<<(odatstream& os, const sff::INFO& info)
  { os.setinfo(info); return(os); }

  inline odatstream& operator<<(odatstream& os, const sff::FREE& free)
  { os.setfree(free); return(os); }

  inline odatstream& operator<<(odatstream& os, const Tdseries::Tcoc& series)
  { os.writeseries(series); return(os); }

  inline odatstream& operator<<(odatstream& os, const Tfseries::Tcoc& series)
  { os.writeseries(series); return(os); }

  inline odatstream& operator<<(odatstream& os, const Tiseries::Tcoc& series)
  { os.writeseries(series); return(os); }

} // namespace datrw

#endif // DATRW_DATWRITE_H_VERSION (includeguard)

/* ----- END OF datrw.h ----- */
