/*! \file sff.h
 * \brief read sff data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read sff data (prototypes)
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
 *  - 12/06/2007   V1.1   added DEBUG member data
 *  - 23/11/2010   V1.2   introduced static member data
 *  - 07/06/2011   V1.3   promise constness of series samples
 *  - 21/11/2011   V1.4   introduce format modifiers
 *  - 18/11/2016   V1.5   use debug flag in base class
 *  - 22/11/2016   V1.6   set default for format modifiers
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SFF_H_VERSION

#define DATRW_SFF_H_VERSION \
  "DATRW_SFF_H   V1.6"

#include<sffxx.h>
#include<datrwxx/datread.h>
#include<datrwxx/datwrite.h>
#include<datrwxx/reservoir.h>

namespace datrw {

  /*! \brief Format specific modules for SFF data
   *
   * \ingroup group_sff
   */
  namespace sff {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace sff 

  /*----------------------------------------------------------------------*/

  /*! \brief I/O module for SFF
   *
   * \defgroup group_sff I/O module for SFF
   */

  /*! \brief class to read SFF data
   *
   * \ingroup group_sff
   */
  class isffstream: public idatstream {
    public:
      typedef idatstream Tbase;
      isffstream(std::istream& is, const bool& debug=false);
      virtual ~isffstream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout)
      { Tbase::help(os, "isffstream"); }
      static const std::ios_base::openmode openmode;
    private:
      void settraceheader(const ::sff::TraceHeader& header);
  }; // class isffstream

/*======================================================================*/

  /*! \brief class to write SFF data
   *
   * \ingroup group_sff
   */
  class osffstream: public odatstream {
    public:
      typedef odatstream Tbase;
      osffstream(std::ostream& os, const std::string& modifier="",
                 const bool& debug=false);
      inline virtual ~osffstream() 
      { this->flushwaitingtrace(true); }
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    protected:
      virtual void writefileheader();
      virtual void writetrace(const Tdseries::Tcoc& series);
      virtual void writetrace(const Tfseries::Tcoc& series);
      virtual void writetrace(const Tiseries::Tcoc& series);
    private:
      /*! flush waiting trace data
       * \PARAM last true if no other trace will be written,
       *             i.e. only set to true when called from destructor
       */
      void flushwaitingtrace(const bool& last=false);

    private:
      bool Mwid2iswaiting, Mfreeiswaiting, Minfoiswaiting;
      ::sff::WID2 Mwid2waiting;
      ::sff::FREE Mfreewaiting;
      ::sff::INFO Minfowaiting;
      datrw::util::seriesreservoir Mserieswaiting;

    protected:
      ::sff::Enormmode Mnormmode;
  }; // class osffstream

  /*----------------------------------------------------------------------*/

  /*! \brief class to write GSE data
   *
   * \ingroup group_sff
   */
  class ogsestream: public osffstream {
    public:
      typedef osffstream Tbase;
      ogsestream(std::ostream& os, const bool& debug=false);
      inline virtual ~ogsestream() { }
      static void help(std::ostream& os=std::cout);
      /*
    protected:
      virtual void writetrace(const Tdseries& series) 
      { DATRW_abort("ogsestream cannot write doubles"); }
      virtual void writetrace(const Tfseries& series) 
      { DATRW_abort("ogsestream cannot write floats"); }
      */
  }; // class ogsestream

} // namespace datrw

#endif // DATRW_SFF_H_VERSION (includeguard)

/* ----- END OF sff.h ----- */
