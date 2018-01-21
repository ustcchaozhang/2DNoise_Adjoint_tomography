/*! \file seife.h
 * \brief seife reading and writing module (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/11/2010
 * 
 * seife reading and writing module (prototypes)
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
 *  - 30/11/2010   V1.0   Thomas Forbriger
 *  - 18/11/2016   V1.1   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SEIFE_H_VERSION

#define DATRW_SEIFE_H_VERSION \
  "DATRW_SEIFE_H   V1.1"

#include<datrwxx/datread.h>
#include<datrwxx/datwrite.h>

namespace datrw {

  /*! \brief internals of the seife I/O module
   *
   * \ingroup group_seife
   */
  namespace seife {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace seife 

  /*----------------------------------------------------------------------*/
  
  /*! \brief class to read seife data
   *
   * \ingroup group_seife
   */
  class iseifestream: public idatstream {
    public:
      typedef idatstream Tbase;
      iseifestream(std::istream& is, 
                   const std::string& modifier="",
                   const bool& debug=false);
      virtual ~iseifestream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      // virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      void settraceheader();
      std::string Mmodifier; //!< format modifier
  }; // class iseifestream

  /*----------------------------------------------------------------------*/

  /*! \brief class to write seife data
   *
   * \ingroup group_seife
   */
  class oseifestream: public odatstream {
    public:
      typedef odatstream Tbase;
      oseifestream(std::ostream& os, 
                   const std::string& modifier="",
                   const bool& debug=false);
      inline virtual ~oseifestream() { }
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    protected:
      virtual void writefileheader();
      virtual void writetrace(const Tdseries::Tcoc& series);
      virtual void writetrace(const Tfseries::Tcoc& series);
    private:
      void writetraceheader(const unsigned int& n);
      bool Mtracewritten; //!< true, if a trace has be written
      std::string Mmodifier; //!< format modifier
  }; // class oseifestream

} // namespace datrw

#endif // DATRW_SEIFE_H_VERSION (includeguard)

/* ----- END OF seife.h ----- */
