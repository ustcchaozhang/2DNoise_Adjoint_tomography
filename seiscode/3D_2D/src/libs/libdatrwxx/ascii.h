/*! \file ascii.h
 * \brief interface to write ASCII data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/04/2006
 * 
 * interface to write ASCII data (prototypes)
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
 *  - 23/08/2012   V1.1   output stream supports modifiers
 *  - 18/11/2016   V1.2   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_ASCII_H_VERSION

#define DATRW_ASCII_H_VERSION \
  "DATRW_ASCII_H   V1.2"

#include<datrwxx/datread.h>
#include<datrwxx/datwrite.h>

namespace datrw {

  /*! \brief internals of the ascii I/O module
   *
   * \ingroup group_ascii
   */
  namespace ascii {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace ascii 

  /*----------------------------------------------------------------------*/
  
  /*! \brief class to read ascii data
   *
   * \ingroup group_ascii
   */
  class iasciistream: public idatstream {
    public:
      typedef idatstream Tbase;
      iasciistream(std::istream& is, 
                   const std::string& modifier="",
                   const bool& debug=false);
      virtual ~iasciistream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      void settraceheader();
      void readheader();
      bool Mnonfatal;
      std::string Mmodifier; //!< format modifier
      std::string Mcurrentline;
      std::string Mdatatype;
      unsigned int Mnsamples;
      ::sff::WID2 Mdefaultwid2;
      ::sff::WID2 Mcurrentwid2;
      bool Mreadfree;
      ::sff::FREE Mcurrentfree;
      bool Mreadinfo;
      ::sff::INFO Mdefaultinfo;
      ::sff::INFO Mcurrentinfo;
      bool Mreadsrce;
      ::sff::SRCE Mdefaultsrce;
      ::sff::SRCE Mcurrentsrce;
  }; // class iasciistream

  /*----------------------------------------------------------------------*/

  /*! \brief class to write ascii data
   *
   * \ingroup group_ascii
   */
  class oasciistream: public odatstream {
    public:
      typedef odatstream Tbase;
      oasciistream(std::ostream& os, 
                   const std::string& modifier="",
                   const bool& debug=false);
      inline virtual ~oasciistream() { }
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    protected:
      virtual void writefileheader();
      virtual void writetrace(const Tdseries::Tcoc& series);
      virtual void writetrace(const Tfseries::Tcoc& series);
      virtual void writetrace(const Tiseries::Tcoc& series);
    private:
      void writetraceheader(const unsigned int& n, const char* type);
      unsigned int Mprecision; //!< floating point format precision
  }; // class oasciistream

} // namespace datrw

#endif // DATRW_ASCII_H_VERSION (includeguard)

/* ----- END OF ascii.h ----- */
