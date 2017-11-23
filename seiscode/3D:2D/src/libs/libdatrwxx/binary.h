/*! \file binary.h
 * \brief write raw binary data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 24/02/2010
 * 
 * write raw binary data (prototypes)
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
 * 
 * REVISIONS and CHANGES 
 *  - 24/02/2010   V1.0   Thomas Forbriger
 *  - 18/11/2016   V1.1   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_BINARY_H_VERSION

#define DATRW_BINARY_H_VERSION \
  "DATRW_BINARY_H   V1.1"

#include<datrwxx/datread.h>
#include<datrwxx/datwrite.h>
#include<datrwxx/ibinstream.h>
#include<datrwxx/obinstream.h>

namespace datrw {

  /*! \brief I/O module for binary data
   *
   * \defgroup group_binary I/O module for binary data
   */

  /*----------------------------------------------------------------------*/

  /*! \brief internals of the binary I/O module
   *
   * \ingroup group_binary
   */
  namespace binary {

    extern const bool isbinary;
    extern const char* const streamID;

    /*! \brief magic number to identify file type and bytesex
     * \ingroup group_binary
     */
    extern const char* const magic;
    /*! \brief a version number for files - just in case
     * \ingroup group_binary
     */
    extern const short version;

    /*! \brief indicate file or trace properties
     * \ingroup group_binary
     */
    enum Eflags {
      Fsrce=   (1<<0), //!< trace has SRCE header
      Ffree=   (1<<1), //!< trace has FREE header
      Finfo=   (1<<2), //!< trace has INFO header
      Fdouble= (1<<3), //!< trace has double data
      Ffloat=  (1<<4), //!< trace has float data
      Fint=    (1<<5)  //!< trace has int data
    }; // enum Eflags

    /*! \brief abort if file flags are inconsistent
     * \ingroup group_binary
     */
    void checkfileflags(const char& flags);
    /*! \brief abort if trace flags are inconsistent
     * \ingroup group_binary
     */
    void checktraceflags(const char& flags);

  } // namespace binary 

  /*----------------------------------------------------------------------*/
  
  /*! \brief class to read binary data
   *
   * \ingroup group_binary
   */
  class ibinarystream: public idatstream {
    public:
      typedef idatstream Tbase;
      ibinarystream(std::istream& is, const bool& debug=false);
      virtual ~ibinarystream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      void readheader();
      void readflags();
      void setnsamples(const unsigned int& nsamples);
      binary::ibinstream Mibs;
      short Mversion;
      char Mnextflags; //!< flags for next trace
  }; // class ibinarystream

  /*----------------------------------------------------------------------*/

  /*! \brief class to write binary
   *
   * \ingroup group_binary
   */
  class obinarystream: public odatstream {
    public:
      typedef odatstream Tbase;
      obinarystream(std::ostream& os, const bool& debug=false);
      virtual ~obinarystream();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    protected:
      virtual void writefileheader();
      //! write double data
      virtual void writetrace(const Tdseries::Tcoc& series);
      //! write single precision float data
      virtual void writetrace(const Tfseries::Tcoc& series);
      //! write integer data
      virtual void writetrace(const Tiseries::Tcoc& series);
    private:
      void writetraceheader(const binary::Eflags&,
                            const unsigned int& nsamples);
      binary::obinstream Mobs;
  }; // class obinarystream

} // namespace datrw

#endif // DATRW_BINARY_H_VERSION (includeguard)

/* ----- END OF binary.h ----- */
