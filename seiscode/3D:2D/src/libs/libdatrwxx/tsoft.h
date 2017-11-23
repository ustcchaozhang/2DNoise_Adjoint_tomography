/*! \file tsoft.h
 * \brief classes for TSOFT in SFF (prototypes)
 * \ingroup group_tsoft
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/09/2009
 * 
 * classes for TSOFT in SFF (prototypes)
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 16/09/2009   V1.0   Thomas Forbriger
 *  - 23/11/2010   V1.1   introduced static member data
 *  - 18/11/2016   V1.2   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_TSOFT_H_VERSION

#define DATRW_TSOFT_H_VERSION \
  "DATRW_TSOFT_H   V1.2"

#include<datrwxx/datread.h>
#include<datrwxx/tsoftsffcontainer.h>
#include<datrwxx/tsoftconfig.h>

namespace datrw {

  namespace tsoft {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace tsoft 

  /*----------------------------------------------------------------------*/

  /*! \brief class to read TSOFT data
   *
   * \ingroup group_tsoft
   */
  class itsoftstream: public idatstream {
    public:
      typedef idatstream Tbase;
      itsoftstream(std::istream& is, const bool& debug=false);
      itsoftstream(std::istream& is, 
                   const std::string& modifier,
                   const bool& debug=false);
      virtual ~itsoftstream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      //! container for complete file
      datrw::tsoft::File Mfile;
      //! index of next trace to be passed
      aff::Tsubscript Mitrace;
      //! read file to buffer
      void read(std::istream& is);
      //! TSOFT reader configuration
      tsoft::ReaderConfig Mreaderconfig;
  }; // class itsoftstream

} // namespace datrw

#endif // DATRW_TSOFT_H_VERSION (includeguard)

/* ----- END OF tsoft.h ----- */
