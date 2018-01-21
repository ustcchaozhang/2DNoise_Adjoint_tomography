/*! \file thiesdl1.h
 * \brief module to read ThiesDL1 data files (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * module to read ThiesDL1 data files (prototypes)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 13/09/2011   V1.0   Thomas Forbriger
 *  - 18/11/2016   V1.1   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_THIESDL1_H_VERSION

#define DATRW_THIESDL1_H_VERSION \
  "DATRW_THIESDL1_H   V1.1"

#include <datrwxx/datread.h>
#include <datrwxx/thiesdl1file.h>

namespace datrw {

  /*! \brief internals of the Thies DL1 reading module
   *
   * \ingroup group_thiesdl1
   */
  namespace thiesdl1 {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace thiesdl1 

  /*----------------------------------------------------------------------*/
  
  /*! \brief class to read Thies DL1 data
   *
   * \ingroup group_thiesdl1
   */
  class ithiesdl1stream: public idatstream {
    public:
      typedef idatstream Tbase;
      ithiesdl1stream(std::istream& is, 
                      const std::string& modifier="",
                      const bool& debug=false);
      virtual ~ithiesdl1stream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      void settraceheader();
      void readsamples();
      std::string Mmodifier;
      thiesdl1::FileHeader Mheader;
      thiesdl1::File Mfile;
  }; // class ithiesdl1stream

} // namespace datrw


#endif // DATRW_THIESDL1_H_VERSION (includeguard)

/* ----- END OF thiesdl1.h ----- */
