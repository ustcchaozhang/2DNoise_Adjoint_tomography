/*! \file bonjer.h
 * \brief read bonjers ASCII data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read bonjers ASCII data (prototypes)
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
 *  - 23/11/2010   V1.1   introduced static members
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_BONJER_H_VERSION

#define DATRW_BONJER_H_VERSION \
  "DATRW_BONJER_H   V1.1"

#include<datrwxx/datread.h>

namespace datrw {

  namespace bonjer {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace bonjer 

  /*----------------------------------------------------------------------*/

  /*! \brief input stream to read seismic data provided by K. Bonjer
   *
   * \ingroup group_bonjer
   *
   * The concept is based on SFF data contents and we will make use of SFF
   * structures.
   */
  class ibonjerstream: public idatstream {
    public:
      typedef idatstream Tbase;
      typedef aff::Series<double> Tseries;
      ibonjerstream(std::istream& is);
      virtual ~ibonjerstream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries() { readheader(); }
      static void help(std::ostream& os=std::cout)
      { Tbase::help(os, "ibonjerstream"); }
      static const std::ios_base::openmode openmode;
    private:
      void readheader();
  }; // class ibonjerstream

} // namespace datrw

#endif // DATRW_BONJER_H_VERSION (includeguard)

/* ----- END OF bonjer.h ----- */
