/*! \file gse.h
 * \brief read raw GSE data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/09/2007
 * 
 * read raw GSE data (prototypes)
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 19/09/2007   V1.0   Thomas Forbriger
 *  - 23/11/2010   V1.1   introduced static members
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_GSE_H_VERSION

#define DATRW_GSE_H_VERSION \
  "DATRW_GSE_H   V1.1"

#include<datrwxx/datread.h>

namespace datrw {

  namespace gse {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace gse 

  /*----------------------------------------------------------------------*/

  /*! \brief class to read raw GSE data
   * \ingroup group_gse
   */
  class igsestream: public idatstream {
    public:
      typedef idatstream Tbase;
      igsestream(std::istream& is);
      virtual ~igsestream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      void readheader();
  }; // class igsestream

} // namespace datrw

#endif // DATRW_GSE_H_VERSION (includeguard)

/* ----- END OF gse.h ----- */
