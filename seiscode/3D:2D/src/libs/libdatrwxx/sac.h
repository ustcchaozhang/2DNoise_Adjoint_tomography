/*! \file sac.h
 * \brief read SAC files (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2004
 * 
 * read SAC files (prototypes)
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
 *  - 21/12/2004   V1.0   Thomas Forbriger
 *  - 03/05/2010   V1.1   sac provides debugging option
 *  - 23/11/2010   V1.2   introduced static member data
 *  - 18/11/2016   V1.3   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SAC_H_VERSION

#define DATRW_SAC_H_VERSION \
  "DATRW_SAC_H   V1.3"

#include<datrwxx/datread.h>

namespace datrw {

  namespace sac {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace sac 

  /*----------------------------------------------------------------------*/
  
  /*! \brief class to read SAC binary data
   *
   * \ingroup group_sac
   */
  class isacstream: public idatstream {
    public:
      typedef idatstream Tbase;
      isacstream(std::istream& is, const bool& debug=false);
      virtual ~isacstream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      void readheader();
  }; // class isacstream

} // namespace datrw

#endif // DATRW_SAC_H_VERSION (includeguard)

/* ----- END OF sac.h ----- */
