/*! \file ibinstream.h
 * \brief basic binary input stream (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/11/2011
 * 
 * basic binary input stream (prototypes)
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
 * 
 * REVISIONS and CHANGES 
 *  - 10/11/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_IBINSTREAM_H_VERSION

#define DATRW_IBINSTREAM_H_VERSION \
  "DATRW_IBINSTREAM_H   V1.0   "

#include <iostream>
#include <fstream>
#include <string>
#include <libtime++.h>
#include <datrwxx/types.h>
#include <sffxx.h>

namespace datrw {

  namespace binary {

    /*! \brief stream like class for binary input of basic types and classes
     * \ingroup group_binary
     */
    class ibinstream {
      public:
        ibinstream(std::istream& is,
                   const char* const magic,
                   const bool& debug=false);
        void read(char& v);
        void read(short& v);
        void read(int& v);
        void read(unsigned int& v);
        void read(double& v);
        void read(float& v);
        void read(std::string& v);
        void read(libtime::TAbsoluteTime& v);
        void read(::sff::FREE& v);
        void read(::sff::WID2& v);
        void read(::sff::SRCE& v);
        void read(::sff::INFO& v);
        void read(Tdseries& v);
        void read(Tfseries& v);
        void read(Tiseries& v);
        unsigned int skipdseries();
        unsigned int skipfseries();
        unsigned int skipiseries();
      private:
        std::istream& Mis;
        bool Mswap;
        bool Mdebug;
    }; // class ibinstream

    /*----------------------------------------------------------------------*/

    /*! \brief input operator for basic types and classes
     * \ingroup group_binary
     */
    template<class C>
      ibinstream& operator>>(ibinstream& ibs, C& v)
      {
        ibs.read(v); return(ibs);
      } // ibinstream& operator>>(ibinstream& ibs, C& v)

  } // namespace binary

} // namespace datrw

#endif // DATRW_IBINSTREAM_H_VERSION (includeguard)

/* ----- END OF ibinstream.h ----- */
