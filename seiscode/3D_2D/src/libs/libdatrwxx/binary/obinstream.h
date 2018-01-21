/*! \file obinstream.h
 * \brief basic binary output stream (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/11/2011
 * 
 * basic binary output stream (prototypes)
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
#ifndef DATRW_OBINSTREAM_H_VERSION

#define DATRW_OBINSTREAM_H_VERSION \
  "DATRW_OBINSTREAM_H   V1.0   "

#include <iostream>
#include <fstream>
#include <string>
#include <libtime++.h>
#include <datrwxx/types.h>
#include <sffxx.h>

namespace datrw {

  namespace binary {

    /*! \brief binary output for basic types and classes
     * \ingroup group_binary
     */
    class obinstream {
      public:
        obinstream(std::ostream& os,
                   const char* const magic,
                   const bool& debug=false);
        void write(const char& v);
        void write(const short& v);
        void write(const int& v);
        void write(const unsigned int& v);
        void write(const double& v);
        void write(const float& v);
        void write(const std::string& v);
        void write(const libtime::TAbsoluteTime& v);
        void write(const ::sff::FREE& v);
        void write(const ::sff::WID2& v);
        void write(const ::sff::SRCE& v);
        void write(const ::sff::INFO& v);
        void write(const Tdseries::Tcoc& v);
        void write(const Tfseries::Tcoc& v);
        void write(const Tiseries::Tcoc& v);
      private:
        std::ostream& Mos;
        bool Mdebug;
    }; // class obinstream

    /*----------------------------------------------------------------------*/

    /*! \brief output operator for basic types and classes
     * \ingroup group_binary
     */
    template<class C>
      obinstream& operator<<(obinstream& obs, const C& v)
      {
        obs.write(v); return(obs);
      } // obinstream& operator<<(obinstream& obs, const C& v)

  } // namespace binary

} // namespace datrw

#endif // DATRW_OBINSTREAM_H_VERSION (includeguard)

/* ----- END OF obinstream.h ----- */
