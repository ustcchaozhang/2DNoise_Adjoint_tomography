/*! \file readany.h
 * \brief provides all specific data reading classes (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/03/2004
 * 
 * provides all specific data reading classes (prototypes)
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
 *  - 31/03/2004   V1.0   Thomas Forbriger
 *  - 29/06/2007   V1.1   added SAC binary format
 *  - 19/09/2007   V1.2   added raw GSE format
 *  - 12/11/2009   V1.3   added TSOFT data
 *  - 06/10/2010   V1.4   added ASCII format of T. Forbrigers any2ascii
 *  - 23/11/2010   V1.5   use static members
 *  - 25/11/2010   V1.6   added Seismic Unix format
 *  - 26/11/2010   V1.7   moved not input specifc part to formats.h
 *  - 29/07/2011   V1.8   added constructor accepting format modifiers
 *                        support property query
 *  - 07/09/2011   V1.9   more string type format ID support: openmode
 *  - 29/11/2011   V1.10  present complete idatstream interface
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_READANY_H_VERSION

#define DATRW_READANY_H_VERSION \
  "DATRW_READANY_H   V1.10"

#include<string>
#include<datrwxx/datread.h>
#include<datrwxx/formats.h>
#include<datrwxx/properties.h>

namespace datrw {

  /*! \brief General module to access all input stream types
   *
   * \defgroup group_readany API: Read any format
   */

  /*! \brief Delegate member function to underlying input stream
   *
   * \ingroup group_readany
   */
#define ANYDELEGATE( function ) function() const { return(Mis->function()); }

  /*! \brief Class to read any type of data file
   *
   * \ingroup group_readany
   *
   * Notice: This does not work by inheritance. To select a specific 
   * type of input stream, we have to keep a reference and to delegate
   * any function calls. But this is only necessary for functions that have to
   * be called as a member function directly. Functions for which an input
   * operator is defined, can by called by the stream input syntax.
   * A delegating input operator will be defined below.
   */
  class ianystream {
    public:
      /*! \brief deprecated constructor, not taking format modifier
       * \deprecated
       * The use of this constructor is deprecated, because it does not allow
       * format modifiers to be passed by the user.
       */
      ianystream(std::istream&, const Eformat& format,
                 const bool& debug=false);
      //! \brief This constructor accepts format modifiers
      ianystream(std::istream&, const std::string& format,
                 const bool& debug=false);
      ~ianystream();
      datrw::idatstream& idatstream() { return(*Mis); }
      bool ANYDELEGATE( last )
      bool ANYDELEGATE( good )
      bool ANYDELEGATE( hasfree )
      bool ANYDELEGATE( hassrce )
      bool ANYDELEGATE( hasinfo )
      bool ANYDELEGATE( providesd )
      bool ANYDELEGATE( providesf )
      bool ANYDELEGATE( providesi )
      void ANYDELEGATE( skipseries )
      Tdseries ANYDELEGATE( dseries )
      Tfseries ANYDELEGATE( fseries )
      Tiseries ANYDELEGATE( iseries )
      ::sff::INFO ANYDELEGATE( info )
      ::sff::WID2 ANYDELEGATE( wid2 )
      ::sff::FREE ANYDELEGATE( free )
      ::sff::SRCE ANYDELEGATE( srce )
      Properties ANYDELEGATE( properties )
      static std::ios_base::openmode openmode(const Eformat& format);
      static std::ios_base::openmode openmode(const std::string& format);
    private:
      //! \brief actually open stream (to be called by constructor)
      void open(std::istream& os, std::string format,
                const bool& debug=false);
      //! \brief no copy
      ianystream(const ianystream&);
      //! \brief no copy
      ianystream& operator=(const ianystream&);
      datrw::idatstream* Mis;
      datrw::Eformat Mformat;
  }; // class ianystream

#undef ANYDELEGATE

  /*! \brief general input operator
   *
   * \ingroup group_readany
   *
   * This function template delegates the actual output to the 
   * output operators for datrw::idatstream by accessing the underlying
   * datrw::idatstream through the type conversion operator 
   * datrw::ianystream::idatstream()
   */
  template<class C>
  ianystream& operator>>(ianystream& is, C& c) 
  { is.idatstream() >> c; return(is); }

} // namespace datrw

#endif // DATRW_READANY_H_VERSION (includeguard)

/* ----- END OF readany.h ----- */
