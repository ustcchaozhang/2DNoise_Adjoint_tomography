/*! \file writeany.h
 * \brief common interface for all data types (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/02/2010
 * 
 * common interface for all data types (prototypes)
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
 * REVISIONS and CHANGES 
 *  - 17/02/2010   V1.0   Thomas Forbriger
 *  - 26/11/2010   V1.1   moved not input specifc part to formats.h
 *  - 07/06/2011   V1.2   promise constness of series samples
 *  - 29/07/2011   V1.3   added constructor which accepts format modifiers
 *                        support property query
 *  - 07/09/2011   V1.4   more string type format ID support: openmode
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_WRITEANY_H_VERSION

#define DATRW_WRITEANY_H_VERSION \
  "DATRW_WRITEANY_H   V1.4"

#include<string>
#include<datrwxx/datwrite.h>
#include<datrwxx/properties.h>

namespace datrw {

  /*! \brief General module to access all output stream types
   *
   * \defgroup group_writeany API: Write any format
   */

  /*! \brief Delegate member function to underlying output stream
   *
   * \ingroup group_writeany
   */
#define ANYDELEGATE( function ) function() const { return(Mos->function()); }

  /*! \brief Class to write any type of data file
   *
   * \ingroup group_writeany
   *
   * Notice: This does not work by inheritance. To select a specific 
   * type of output stream, we have to keep a reference and to delegate
   * any function calls. But this is only necessary for functions that have to
   * be called as a member function directly. Functions for which an output
   * operator is defined, can by called by the stream output syntax.
   * A delegating output operator will be defined below.
   */
  class oanystream {
    public:
      /*! \brief deprecated constructor, not taking format modifier
       * \deprecated
       * The use of this constructor is deprecated, because it does not allow
       * format modifiers to be passed by the user.
       */
      oanystream(std::ostream&, const Eformat& format,
                 const bool& debug=false);
      //! \brief This constructor accepts format modifiers
      oanystream(std::ostream&, const std::string& format,
                 const bool& debug=false);
      ~oanystream();
      datrw::odatstream& odatstream() { return(*Mos); }
      void ANYDELEGATE( flushfileheader )
      bool ANYDELEGATE( handlesfilefree )
      bool ANYDELEGATE( handlestracefree )
      bool ANYDELEGATE( handlessrce )
      bool ANYDELEGATE( handlesinfo )
      Properties ANYDELEGATE( properties )
      Edatatype ANYDELEGATE( seriestype )
      static std::ios_base::openmode openmode(const Eformat& format);
      static std::ios_base::openmode openmode(const std::string& format);
    private:
      //! \brief actually open stream (to be called by constructor)
      void open(std::ostream& os, std::string format,
                const bool& debug=false);
      //! \brief no copy
      oanystream(const oanystream&);
      //! \brief no copy
      oanystream& operator=(const oanystream&);
      datrw::odatstream* Mos;
      datrw::Eformat Mformat;
  }; // class ianystream

#undef ANYDELEGATE

  /*! \brief general output operator
   *
   * \ingroup group_writeany
   *
   * This function template delegates the actual output to the 
   * output operators for datrw::odatstream by accessing the underlying
   * datrw::odatstream through the type conversion operator 
   * datrw::oanystream::odatstream()
   */
  template<class C>
  oanystream& operator<<(oanystream& os, const C& c) 
  { os.odatstream() << c; return(os); }

} // namespace datrw

#endif // DATRW_WRITEANY_H_VERSION (includeguard)

/* ----- END OF writeany.h ----- */
