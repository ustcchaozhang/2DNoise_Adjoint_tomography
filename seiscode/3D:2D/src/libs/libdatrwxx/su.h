/*! \file su.h
 * \brief read Seismic Unix data (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/11/2010
 * 
 * read Seismic Unix data (prototypes)
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
 *  - 19/11/2010   V1.0   Thomas Forbriger
 *  - 23/11/2010   V1.1   introduced static member data
 *  - 07/06/2011   V1.2   promise constness of series samples
 *  - 21/01/2012   V1.3    
 *                        - prepared isustream and osustream to take modifiers
 *                        - use SUHeaderControl to store format modifier values
 *  - 24/01/2012   V1.4   there is a subtle bute relevant difference in the
 *                        possible orders of member data fields of isustream:
 *                        Mheadercontrol must be first, followed by
 *                        Mnextheader. Otherwise Mnexheader is 
 *                        initialized with a non-initialized Mheadercontrol
 *                        which contains the improper scalco value "0"; the
 *                        wrong order results in warning messages each time
 *                        the code is used and in an unavoidable abort if the
 *                        strict format modifier is set
 *  - 18/11/2016   V1.5   use debug flag in base class
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_SU_H_VERSION

#define DATRW_SU_H_VERSION \
  "DATRW_SU_H   V1.5"

#include <datrwxx/datread.h>
#include <datrwxx/datwrite.h>
#include <datrwxx/suheader.h>
#include <datrwxx/suformat.h>

namespace datrw  {

  namespace su {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace su 

  /*----------------------------------------------------------------------*/

  /*! \brief class to read SeismicUnix data
   *
   * \ingroup group_su
   */
  class isustream: public idatstream {
    public:
      typedef idatstream Tbase;
      isustream(std::istream& is,
                const std::string& modifier="",
                const bool& debug=false);
      virtual ~isustream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    private:
      void readheader();
      datrw::su::options::SUHeaderControl Mheadercontrol;
      datrw::su::SUheader Mnextheader;
  }; // class isustream

  /*----------------------------------------------------------------------*/

  /*! \brief class to write SU data
   *
   * \ingroup group_su
   */
  class osustream: public odatstream {
    public:
      typedef odatstream Tbase;
      osustream(std::ostream& os, 
                const std::string& modifier="",
                const bool& debug=false);
      inline virtual ~osustream() { }
      static void help(std::ostream& os=std::cout);
      static const std::ios_base::openmode openmode;
    protected:
      virtual void writefileheader();
      virtual void writetrace(const Tdseries::Tcoc& series);
      virtual void writetrace(const Tfseries::Tcoc& series);
      virtual void writetrace(const Tiseries::Tcoc& series);
    private:
      unsigned int Mitrace;
      datrw::su::options::SUHeaderControl Mheadercontrol;
  }; // class osustream

} // namespace datrw

#endif // DATRW_SU_H_VERSION (includeguard)

/* ----- END OF su.h ----- */
