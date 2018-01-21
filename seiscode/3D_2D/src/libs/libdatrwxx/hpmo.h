/*! \file hpmo.h
 * \brief provide data from HP Mo (BFO data acquisition system) (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/03/2004
 * 
 * provide data from HP Mo (BFO data acquisition system) (prototypes)
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
 *  - 23/11/2010   V1.1   introduced static members
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_HPMO_H_VERSION

#define DATRW_HPMO_H_VERSION \
  "DATRW_HPMO_H   V1.1"

#include<datrwxx/datread.h>
#include<datrwxx/hpmodata.h>
#include<libtime++.h>

namespace datrw {

  namespace hpmo {

    extern const bool isbinary;
    extern const char* const streamID;

  } // namespace hpmo 

  /*----------------------------------------------------------------------*/

  /*! \brief class to read HPMO data
   *
   * \ingroup group_hpmo
   */
  class ihpmostream: public idatstream {
    public:
      typedef idatstream Tbase;
      ihpmostream(std::istream& is, const bool& verbose=false);
      virtual ~ihpmostream() { }
      virtual Tdseries dseries();
      virtual Tfseries fseries();
      //virtual Tiseries iseries();
      virtual void skipseries();
      static void help(std::ostream& os=std::cout)
      { Tbase::help(os, "ihpmostream"); }
      static const std::ios_base::openmode openmode;
    private:
      //! read one data file
      void read(std::istream& is, const bool& verbose=false);
      //! set header for next request
      void set_next_header();
      //! buffer to hold one data file
      datrw::hpmo::MinuteBlock Mdatafile[datrw::hpmo::nminutes];
      int Mindex[datrw::hpmo::nminutes]; //<! index to contiguous blocks
      int Mnblocks; //<! number of contiguous blocks
      int Mnextblock; //<! next block to extract
      int Mnextchannel; //<! next channel to extract
  }; // class ihpmostream

} // namespace datrw

#endif // DATRW_HPMO_H_VERSION (includeguard)

/* ----- END OF hpmo.h ----- */
