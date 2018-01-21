/*! \file readbonjer.h
 * \brief read data obtained in ASCII from K. Bonjer (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/03/2004
 * 
 * read data obtained in ASCII from K. Bonjer (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_READBONJER_H_VERSION

#define DATRW_READBONJER_H_VERSION \
  "DATRW_READBONJER_H   V1.0   "

#include<aff/series.h>
#include<iostream>
#include<libtime++.h>

namespace datrw {

  /*! \brief all functions, classes, etc. to read K. Bonjers data format
   *
   * \defgroup group_bonjer Reading module for: file format defined by K. Bonjer
   */

  /*! \brief all functions, classes, etc. to read K. Bonjers data format
   *
   * \ingroup group_bonjer
   */
  namespace bonjer {

    //! hold file header contents
    struct header {
      std::string station;
      int nsamples;
      std::string filename;
      std::string component;
      libtime::TAbsoluteTime date;
      double rate;
      std::string sensitivity;
      std::string units;
    }; // struct header

    //! we read the data to a vector
    typedef aff::Series<double> Tdata;

    //! function to read the file header
    header readheader(std::istream& is, const bool& verbose=false);
    //! function to read the file data
    Tdata readdata(std::istream& is, const header& hd);

  } // namespace bonjer

} // namespace datrw

#endif // DATRW_READBONJER_H_VERSION (includeguard)

/* ----- END OF readbonjer.h ----- */
