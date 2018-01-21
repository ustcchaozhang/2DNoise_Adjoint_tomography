/*! \file pdasflags.h
 * \brief some flags that are internal to the PDAS reading functions (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 10/09/2004
 * 
 * some flags that are internal to the PDAS reading functions (prototypes)
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
 *  - 10/09/2004   V1.0   Thomas Forbriger
 *  - 19/10/2004   V1.1   support gainranged format
 * 
 * ============================================================================
 */

// include guard
#ifndef DATRW_PDASFLAGS_H_VERSION

#define DATRW_PDASFLAGS_H_VERSION \
  "DATRW_PDASFLAGS_H   V1.0   "

namespace datrw {

  namespace pdas {

    //! define pdas data types
    enum Etype {
      FtypeINT,
      FtypeLONG,
      FtypeGAINRANGED
    }; // enum Epdastype

  } // namespace pdas

} // namespace datrw

#endif // DATRW_PDASFLAGS_H_VERSION (includeguard)

/* ----- END OF pdasflags.h ----- */
