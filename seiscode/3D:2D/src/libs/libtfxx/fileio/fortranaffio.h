/*! \file fortranaffio.h
 * \brief input/ouput to/from aff containers (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/05/2006
 * 
 * input/ouput to/from aff containers (prototypes)
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 16/05/2006   V1.0   Thomas Forbriger
 *  - 13/06/2006   V1.1   we have to increase the iterator :-)
 *  - 19/06/2006   V1.2   provide a reading operator
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FORTRANAFFIO_H_VERSION

#define TF_FORTRANAFFIO_H_VERSION \
  "TF_FORTRANAFFIO_H   V1.2"

#include <aff/array.h>
#include <aff/series.h>
#include <aff/iterator.h>
#include <tfxx/fortranio.h>

namespace tfxx {
  namespace fortranio {

    //! read an aff container from the file
    template<class C>
    void readaff(tfxx::fortranio::FortranBinInput& is, C& s)
    {
      aff::Iterator<C> i(s);
      while (i.valid()) { is >> *i; ++i; }
    }

    //! reading operator
    template<class T>
    tfxx::fortranio::FortranBinInput& 
    operator >>(tfxx::fortranio::FortranBinInput& is, aff::Series<T>& s)
    {
      readaff(is, s);
      return(is);
    }

    //! reading operator
    template<class T>
    tfxx::fortranio::FortranBinInput& 
    operator >>(tfxx::fortranio::FortranBinInput& is, aff::Array<T>& s)
    {
      readaff(is, s);
      return(is);
    }

  } // namespace fortranio
} // namespace tfxx

#endif // TF_FORTRANAFFIO_H_VERSION (includeguard)

/* ----- END OF fortranaffio.h ----- */
