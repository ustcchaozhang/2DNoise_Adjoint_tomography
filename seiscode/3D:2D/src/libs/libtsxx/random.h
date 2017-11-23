/*! \file random.h
 * \brief create a random series (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/06/2006
 * 
 * create a random series (prototypes)
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
 * 
 * REVISIONS and CHANGES 
 *  - 27/06/2006   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TS_RANDOM_H_VERSION

#define TS_RANDOM_H_VERSION \
  "TS_RANDOM_H   V1.0"

#include<aff/series.h>

namespace ts {

  /*! random noise signal toolbox
   */
  namespace rnd {

    typedef aff::Series<double> Tdseries;

    //! return gaussian uniform noise (standard dev=1, zero mean)
    Tdseries dugauss(const int& n);

  } // namespace rnd

} // namespace ts

#endif // TS_RANDOM_H_VERSION (includeguard)

/* ----- END OF random.h ----- */
