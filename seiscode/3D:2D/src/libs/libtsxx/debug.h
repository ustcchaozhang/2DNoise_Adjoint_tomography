/*! \file debug.h
 * \brief debug macro (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2012
 * 
 * debug macro (prototypes)
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 28/01/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TSXX_DEBUG_H_VERSION

#define TSXX_DEBUG_H_VERSION \
  "TSXX_DEBUG_H   V1.0   "

#include<iostream>

/*! \brief produce debug output
 * \ingroup group_util
 *
 * \param C output will be generated if C == true
 * \param N name of function 
 * \param M message to print
 */
#define TSXX_debug(C,N,M) \
  if (C) { \
    std::cerr << "DEBUG (" << N << ", " \
      << __FILE__ << " line #" << __LINE__ << "):" << std::endl \
      << "      " << M << std::endl; \
    std::cerr.flush(); \
  }

/*! \brief report value
 * \ingroup group_util
 *
 * \param V expression variable to report
 *
 * The macro can be used in the argument \par M of DATRW_debug
 */
#define TSXX_value(V)  #V << ": " << V

#endif // TSXX_DEBUG_H_VERSION (includeguard)

/* ----- END OF debug.h ----- */
