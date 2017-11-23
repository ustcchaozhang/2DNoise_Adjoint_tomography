/*! \file debug.h
 * \brief suport debugging in libstfinv (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/05/2011
 * 
 * suport debugging in libstfinv (prototypes)
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
 *  - 05/05/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_DEBUG_H_VERSION

#define STFINV_DEBUG_H_VERSION \
  "STFINV_DEBUG_H   V1.0"

/*! \brief produce debug output
 * \ingroup group_debug
 *
 * \param C output will be generated if C == true
 * \param N name of function 
 * \param M message to print
 */
#define STFINV_debug(C,N,M) \
  if (C) { \
    std::cerr << "DEBUG (" << N << ", " \
      << __FILE__ << " line #" << __LINE__ << "):" << std::endl \
      << "      " << M << std::endl; \
    std::cerr.flush(); \
  }

/*! \brief report value in a sequence of output operators
 * \ingroup group_debug
 *
 * \param P parameter to dump
 */
#define STFINV_value( P ) #P << "=" << P

/*! \brief report value of expression
 * \ingroup group_debug
 *
 * \param P parameter to dump
 */
#define STFINV_DUMP( P ) \
  std::cerr << "DEBUG: " \
    << __FILE__ << " line #" << __LINE__ << " " \
    << #P << "=" << P << std::endl;

#endif // STFINV_DEBUG_H_VERSION (includeguard)

/* ----- END OF debug.h ----- */
