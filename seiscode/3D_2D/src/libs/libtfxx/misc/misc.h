/*! \file misc.h
 * \brief some miscellaneous prototypes (prototypes)
 * 
 * \ingroup misc_h
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 18/11/2002
 * 
 * some miscellaneous prototypes (prototypes)
 *
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
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
 *  - 18/11/2002   V1.0   Thomas Forbriger
 *  - 19/11/2002   V1.1   should have reached stbility
 *  - 19/07/2005   V2.0   
 *                        - removed the whole ioswap stuff
 *                        - moved ioswap stuff to its own header file 
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_MISC_H_VERSION

#define TF_MISC_H_VERSION \
  "TF_MISC_H   V2.0"

#include<iostream>

namespace tfxx {

/*! \brief Interface provided through misc.h
 * \defgroup misc_h Interface provided through misc.h
 */

} // namespace tfxx

/*======================================================================*/
// some misc macro functions
//

/*! \brief produce debug output
 * \ingroup misc_h
 *
 * \param C output will be generated if C == true
 * \param N name of function 
 * \param M message to print
 */
#define TFXX_debug(C,N,M) \
  if (C) { \
    std::cerr << "DEBUG (" << N << ", " \
      << __FILE__ << " line #" << __LINE__ << "):" << std::endl \
      << "      " << M << std::endl; \
    std::cerr.flush(); \
  }

/*! \brief output value
 * \ingroup misc_h
 *
 * \param V value to report
 */
#define TFXX_value(V) #V << "=" << V

#endif // TF_MISC_H_VERSION (includeguard)

/* ----- END OF misc.h ----- */
