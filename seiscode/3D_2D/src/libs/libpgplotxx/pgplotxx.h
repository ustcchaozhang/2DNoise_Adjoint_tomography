/*! \file pgplotxx.h
 * \brief C++ interface for PGPLOT
 *
 * ----------------------------------------------------------------------------
 *
 * Copyright (c) 2001 by Thomas Forbriger (IMGF Frankfurt)
 *
 * C++ interface for PGPLOT
 *
 * most of the code is inline and just a function call interface to
 * libcpgplot.a (coming together with pgplot Fortran version)
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
 *   - 05/06/2001   V1.0   Thomas Forbriger
 *   - 13/02/2002   V1.1   a lot of Trect and Trange manipulation code
 *   - 14/02/2002   V1.2   made all void functions return a device reference
 *   - 15/02/2002   V1.3   opened more of the functions
 *   - 16/02/2002   V1.4   support range swapping
 *   - 07/03/2008   V1.5   implemented Tcoor and Tbbox
 *   - 08/03/2008   V1.5   implemented Tcol
 *   - 31/12/2008   V1.6   reorganized to support remcmmnt awk-script
 *   - 18/01/2011   V1.7   \b !!! \b changed \b interface:
 *                         functions Trange::total() and Trange::absrange()
 *                         are renamed to
 *                         pgplot::Trange::abs() and 
 *                         pgplot::Trange::fullrange()
 *   - 17/03/2015   V1.8   rename file to adopt naming convention in Seitosh
 *
 * ============================================================================
 */
 
#ifndef TF_PGPLOTCPP_H_VERSION
//! include guard
#define TF_PGPLOTCPP_H_VERSION \
  "TF_PGPLOTCPP_H   V1.0   (17-03-2015)"

/*! \mainpage PGPLOT C++ interface
 * \author Thomas Forbriger (IMG Frankfurt)
 * \since 2001
 *
 * This provides a C++ interface to the FORTRAN PGPLOT library. It is based on
 * the C-interface (cpgplot.h) coming with the library.
 *
 * \sa \ref pgplot
 * \sa \ref pgplot::device
 * \sa \ref pgplotcpp_h
 * \sa \ref xpgplotcpp_h
 * \sa \ref affpgplot_h
 * \sa \ref device_h
 * \sa \ref basicdevice_h
 * \sa \ref handle_h
 */

#include <pgplotxx/structs.h>
#include <pgplotxx/basicdevice.h>
#include <pgplotxx/device.h>
 
/*! \brief contains all pgplot stuff
 *
 * This namespace contains all pgplot stuff.
 *
 * It contains all classes and functions and it contains all definitions from
 * cpgplot.h.
 */
namespace pgplot {

/*! \brief Interface provided through pgplotxx.h
 *
 * \defgroup pgplotcpp_h Interface provided through pgplotxx.h
 *
 * \sa \ref device_h
 * \sa \ref basicdevice_h
 */
/*@{*/


/*! \brief Usage texts
 */
/*@{*/
    
extern const char* const usage_escape_sequences;

/*@}*/

/*@}*/
 
}
 
#endif // TF_PGPLOTCPP_H_VERSION
 
/* ----- END OF pgplotxx.h ----- */
