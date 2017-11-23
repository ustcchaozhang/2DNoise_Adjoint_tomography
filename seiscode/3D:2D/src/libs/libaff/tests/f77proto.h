/*! \file f77proto.h
 * \brief prototypes for Fortran interface (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/12/2002
 * 
 * prototypes for Fortran interface (prototypes)
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
 * \sa \ref page_fortran
 * \sa f77interface
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 22/12/2002   V1.0   Thomas Forbriger
 *  - 27/12/2002   V1.1   (thof)
 *                        - now provides some documentation
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_F77PROTO_H_VERSION

#define AFF_F77PROTO_H_VERSION \
  "AFF_F77PROTO_H   V1.1"

#include<aff/array.h>

#include<complex>

/*! \brief This namespace collects all test functions for interfacing Fortran 77
 * 
 * \sa \ref page_fortran
 * \sa tests/f77test.cc
 * \sa tests/f77interface.cc
 */
namespace f77interface {

typedef aff::Array<std::complex<float> > Tcarray;
typedef aff::Array<std::complex<double> > Tzarray;

/*! \brief fill an AFF array thorugh a Fortran subroutine
 *
 * An aff::Array<int> object is passed to the Fortran subroutine fill and is
 * filled with values there. 
 * The concept is discussed on page "\ref page_fortran".
 */
int fill(const aff::Array<int>& fa);

/*! \brief fill the Fortran common-block array
 *
 * Two aff::Array<float> objects are passed to the Fortran subroutine
 * fillarray which calculates complex values from the elements of these arrays
 * and fills the array in common-block \c f77common which is defined in
 * tests/f77common.inc.
 *
 * The concept is discussed on page "\ref page_fortran".
 */
int fillarray(const aff::Array<float>& v1,
              const aff::Array<float>& v2);

/*! \brief returns results from Fortran subroutine sums
 *
 * The Fortran subroutine sums calculates column-sums from the array in the
 * common block. These values are return in an 
 * \code aff::Array<std::complex<float> > \endcode object.
 *
 * The concept is discussed on page "\ref page_fortran".
 */
Tcarray sums();

/*! \brief returns direct access to Fortran common block
 *
 * This function returns a 
 * \code aff::Array<std::complex<double> > \endcode object, which offers
 * direct read/write access to the array in the Fortran common block
 * f77common.
 *
 * The concept is discussed on page "\ref page_fortran".
 */
Tzarray viewcommon();

} // namespace f77interface


#endif // AFF_F77PROTO_H_VERSION (includeguard)

/* ----- END OF f77proto.h ----- */
