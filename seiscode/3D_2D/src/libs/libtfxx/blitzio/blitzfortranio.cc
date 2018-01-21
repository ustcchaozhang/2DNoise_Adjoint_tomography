/*! \file blitzfortranio.cc
 * \brief instantiate our magic numbers (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/11/2002
 * 
 * instantiate our magic numbers (implementation)
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
 *  - 28/11/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_BLITZFORTRANIO_CC_VERSION \
  "TF_BLITZFORTRANIO_CC   V1.0   "

#define TF_COMPLEX_ARRAY
#include <tfxx/blitzfortranio.h>

namespace tfxx {
namespace fortranio {

const char blitz_magic<float, 1>::magic[];
const char blitz_magic<float, 2>::magic[];
const char blitz_magic<float, 3>::magic[];
const char blitz_magic<float, 4>::magic[];
const char blitz_magic<float, 5>::magic[];

const char blitz_magic<double, 1>::magic[];
const char blitz_magic<double, 2>::magic[];
const char blitz_magic<double, 3>::magic[];
const char blitz_magic<double, 4>::magic[];
const char blitz_magic<double, 5>::magic[];

const char blitz_magic<int, 1>::magic[];
const char blitz_magic<int, 2>::magic[];
const char blitz_magic<int, 3>::magic[];
const char blitz_magic<int, 4>::magic[];
const char blitz_magic<int, 5>::magic[];

const char blitz_magic<std::complex<float>, 1>::magic[];
const char blitz_magic<std::complex<float>, 2>::magic[];
const char blitz_magic<std::complex<float>, 3>::magic[];
const char blitz_magic<std::complex<float>, 4>::magic[];
const char blitz_magic<std::complex<float>, 5>::magic[];

const char blitz_magic<std::complex<double>, 1>::magic[];
const char blitz_magic<std::complex<double>, 2>::magic[];
const char blitz_magic<std::complex<double>, 3>::magic[];
const char blitz_magic<std::complex<double>, 4>::magic[];
const char blitz_magic<std::complex<double>, 5>::magic[];
  
} // namespace fortranio
} // namespace tfxx

/* ----- END OF blitzfortranio.cc ----- */
