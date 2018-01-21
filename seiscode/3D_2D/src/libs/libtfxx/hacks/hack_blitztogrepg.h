/*! \file hack_blitztogrepg.h
 * \brief a quick hack to create a grepg file from any 2D blitz array (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/11/2002
 * 
 * a quick hack to create a grepg file from any 2D blitz array (prototypes)
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
 *  - 29/11/2002   V1.0   Thomas Forbriger
 *  - 03/12/2002   V1.1   provides any-type output
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_HACK_BLITZTOGREPG_H_VERSION

#define TF_HACK_BLITZTOGREPG_H_VERSION \
  "TF_HACK_BLITZTOGREPG_H   V1.1"

#include<complex>
#include<blitz/array.h>

namespace tfxx {
namespace hacks {

/*! \brief create grepg file from complex array
 *
 * \param array 2D complex float blitz array
 * \param filename name of file to write to
 * \param dx x-sampling to simulate
 * \param dy y-sampling to simulate
 */
void write_grepg_c(const blitz::Array<std::complex<float>, 2>& array,
                   const char* filename,
                   const double& dx=1., const double& dy=1.);

/*! \brief create grepg file from float array
 *
 * \param array 2D float blitz array
 * \param filename name of file to write to
 * \param dx x-sampling to simulate
 * \param dy y-sampling to simulate
 */
void write_grepg_f(const blitz::Array<float, 2>& array,
                   const char* filename,
                   const double& dx=1., const double& dy=1.);

/*! \brief create grepg file from 2D array of any type
 *
 * \param any 2D blitz array
 * \param filename name of file to write to
 * \param dx x-sampling to simulate
 * \param dy y-sampling to simulate
 */
template<class T>
void write_grepg(const blitz::Array<T, 2>& array,
                 const char* filename,
                 const double& dx=1., const double& dy=1.)
{
  blitz::Array<std::complex<float>, 2> 
    carray(array.lbound(), array.shape(), blitz::fortranArray);
  carray=blitz::cast<std::complex<float> >(array);
  write_grepg_c(carray, filename, dx, dy);
}

} // namespace hacks
} // namespace tfxx

#endif // TF_HACK_BLITZTOGREPG_H_VERSION (includeguard)

/* ----- END OF hack_blitztogrepg.h ----- */
