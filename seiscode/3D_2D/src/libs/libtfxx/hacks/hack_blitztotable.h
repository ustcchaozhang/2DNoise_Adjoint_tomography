/*! \file hack_blitztotable.h
 * \brief dump 1D blitz array to a table (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 03/12/2002
 * 
 * dump 1D blitz array to a table (prototypes)
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
 *  - 03/12/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_HACK_BLITZTOTABLE_H_VERSION

#define TF_HACK_BLITZTOTABLE_H_VERSION \
  "TF_HACK_BLITZTOTABLE_H   V1.0   "

#include<fstream>
#include<cmath>
#include<blitz/array.h>

namespace tfxx {
namespace hacks {

/*! \brief create grepg file from complex array
 *
 * \param any 1D blitz array
 * \param filename name of file to write to
 * \param dx x-sampling to simulate
 */
template<class T>
void write_blitztable(const blitz::Array<T, 1>& array,
                      const char* filename,
                      const double& dx=1.)
{
  std::ofstream os(filename);
  for (int i=array.lbound(blitz::firstDim);
       i<=array.ubound(blitz::secondDim); i++)
  {
    os << dx*(i-1) << " " 
      << array(i) << " "
      << std::abs(array(i)) << std::endl;
  }
}

} // namespace hacks
} // namespace tfxx

#endif // TF_HACK_BLITZTOTABLE_H_VERSION (includeguard)

/* ----- END OF hack_blitztotable.h ----- */
