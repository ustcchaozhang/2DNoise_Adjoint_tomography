/*! \file fortranshape.cc
 * \brief prepare information to pass to Fortran subroutines (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 23/12/2002
 * 
 * prepare information to pass to Fortran subroutines (implementation)
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
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 23/12/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define AFF_FORTRANSHAPE_CC_VERSION \
  "AFF_FORTRANSHAPE_CC   V1.0   "

#include <aff/fortranshape.h>
#include <aff/lib/error.h>

namespace aff {
namespace util {

  //! all calculations are done herein
  FortranShape::FortranShape(const Tshape& inshape, const bool& BaseOne)
  {
    // create a local copy of the shape
    Tshape shape(inshape);
    // set first index as required
    if (BaseOne) shape.setfirst(TIndexVec(1));
    // extract first and last index
    Mfirst=shape.first();
    Mlast=shape.last();
    // extract offset to first element
    Moffset=shape.first_offset();
    // first test for Fortran memory layout
    for (Tdim i=1; i<Tshape::Mmax_dimen; i++)
    {
      AFF_assert((shape.stride(i-1)<=shape.stride(i)),
                 "ERROR (FortranShape): no Fortran layout!");
    }
    // native Fortran arrays have stride 1 in base dimension
    // In case we ever want to pass a slice, we have to think about how to
    // pass strides to the Fortran subroutine
    AFF_assert((shape.stride(0)==1),
               "ERROR (FortranShape): No native Fortran array layout!");
    // We assume that every properly sliced array has strides that are integer
    // multiples of the stride of the lower dimension. Just for safety, we
    // test for that assumption.
    for (Tdim i=1; i<Tshape::Mmax_dimen; i++)
    {
      Tsize mult=shape.stride(i)/shape.stride(i-1);
      AFF_assert((shape.stride(i)==(mult*shape.stride(i-1))),
                 "ERROR (FortranShape): Stride is no integer multiple!");
    }
    // calculate dimensioned last
    for (Tdim i=1; i<Tshape::Mmax_dimen; i++)
    {
      Mdimlast[i-1]=shape.stride(i)/shape.stride(i-1)+Mfirst[i-1]-1;
    }
    Mdimlast[Tshape::Mmax_dimen-1]=Mlast[Tshape::Mmax_dimen-1];
    // check bounds (just in case...)
    for (Tdim i=0; i<Tshape::Mmax_dimen; i++)
    {
      AFF_assert((Mlast[i]<=Mdimlast[i]),
                 "ERROR (FortranShape): illegal index range in shape!");
    }
    // that's it...
  }

} // namespace util
} // namespace aff

/* ----- END OF fortranshape.cc ----- */
