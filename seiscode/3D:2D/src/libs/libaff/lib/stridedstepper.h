/*! \file stridedstepper.h
 * \brief Stepper class for strided shapes (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * Stepper class for strided shapes (prototypes)
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
 * \note
 * You will not include this header directly. It is included through array.h
 * or binarray.h
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 15/12/2002   V1.1   (thof)
 *                        - never place in namespace prebuilt
 *  - 18/12/2002   V1.2   (thof)
 *                        - reorganized the whole thing
 *  - 19/12/2002   V1.3   (thof)
 *                        - introduced Mvalid;
 *                          cycling full array needs and invalidate AFTER
 *                          passing the first or last element
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_STRIDEDSTEPPER_H_VERSION

#define AFF_STRIDEDSTEPPER_H_VERSION \
  "AFF_STRIDEDSTEPPER,H_H   V1.3"

#include<aff/lib/strided.h>

namespace aff {
namespace util {

/*! \brief A stepper for all strided shapes.
 *
 * \note
 * The Stepper holds a reference to the shape. It is designed to be a
 * temporary object.
 */
class StridedStepper 
{
  public:
    //! Type of Shape corresponding class
    typedef aff::Strided Tshape;
    //! type of stride array
    typedef Tshape::TIndexVec TIndexVec;
    //! type of limit arrays
    typedef Tshape::TSizeVec TSizeVec;
    //
    //! only non-copy constructor
    StridedStepper(const Strided& strided);
    //! return current index value for Representation access
    const Tsubscript& current() const { return(Mcurrent); }
    //! return current index vector for array access
    const TIndexVec& index() const { return(Mindex); }
    //! return current index vector for array access
    const Tsubscript& index(const Tdim& i) const { return(Mindex[i]); }
    //! increment offset - return reference to itself
    StridedStepper& incr();
    //! decrement offset - return reference to itself
    StridedStepper& decr();
    //! returns true if there are more elements in incr-direction
    bool more() const
    { return(Mcurrent<Mlast_offset); }
    //! returns true if there are more elements in decr-direction
    bool less() const
    { return(Mcurrent>Mfirst_offset); }
    //! valid if not passed end or beginning
    const bool& valid() const
    { return(Mvalid); }
    //! set current element index to the first - return reference to itself
    StridedStepper& tofirst()
    {
      Mindex=Mshape.first();
      Mcurrent=Mshape.first_offset();
      Mvalid=true;
      return(*this);
    }
    //! set current element index to the last - return reference to itself
    StridedStepper& tolast()
    {
      Mindex=Mshape.last();
      Mcurrent=Mshape.last_offset();
      Mvalid=true;
      return(*this);
    }
  private:
    //! hold reference to shape
    const Tshape& Mshape;
    //! hold current position offset to memory
    Tsubscript Mcurrent;
    //! hold current index vector
    TIndexVec Mindex;
    //! hold carry values
    TSizeVec Mcarry;
    //! cycle check
    bool Mvalid;
    //! store current bounds to reduce execution time 
    Tsubscript Mfirst_offset, Mlast_offset;

}; // StridedStepper

} // namespace util

} // namespace aff

#endif // AFF_STRIDEDSTEPPER_H_VERSION (includeguard)

/* ----- END OF stridedstepper.h ----- */
