/*! \file seriesstepper.h
 * \brief a stepper class for aff::Series (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/12/2002
 * 
 * a stepper class for aff::Series (prototypes)
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
 * This file is automatically included through aff/series.h
 * 
 * Copyright (c) 2002 by Thomas Forbriger (IMG Frankfurt) 
 * 
 * REVISIONS and CHANGES 
 *  - 20/12/2002   V1.0   Thomas Forbriger
 *  - 23/12/2002   V1.1   (thof)
 *                        - this class had no public interface
 *  - 23/12/2003   V1.2   (thof)
 *                        - constructor must take shape as argument
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SERIESSTEPPER_H_VERSION

#define AFF_SERIESSTEPPER_H_VERSION \
  "AFF_SERIESSTEPPER_H   V1.2"

#include<aff/lib/types.h>

namespace aff {
namespace util {

  /*! \brief A stepper for aff::Series
   *
   * If we provide a stepper class, The aff::Iterator template can work with
   * the aff::Series. 
   *
   * This class cycles through the offset range of an aff::Series. It is used
   * internally within aff::Series::operator=() for value assignment. And it
   * is used within the aff::Iterator and aff::Browser classes. You normally
   * will not use it directly.
   */
  class SeriesStepper 
  {
    public:
      //! only non-copy constructor
      SeriesStepper(const LinearShape& shape):
        Mfirst_offset(shape.offset(shape.first())),
        Mlast_offset(shape.offset(shape.last())),
        Mbase(shape.base()),
        Mcurrent(Mfirst_offset), Mvalid(true) { }
      //! return current index value for Representation access
      const Tsubscript& current() const { return(Mcurrent); }
      //! return current index vector for array access
      Tsubscript index() const { return(Mcurrent+Mbase); }
      //! increment offset - return reference to itself
      SeriesStepper& incr();
      //! decrement offset - return reference to itself
      SeriesStepper& decr();
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
      SeriesStepper& tofirst();
      //! set current element index to the last - return reference to itself
      SeriesStepper& tolast();
    private:
      //! store current bounds to reduce execution time 
      Tsubscript Mfirst_offset, Mlast_offset;
      //! hold base to be able to return index value
      Tsubscript Mbase;
      //! hold current position offset to memory
      Tsubscript Mcurrent;
      //! true while not passed start or end
      bool Mvalid;
  }; // class SeriesStepper

} // namespace util
} // namespace aff

#endif // AFF_SERIESSTEPPER_H_VERSION (includeguard)

/* ----- END OF seriesstepper.h ----- */
