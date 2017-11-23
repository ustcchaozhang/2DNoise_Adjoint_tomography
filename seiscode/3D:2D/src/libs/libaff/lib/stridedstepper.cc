/*! \file stridedstepper.cc
 * \brief definition of strided stepper functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * definition of strided stepper functions
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
 *  - 08/12/2002   V1.0   copied from libcontxx
 *  - 18/12/2002   V1.1   (thof)
 *                        - reorganized the whole thing
 *                        - removed template parameters
 *                        - removed inline template recursion class
 *                        - use carry arithmetic
 *  - 19/12/2002   V1.2   (thof)
 *                        - introduced Mvalid
 * 
 * ============================================================================
 */

#define AFF_STRIDEDSTEPPER_CC_VERSION \
  "AFF_STRIDEDSTEPPER_DEF_H   V1.2"

#include<aff/lib/stridedstepper.h>

namespace aff {

namespace util {

  //! initialize
  StridedStepper::StridedStepper(const Strided& strided):
    Mshape(strided), Mcurrent(strided.first_offset()),
    Mindex(strided.first()), Mcarry(0), Mvalid(true),
    Mfirst_offset(strided.first_offset()),
    Mlast_offset(strided.last_offset())
    { 
      // calculate Mcarry
      for (Tsubscript i=0; i<(Tshape::Mmax_dimen); i++)
      { Mcarry[i]=strided.size(i)*strided.stride(i); }
    }

  /*----------------------------------------------------------------------*/

  //! next position
  StridedStepper& StridedStepper::incr()
    {
      if (!this->more()) Mvalid=false;
      Tsubscript i=0;
      while (i<Tshape::Mmax_dimen)
      {
        ++Mindex[i];
        Mcurrent+=Mshape.stride(i);
        if (Mindex[i]>Mshape.last(i)) 
        {
          Mindex[i]=Mshape.first(i);
          Mcurrent-=Mcarry[i];
          ++i;
        }
        else
        {
          i=Tshape::Mmax_dimen;
        }
      }
      return(*this);
    }

  /*----------------------------------------------------------------------*/

  //! previous position
  StridedStepper& StridedStepper::decr()
    {
      if (!this->less()) Mvalid=false;
      Tsubscript i=0;
      while (i<Tshape::Mmax_dimen)
      {
        --Mindex[i];
        Mcurrent-=Mshape.stride(i);
        if (Mindex[i]<Mshape.first(i)) 
        {
          Mindex[i]=Mshape.last(i);
          Mcurrent+=Mcarry[i];
          ++i;
        }
        else
        {
          i=Tshape::Mmax_dimen;
        }
      }
      return(*this);
    }

} // namespace util

} // namespace aff

/* ----- END OF stridedstepper.cc ----- */
