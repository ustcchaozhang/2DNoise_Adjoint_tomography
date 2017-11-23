/*! \file seriesstepper.cc
 * \brief a stepper class for aff::Series (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/12/2002
 * 
 * a stepper class for aff::Series (implementation)
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
 *  - 20/12/2002   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define AFF_SERIESSTEPPER_CC_VERSION \
  "AFF_SERIESSTEPPER_CC   V1.0"

#include <aff/series.h>

namespace aff {
namespace util {

  //! next element
  SeriesStepper& SeriesStepper::incr()
  {
    if (++Mcurrent>Mlast_offset) 
    { 
      Mcurrent=Mfirst_offset;
      Mvalid=false;
    }
    return(*this);
  }

  /*----------------------------------------------------------------------*/

  //! previous element
  SeriesStepper& SeriesStepper::decr()
  {
    if (--Mcurrent<Mfirst_offset) 
    { 
      Mcurrent=Mlast_offset;
      Mvalid=false;
    }
    return(*this);
  }

  /*----------------------------------------------------------------------*/

  //! skip to first
  SeriesStepper& SeriesStepper::tofirst()
  {
    Mcurrent=Mfirst_offset;
    Mvalid=true;
    return(*this);
  }

  /*----------------------------------------------------------------------*/

  //! skip to last
  SeriesStepper& SeriesStepper::tolast()
  {
    Mcurrent=Mlast_offset;
    Mvalid=true;
    return(*this);
  }


} // namespace util
} // namespace aff

/* ----- END OF seriesstepper.cc ----- */
