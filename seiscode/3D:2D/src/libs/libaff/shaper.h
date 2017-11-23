/*! \file shaper.h
 * \brief rectangular Fortran array layout (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \since 08/12/2002
 * 
 * rectangular Fortran array layout (prototypes)
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
 *  - 11/12/2002   V1.1   rework class (thof)
 *                        - remove template parameters
 *  - 12/12/2002   V1.2   (thof)
 *                        - has now full functionality
 *                        - documentation ist still missing
 *  - 15/12/2002   V1.3   (thof)
 *                        - never place in namspace prebuilt
 *  - 18/12/2002   V1.4   (thof)
 *                        - small but essential correction in operator()
 *  - 09/01/2003   V1.5   (thof)
 *                        - now throws exception in case of too many
 *                          concatenated bracket operators
 *  - 14/05/2011   V1.6   (thof)
 *                        - removed obsolete reference to libcontxx in
 *                          documentation
 * 
 * ============================================================================
 */

// include guard
#ifndef AFF_SHAPER_H_VERSION

#define AFF_SHAPER_H_VERSION \
  "AFF_SHAPER_H   V1.6"

#include<aff/lib/strided.h>
#include<aff/lib/error.h>

namespace aff {

/*! \brief Shaper class for Fortran layout
 *
 * \todo 
 * rework documentation of class Shaper
 */
class Shaper 
{
  public:
    //! Type of Shape class
    typedef Strided Tshape;
    //! First dimension is defined by constructor
    Shaper(const Tsubscript& last): 
      Mfirst(1), Mlast(1), Mmaxlast(1), Mdim(1)
    {
      AFF_assert((last>=1), "ERROR (shaper): invalid limit");
      Mfirst[0]=1;
      Mlast[0]=last;
      Mmaxlast[0]=last;
    }
    //! First dimension is defined by constructor
    Shaper(const Tsubscript& first, const Tsubscript& last):
      Mfirst(1), Mlast(1), Mmaxlast(1), Mdim(1)
    {
      AFF_assert((last>=first), "ERROR (shaper): invalid index range");
      Mfirst[0]=first;
      Mlast[0]=last;
      Mmaxlast[0]=last;
    }
    //! First dimension is defined by constructor
    Shaper(const Tsubscript& first, const Tsubscript& last,
           const Tsubscript& maxlast):
      Mfirst(1), Mlast(1), Mmaxlast(1), Mdim(1)
    {
      AFF_assert((last>=first), "ERROR (shaper): invalid index range");
      AFF_assert((maxlast>=last), "ERROR (shaper): invalid alloc size");
      Mfirst[0]=first;
      Mlast[0]=last;
      Mmaxlast[0]=maxlast;
    }
    //! Other dimensions are defined by bracket operator
    Shaper& operator() (const Tsubscript& last)
    {
      AFF_assert((last>=1), "ERROR (shaper): invalid limit");
      check_dim();
      Mfirst[Mdim]=1;
      Mlast[Mdim]=last;
      Mmaxlast[Mdim]=last;
      Mdim++;
      return(*this);
    }
    //! Other dimensions are defined by bracket operator
    Shaper& operator() (const Tsubscript& first, const Tsubscript& last)
    {
      AFF_assert((last>=first), "ERROR (shaper): invalid index range");
      check_dim();
      Mfirst[Mdim]=first;
      Mlast[Mdim]=last;
      Mmaxlast[Mdim]=last;
      Mdim++;
      return(*this);
    }
    //! Other dimensions are defined by bracket operator
    Shaper& operator() (const Tsubscript& first, const Tsubscript& last,
                        const Tsubscript& maxlast)
    {
      AFF_assert((last>=first), "ERROR (shaper): invalid index range");
      AFF_assert((maxlast>=last), "ERROR (shaper): invalid alloc size");
      check_dim();
      Mfirst[Mdim]=first;
      Mlast[Mdim]=last;
      Mmaxlast[Mdim]=maxlast;
      Mdim++;
      return(*this);
    }
    //! Return the shape class
    operator Tshape () const
    {
      Tshape shape(Mfirst, Mmaxlast);
      shape.shrink(Mlast);
      return(shape);
    }
  private:
    //! check dimensions
    void check_dim() const
    {
      AFF_assert((Mdim<Tshape::Mmax_dimen),
                 "ERROR (Shaper): you use too many dimensions!");
    }
    //! limits
    Tshape::TIndexVec Mfirst, Mlast, Mmaxlast;
    //! next dimension to set
    Tdim Mdim;
}; // Shaper

} // namespace aff

#endif // AFF_SHAPER_H_VERSION (includeguard)

/* ----- END OF shaper.h ----- */
