/*! \file polesnzeroes.cc
 * \brief pole and zero filters (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/01/2003
 * 
 * pole and zero filters (implementation)
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
 *
 * ----
 * libfourier is free software; you can redistribute it and/or modify
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
 *  - 05/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_POLESNZEROES_CC_VERSION \
  "TF_POLESNZEROES_CC   V1.0   "

#include <fourier/polesnzeroes.h>

namespace fourier {

  /*----------------------------------------------------------------------*/

  void PolesNZeroes::clear() 
  {
    Mpoles.clear(); Mzeroes.clear(); 
    Mnumerator=1.; Mdenominator=1.;
    Mnormal=true;
  }

  /*----------------------------------------------------------------------*/

  void PolesNZeroes::setpole(const PolesNZeroes::Tcvalue& pole) 
  {
    if (isnormal()) 
    { Mpoles.push_back(pole); }
    else
    { Mzeroes.push_back(pole); }
  }

  /*----------------------------------------------------------------------*/

  void PolesNZeroes::setzero(const PolesNZeroes::Tcvalue& zero) 
  {
    if (isnormal()) 
    { Mzeroes.push_back(zero); }
    else
    { Mpoles.push_back(zero); }
  }

  /*----------------------------------------------------------------------*/

  void PolesNZeroes::numfactor(const PolesNZeroes::Tcvalue& factor) 
  {
    if (isnormal()) 
    { Mnumerator *= factor; }
    else
    { Mdenominator *= factor; }
  }

  /*----------------------------------------------------------------------*/

  void PolesNZeroes::denfactor(const PolesNZeroes::Tcvalue& factor) 
  {
    if (isnormal()) 
    { Mdenominator *= factor; }
    else
    { Mnumerator *= factor; }
  }

  /*----------------------------------------------------------------------*/

  PolesNZeroes::Tcvalue PolesNZeroes::operator()(const Tcvalue& omega)  const
  {
    const double waterlevel=1.e-30;
    Tcvalue om = std::abs(omega) > waterlevel ? omega : waterlevel;
    Tcvalue numerator=Mnumerator;
    Tcvalue denominator=Mdenominator;
    Tlist::const_iterator in=Mzeroes.begin();
    while (in != Mzeroes.end())
    { numerator *= (om - *in); ++in; }
    Tlist::const_iterator id=Mpoles.begin();
    while (id != Mpoles.end())
    { denominator *= (om - *id); ++id; }
    return(numerator/denominator);
  }

} // namespace fourier

/* ----- END OF polesnzeroes.cc ----- */
