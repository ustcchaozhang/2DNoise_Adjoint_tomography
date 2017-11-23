/*! \file filter.cc
 * \brief filter types (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/01/2003
 * 
 * filter types (implementation)
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
 * 
 * REVISIONS and CHANGES 
 *  - 05/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FILTER_CC_VERSION \
  "TF_FILTER_CC   V1.0"

#include <fourier/filters.h>
#include <fourier/error.h>
#include <cmath>

namespace fourier {

  const double Filter::pi=M_PI;
  const Filter::Tcvalue Filter::ime=Tcvalue(0.,1.);

  /*----------------------------------------------------------------------*/

  Filter::Tcvalue Filter::omega(const Filter::Tcvalue& par) const 
  {
    Tcvalue retval;
    if (isfreqmod()) { retval=2.*pi*par; } else { retval=2.*pi/par; }
    return(retval);
  }

  /*----------------------------------------------------------------------*/

  void Filter::setint()
  {
     this->Tbase::setpole(Tcvalue(0.,0.));
     this->Tbase::denfactor(Tcvalue(0.,1.));
  }

  /*----------------------------------------------------------------------*/

  void Filter::setdif()
  {
     this->Tbase::setzero(Tcvalue(0.,0.));
     this->Tbase::numfactor(Tcvalue(0.,1.));
  }

  /*----------------------------------------------------------------------*/

  void Filter::sethpb(const Filter::Tcvalue& par, const int& ord) 
  {
    Tcvalue om=omega(par);
    FOURIER_assert((ord>0), "ERROR (Filter hpb): illegal order!");
    for (int i=1; i<=ord; ++i)
    {
      this->Tbase::setzero(Tcvalue(0.,0.));
      Tcvalue pole=om*std::exp(ime*pi*(2.*i-1.)/(2.*ord));
      this->Tbase::setpole(pole);
    }
  }

  /*----------------------------------------------------------------------*/

  void Filter::setlpb(const Filter::Tcvalue& par, const int& ord) 
  {
    Tcvalue om=omega(par);
    FOURIER_assert((ord>0), "ERROR (Filter lpb): illegal order!");
    for (int i=1; i<=ord; ++i)
    {
      this->Tbase::numfactor(-ime*om);
      Tcvalue pole=om*std::exp(ime*pi*(2.*i-1.)/(2.*ord));
      this->Tbase::setpole(pole);
    }
  }

  /*----------------------------------------------------------------------*/

  void Filter::sethp2(const Filter::Tcvalue& par, const double& h) 
  {
    Tcvalue om=omega(par);
    this->Tbase::setzero(Tcvalue(0.,0.));
    this->Tbase::setzero(Tcvalue(0.,0.));
    Tcvalue pole=om*(ime*h+std::sqrt(1.-h*h));
    this->Tbase::setpole(pole);
    pole=om*(ime*h-std::sqrt(1.-h*h));
    this->Tbase::setpole(pole);
  }

  /*----------------------------------------------------------------------*/

  void Filter::setlp2(const Filter::Tcvalue& par, const double& h) 
  {
    Tcvalue om=omega(par);
    this->Tbase::numfactor(-ime*om);
    this->Tbase::numfactor(-ime*om);
    Tcvalue pole=om*(ime*h+std::sqrt(1.-h*h));
    this->Tbase::setpole(pole);
    pole=om*(ime*h-std::sqrt(1.-h*h));
    this->Tbase::setpole(pole);
  }

} // namespace fourier

/* ----- END OF filter.cc ----- */
