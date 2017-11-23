/*! \file filters.h
 * \brief provides specific filters (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/01/2003
 * 
 * provides specific filters (prototypes)
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
 *  - 08/01/2003   V1.1   make period mode the default (to be compatible with
 *                        seife command files)
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FILTERS_H_VERSION

#define TF_FILTERS_H_VERSION \
  "TF_FILTERS_H   V1.0"

#include<fourier/polesnzeroes.h>
  
namespace fourier {

  class Filter: public PolesNZeroes {
    public:
      typedef PolesNZeroes Tbase;
      typedef Tbase::Tcvalue Tcvalue;

      static const double pi;
      static const Tcvalue ime;

      Filter(): Tbase(), Mfrequency(false) { }

      void clear() { this->Tbase::clear(); Mfrequency=false; }

      void setfreqmod() { Mfrequency=true; }
      void setpermod() { Mfrequency=false; }
      bool isfreqmod() const { return(Mfrequency); }
      bool ispermod() const { return(!Mfrequency); }

      Tcvalue eval(const double& par) const
      { return(this->eval(Tcvalue(par))); }
      Tcvalue eval(const Tcvalue& par) const
      { return(this->Tbase::operator()(omega(par))); }

      Tcvalue omega(const Tcvalue& par) const;

      void setint();
      void setdif();

      void sethpb(const Tcvalue& par, const int& ord);
      void sethpb(const double& par, const int& ord)
      { sethpb(Tcvalue(par), ord); }

      void setlpb(const Tcvalue& par, const int& ord);
      void setlpb(const double& par, const int& ord)
      { setlpb(Tcvalue(par), ord); }

      void sethp2(const Tcvalue& par, const double& h);
      void sethp2(const double& par, const double& h)
      { sethp2(Tcvalue(par), h); }

      void setlp2(const Tcvalue& par, const double& h);
      void setlp2(const double& par, const double& h)
      { setlp2(Tcvalue(par), h); }
    private:
      // use frequency (or period)
      bool Mfrequency;

  }; // class Filter

} // namespace fourier

#endif // TF_FILTERS_H_VERSION (includeguard)

/* ----- END OF filters.h ----- */
