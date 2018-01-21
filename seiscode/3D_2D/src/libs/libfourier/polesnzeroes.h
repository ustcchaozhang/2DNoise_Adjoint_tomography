/*! \file polesnzeroes.h
 * \brief poles and zeros representation (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/01/2003
 * 
 * poles and zeros representation (prototypes)
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

// include guard
#ifndef TF_POLESNZEROES_H_VERSION

#define TF_POLESNZEROES_H_VERSION \
  "TF_POLESNZEROES_H   V1.0   "

#include<cmath>
#include<complex>
#include<vector>

namespace fourier {

  class PolesNZeroes {
    public:
      typedef std::complex<double> Tcvalue;
      typedef std::vector<Tcvalue> Tlist;
      PolesNZeroes(): Mpoles(), Mzeroes(), Mnumerator(1.),
        Mdenominator(1.), Mnormal(true) { }

      bool isnormal() const { return(Mnormal); }
      bool isinverse() const { return(!Mnormal); }

      void clear();

      void setnormal() { Mnormal=true; }
      void setinverse() { Mnormal=false; }

      void setpole(const Tcvalue& pole);
      void setzero(const Tcvalue& zero);

      void numfactor(const Tcvalue& factor);
      void numfactor(const double& factor) 
      { this->numfactor(Tcvalue(factor)); };

      void denfactor(const Tcvalue& factor);
      void denfactor(const double& factor)
      { this->denfactor(Tcvalue(factor)); };

      Tcvalue operator()(const double& omega) const
      { return(this->operator()(Tcvalue(omega))); }
      Tcvalue operator()(const Tcvalue& omega) const;
    private:
      // poles
      Tlist Mpoles;
      // zeroes
      Tlist Mzeroes;
      // scale factor
      Tcvalue Mnumerator;
      // scale factor
      Tcvalue Mdenominator;
      // normal (or inverse) 
      bool Mnormal;
  }; // class PolesNZeroes

} // namespace fourier

#endif // TF_POLESNZEROES_H_VERSION (includeguard)

/* ----- END OF polesnzeroes.h ----- */
