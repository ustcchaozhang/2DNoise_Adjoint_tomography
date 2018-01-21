/*! \file srcires.h
 * \brief source wavelet impulse response (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 04/01/2003
 * 
 * source wavelet impulse response (prototypes)
 * 
 * Copyright (c) 2003 by Thomas Forbriger (IMG Frankfurt) 
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
 * REVISIONS and CHANGES 
 *  - 04/01/2003   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_SRCIRES_H_VERSION

#define TF_SRCIRES_H_VERSION \
  "TF_SRCIRES_H   V1.0   "
#define TF_SRCIRES_H_CVSID \
  "$Id$"

#include<complex>
#include<iostream>
#include<aff/array.h>
#include<aff/shaper.h>

namespace gremlin1 {

  class GremlinIres {
    public:
      typedef std::complex<float> Tcvalue;
      typedef aff::Array<Tcvalue> Tcarray;
      GremlinIres(): Mdf(0.), Mires(1) { Mires=Tcvalue(0.); }
      GremlinIres(const double& df, const int& n1, const int& n2):
        Mdf(df), Mires(aff::Shaper(n1,n2)) { Mires=Tcvalue(0.); }
      GremlinIres(const double& df, const double& f1, const double& f2):
        Mdf(df), Mires(aff::Shaper(findex(f1),findex(f2))) 
        { Mires=Tcvalue(0.); }
      int nf() const { return(Mires.size(0)); }
      int n1() const { return(Mires.f(0)); }
      int n2() const { return(Mires.l(0)); }
      const double& df() const { return(Mdf); }
      double f(const int& i) const { return((i-1)*Mdf); }
      int findex(const double& fr) const { return(static_cast<int>(fr/Mdf+1)); }

      Tcarray ires() { return(Mires); }
      Tcarray::Tcoc ires() const { return(Mires); }
    private:
      // frequency step
      double Mdf;
      // coefficients
      Tcarray Mires;
  }; // GremlinIres

  std::istream& operator>>(std::istream& is, GremlinIres& green);
  std::ostream& operator<<(std::ostream& os, const GremlinIres& green);

}

#endif // TF_SRCIRES_H_VERSION (includeguard)

/* ----- END OF srcires.h ----- */
