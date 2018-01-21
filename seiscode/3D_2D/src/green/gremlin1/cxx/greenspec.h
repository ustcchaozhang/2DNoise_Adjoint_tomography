/*! \file greenspec.h
 * \brief class handling greens spectra (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 04/01/2003
 * 
 * class handling greens spectra (prototypes)
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
 *  - 09/02/2010   V1.1   moved initialization of magic code
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_GREENSPEC_H_VERSION

#define TF_GREENSPEC_H_VERSION \
  "TF_GREENSPEC_H   V1.1"
#define TF_GREENSPEC_H_CVSID \
  "$Id$"

#include<aff/array.h>
#include<iostream>
#include<complex>

namespace gremlin1 {

  class GreenSpectrum {
    public:
      typedef std::complex<float> Tcvalue;
      typedef aff::Array<Tcvalue> Tcarray;
      static const char Mcmagic[];
      GreenSpectrum(): Mdp(0.), Mpmin(0.), Mdf(0.), Mfmin(0.), Mgreen(1,1) 
      { Mgreen=Tcvalue(0.); } 
      GreenSpectrum(const double& fmin, const double& fmax, const int& nf,
                    const double& pmin, const double& pmax, const int& np):
        Mdp((pmax-pmin)/(np-1)), Mpmin(pmin),
        Mdf((fmax-fmin)/(nf-1)), Mfmin(fmin),
        Mgreen(np,nf) { Mgreen=Tcvalue(0.); }
      int nf() const { return(Mgreen.size(1)); }
      int np() const { return(Mgreen.size(0)); }
      const double& fmin() const { return(Mfmin); }
      double fmax() const { return(f(nf())); }
      const double& pmin() const { return(Mpmin); }
      double pmax() const { return(p(np())); }
      const double& df() const { return(Mdf); }
      const double& dp() const { return(Mdp); }
      double f(const int& i) const { return((i-1)*Mdf+Mfmin); }
      double p(const int& i) const { return((i-1)*Mdp+Mpmin); }

      Tcarray green() { return(Mgreen); }
      Tcarray::Tcoc green() const { return(Mgreen); }
    private:
      // slowness step
      double Mdp;
      // minimum slowness
      double Mpmin;
      // frequency step
      double Mdf;
      // minimum frequency
      double Mfmin;
      // coefficients
      Tcarray Mgreen;
  }; // GreenSpectrum

  std::istream& operator>>(std::istream& is, GreenSpectrum& green);
  std::ostream& operator<<(std::ostream& os, const GreenSpectrum& green);

}

#endif // TF_GREENSPEC_H_VERSION (includeguard)

/* ----- END OF greenspec.h ----- */
