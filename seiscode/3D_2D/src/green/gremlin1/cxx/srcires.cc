/*! \file srcires.cc
 * \brief source wavelet impulse response (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 04/01/2003
 * 
 * source wavelet impulse response (implementation)
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
 *  - 12/10/2011   V1.1   different header file required
 * 
 * ============================================================================
 */
#define TF_SRCIRES_CC_VERSION \
  "TF_SRCIRES_CC   V1.1"
#define TF_SRCIRES_CC_CVSID \
  "$Id$"

#include <cmath>
#include <cstdio>
#include <cstdarg>
#include <gremlin1/srcires.h>
#include <tfxx/error.h>
#include <aff/array.h>
#include <aff/subarray.h>

namespace gremlin1 {

  std::istream& operator>>(std::istream& is, GremlinIres& ires)
  {
    const double pi2=2.*3.141592653589793115997;
    const int maxskip=500;
    is.ignore(maxskip,'\n');
    is.ignore(maxskip,'\n');
    is.ignore(maxskip,'\n');
    is.ignore(maxskip,'\n');
    is.ignore(maxskip,'\n');
    double df;
    is >> df;
    df /= pi2;
    is.ignore(maxskip,'\n');
    int nf;
    is >> nf;
    is.ignore(maxskip,'\n');

    GremlinIres::Tcarray inarray(nf);
    aff::Array<double> inomega(nf);

    for (int i=inarray.f(0); i<=inarray.l(0); ++i)
    {
      double omega, realpart, imagpart;
      is >> omega >> realpart >> imagpart;
      inomega(i)=omega;
      inarray(i)=GremlinIres::Tcvalue(realpart,imagpart);
    }

    int n1=static_cast<int>(inomega(1)/(df*pi2))+1;
    int n2=n1+nf-1;

    GremlinIres inires(df,n1,n2);
    GremlinIres::Tcarray iresarray=inires.ires();
    iresarray.copyin(inarray);

    TFXX_assert((std::abs(1.-pi2*inires.f(n1)/inomega(1))<1.e-4),
                "ERROR: frequency range mismatch in gremlin ires");

    ires=inires;
    return(is);
  }

  /*----------------------------------------------------------------------*/

  std::ostream& operator<<(std::ostream& os, const GremlinIres& ires)
  {
    const double pi2=2.*3.141592653589793115997;
    // get copy including frequency zero
    GremlinIres::Tcarray outires(ires.n2());
    GremlinIres::Tcarray
      subires(aff::subarray(outires)(ires.n1(),ires.n2()));

    subires.copyin(ires.ires());
    os << "C++ version of gremlin ires" << std::endl;
    os << "columns below row number 7:" << std::endl;
    os << "  1: angular frequency (1/s)" << std::endl;
    os << "  2: real part" << std::endl;
    os << "  3: imaginary part" << std::endl;
    os << pi2*ires.df() << " angular frequency step width" << std::endl;
    os << outires.size() << " number of frequency samples" << std::endl;

    const int bufsize=60;
    char buffer[bufsize];

    for (int i=outires.f(0); i<=outires.l(0); ++i) 
    {
      double angfreq=static_cast<double>(pi2*ires.f(i));
      double realpart=static_cast<double>(real(outires(i)));
      double imagpart=static_cast<double>(imag(outires(i)));
      std::snprintf(buffer, bufsize, "%15.5f %15.7e %15.7e", 
                    angfreq, realpart, imagpart);
      os << buffer << std::endl;
    }

    return(os);
  }

}

/* ----- END OF srcires.cc ----- */
