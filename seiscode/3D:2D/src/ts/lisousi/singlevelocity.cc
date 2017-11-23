/*! \file singlevelocity.cc
 * \brief all types of single velocity transformations in the Fourier domain (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * all types of single velocity transformations in the Fourier domain (implementation)
 * 
 * Copyright (c) 2013 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * lisousi is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version. 
 * 
 * lisousi is distributed in the hope that it will be useful,
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
 *  - 17/04/2013   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_SINGLEVELOCITY_CC_VERSION \
  "TF_SINGLEVELOCITY_CC   V1.0"

#include "lisousi.h"
#include "functions.h"

TFourier::Tseries singlevelocitytransformation(const TFourier::Tseries& series,
                                               const Parameters& par,
                                               const Exco& ec,
                                               const Options& opt)
{
  TFXX_debug(opt.debug, "singlevelocitytransformation",
             "frequency domain single velocity transformation");
  if (opt.verbose)
  {
    cout << "  apply 2D/3D Greens function ratio for single "
      << "wave velocity: " << opt.velocity << " km/s" << endl;
  }

  TFXX_debug(opt.debug, "singlevelocity", "  series size: " << series.size());
  TFourier::Tspectrum coeff=Fourier(series, par.dt);
  TFXX_debug(opt.debug, "singlevelocity", "  coefficients size: "
             << coeff.size());
  // scale with frequency independent and offset dependent factor
  if (opt.fdtype==Ffdfarfield)
  {
    coeff*=(CFTfac*sqrt(par.offset));
  }

  // prepare Fourier coefficients of single force line and point source
  // Green's function in full space, if required
  TFourier::Tspectrum zfpoint, zfline;
  if (opt.fdtype==Ffdzforce)
  {
    zfpoint=zfpointfc(series.size(), par.dt, par.offset, 
                      1.e3*opt.velocity, 1.e3*opt.velocity*opt.vpvsratio,
                      opt.debug);
    zfline=zflinefc(series.size(), par.dt, par.offset, 
                    1.e3*opt.velocity, 1.e3*opt.velocity*opt.vpvsratio,
                    opt.debug);
    TFXX_assert(zfpoint.f()==coeff.f(), "shape mismatch (programming error)");
    TFXX_assert(zfline.f()==coeff.f(), "shape mismatch (programming error)");
    TFXX_assert(zfpoint.l()==coeff.l(), "shape mismatch (programming error)");
    TFXX_assert(zfline.l()==coeff.l(), "shape mismatch (programming error)");
  }

  // factor in argument of Hankel function and complex exponential
  // in case of exact solution
  double argfact=2.e-3*M_PI*par.offset/opt.velocity;

  for (unsigned int i=coeff.f(); i<=coeff.l(); ++i)
  {
    int ifre=i-coeff.f();
    if (ifre==0) { ifre = 1; }
    double f=ifre/par.T;
    if (opt.fdtype==Ffdexplosion)
    {
      // std::cout << "exact!" << std::endl;
      double argument=f*argfact;
      TFourier::Tcoeff numerator= -IME*M_PI*hankel(argument)*par.offset;
      TFourier::Tcoeff denominator=exp(-IME*argument);
      coeff(i) *= (numerator/denominator);
    }
    else if (opt.fdtype==Ffdzforce)
    {
      coeff(i) *= (zfline(i)/zfpoint(i));
    }
    else if ((opt.fdtype==Ffdwizforce) || (opt.fdtype==Ffdlamb))
    {
      /*
       * the angular frequency appears as a factor in the denominator;
       * this essentiall is an integration and produces a singularity at
       * f=0Hz; alternatively to using this factor here, we could discard it
       * and do the integration in the time domain...
       */
      TFourier::Tcoeff numerator;
      TFourier::Tcoeff denominator;
      if ((opt.fdtype==Ffdlamb) && (opt.radial))
      {
        numerator = 2.*wnintegration(ec, f, par.offset, Fsin);
        denominator = 2.*M_PI*f*wnintegration(ec, f, par.offset, Fbessel1);
      }
      else
      {
        numerator = 2.*wnintegration(ec, f, par.offset, Fcos);
        denominator = 2.*M_PI*f*wnintegration(ec, f, par.offset, Fbessel0);
      }
      coeff(i) *= (numerator/denominator);
    }
    else
    {
      coeff(i)*=sqrt(1.e3*opt.velocity/f);
    }
  }
  return(Fourier(coeff, par.dt));
}

/* ----- END OF singlevelocity.cc ----- */
