/*! \file fcsingleforce.cc
 * \brief Fourier coefficients for a single force full space wavefield (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * Fourier coefficients for a single force full space wavefield (implementation)
 *
 * NOTICE:
 * This fragment is not yet complete. Part of is might produce false results.
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
 * REVISIONS and CHANGES 
 *  - 17/04/2013   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_FCSINGLEFORCE_CC_VERSION \
  "TF_FCSINGLEFORCE_CC   V1.0   "

#include "lisousi.h"
#include "functions.h"

/*! Fourier coefficients of the wave field of a vertical single point source
 * in homogeneous full space
 */
TFourier::Tspectrum zfpointfc(const int& n, const double& dt, 
                              const double& offset, 
                              const double& vs,
                              const double& vp,
                              const bool& debug)
{
  TFourier::Tseries gPN(0,n-1);
  const double tp=offset/vp;
  const double ts=offset/vs;

  TFXX_debug(debug, "zfpointfc", "go..."
             << " " << TFXX_value(n)
             << " " << TFXX_value(vp)
             << " " << TFXX_value(vs)
             << " " << TFXX_value(offset)
             << " " << TFXX_value(dt)
             << " " << TFXX_value(tp)
             << " " << TFXX_value(ts)
             );

  // prepare near-field response
  for (int i=0; i<n; ++i)
  {
    double t=i*dt;
    if ((tp <= t) && (t >= ts))
    {
      gPN(i)=-t/(offset*offset*offset);
    }
    else
    {
      gPN(i)=0.;
    }
  }
  TFXX_debug(debug, "zfpointfc", "Fourier Transformation");
  TFourier::Tspectrum FCgPN=Fourier(gPN, dt);

  TFourier::Tspectrum retval(FCgPN.shape());

  TFXX_debug(debug, "zfpointfc", "total field " 
             << TFXX_value(retval.l()) << " "
             << TFXX_value(FCgPN.l()));
  const double df=1./(n*dt);
  // prepare total response
  for (int i=FCgPN.f(); i<=FCgPN.l(); ++i)
  {
    double f=(i-FCgPN.f())*df;
    // far-field
    retval(i) = exp(-IME*2.*M_PI*f*ts)/(offset*vs*vs);
    TFXX_debug(debug, "zfpointfc",
               TFXX_value(i) << " " <<
               TFXX_value(f) << " " <<
               TFXX_value(abs(retval(i))/abs(FCgPN(i))));
    // near-field
    retval(i) += FCgPN(i);
  }

  TFXX_debug(debug, "zfpointfc", "finished");
  return(retval);
}

/*----------------------------------------------------------------------*/

/*! Helper function to save sign problems in sqrt
 * function must not be called where sqrt argument would be negative
 */
inline
double sqrtfct(const double& psample, const double& v)
{
  double retval;
  double arg=(v*v*psample*psample)-1.;
  TFXX_assert(arg>=0., "negative argument of sqrt: "
                       "programming error - check source code!");
  retval=sqrt(arg)/v;
  return (retval);
}

/*----------------------------------------------------------------------*/

/*! Fourier coefficients of the wave field of a vertical single line source
 * in homogeneous full space
 */
TFourier::Tspectrum zflinefc(const int& n, const double& dt, 
                             const double& offset, 
                             const double& vs,
                             const double& vp,
                             const bool& debug)
{
  TFourier::Tseries gLN(0,n-1);
  const double tp=offset/vp;
  const double ts=offset/vs;

  // prepare near-field response
  for (int i=0; i<n; ++i)
  {
    double t=i*dt;
    if (t >= tp)
    {
      gLN(i) = sqrtfct(t/offset,vp);
      if (t>=ts)
      {
        gLN(i) -= sqrtfct(t/offset,vs);
      }
      gLN(i) *= (-2./offset);
    }
    else
    {
      gLN(i)=0.;
    }
  }
  TFourier::Tspectrum FCgLN=Fourier(gLN, dt);

  TFourier::Tspectrum retval(FCgLN.shape());

  const double df=1./(n*dt);
  // prepare total response
  for (int i=FCgLN.f(); i<=FCgLN.l(); ++i)
  {
    double f=(i-FCgLN.f())*df;
    // far-field
    retval(i) = -IME*M_PI*hankel(ts*2.*M_PI*f)/(vs*vs);
    TFXX_debug(debug, "zflinefc",
               TFXX_value(i) << " " <<
               TFXX_value(f) << " " <<
               TFXX_value(abs(retval(i))/abs(FCgLN(i))));
    // near-field
    retval(i) += FCgLN(i);
  }
  return(retval);
}

/* ----- END OF fcsingleforce.cc ----- */
