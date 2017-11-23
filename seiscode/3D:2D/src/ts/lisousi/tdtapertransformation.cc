/*! \file tdtapertransformation.cc
 * \brief time domain tapering approaches to transformation (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * time domain tapering approaches to transformation (implementation)
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
 *  - 21/09/2015   V1.1   properly trim convolution result
 * 
 * ============================================================================
 */
#define TF_TDTAPERTRANSFORMATION_CC_VERSION \
  "TF_TDTAPERTRANSFORMATION_CC   V1.1"

#include "lisousi.h"
#include "functions.h"

TFourier::Tseries tdtapertransformation(TFourier::Tseries series,
                                        Parameters& par,
                                        const Options& opt)
{
  /*-------------------------------------*/
  /* set offset dependent scaling factor */

  TFXX_debug(opt.debug, "tdtapertransformation",
             "transformation with time domain taper");

  if (opt.sqrttaper)
  {
    if (opt.verbose)
    {
      cout << "  apply reflected wave approximation for wave "
        << "velocity: " << opt.velocity << " km/s" << endl;
    }
    par.offsetfactor=opt.velocity*1.e3*sqrt(2.);
  }
  else
  {
    if (opt.verbose)
    {
      cout << "  apply direct wave approximation for travel "
        << "distance: " << par.offset << " m" << endl;
    }
    par.offsetfactor=par.offset*sqrt(2.);
  }

  /*-------------------------------------------*/
  /* prepare filter impulse response and taper */

  // prepare taper
  if (taper.size() < series.size())
  {
    taper=filterresponse(series.size(), par.dt, opt);
    filter=taper;

    if (opt.sqrttaper)
    {
      if (opt.verbose)
      {
        cout << "  apply sqrt(t) taper" << endl;
      }
      filter=taper.copyout();
      // prepare alternative taper
      aff::Iterator<Tseries> I(taper);
      for (unsigned int i=0; i<taper.size(); ++i)
      {
        *I=sqrt(double(i)*par.dt);
        ++I;
      }
    }
  }

  // apply taper
  if (opt.taperfirst)
  {
    TFXX_debug(opt.debug, "tdtapertransformation", "apply taper");
    series=applytaper(series, taper, par.offsetfactor, par.dt, par.offset, opt);
  }

  /*-------------------------------------------*/
  /* apply filter impulse response and taper   */

  Tseries convresult;
  if (opt.tdfilter)
  {
    if (opt.verbose)
    {
      cout << "  apply 1/sqrt(t) filter in the time domain" << endl;
    }
    // time domain convolution
    convresult=ts::convolve(series, filter) * par.dt;
    // trim to reasonable index range
    // see comments in documentation of ts::convolve
    convresult.setlastindex(convresult.f()+series.size()-1);
  }
  else if (opt.fdfilter)
  {
    if (opt.verbose)
    {
      cout <<
        "  apply 1/sqrt(t) filter from discrete Fourier transform"
        << endl;
    }

    TFourier::Tspectrum scoeff=Fourier(series, par.dt);
    TFourier::Tspectrum fcoeff=Fourier(filter, par.dt);

    aff::Browser<TFourier::Tspectrum> IF(fcoeff);
    aff::Iterator<TFourier::Tspectrum> IS(scoeff);
    while (IF.valid() && IS.valid())
    {
      (*IS) *= (*IF);
      ++IS;
      ++IF;
    }

    convresult=Fourier(scoeff, par.dt);
  }
  else
  {
    if (opt.verbose)
    {
      cout <<
        "  apply analytic Fourier transform of 1/sqrt(t)"
        << endl;
    }
    // Fourier domain convolution
    TFourier::Tspectrum coeff=Fourier(series, par.dt);
    const TFourier::Tcoeff IME(0.,1.);
    const double df=1./par.T;
    for (unsigned int i=coeff.f(); i<=coeff.l(); ++i)
    {
      double ifre=i-coeff.f();
      double f=ifre*df;
      if (f<df) { f=0.1*df; }
      coeff(i) *= sqrt(M_PI/2.)*(1.-IME)/sqrt(2.*M_PI*f);
    }
    convresult=Fourier(coeff, par.dt);
  }

  /*---------------------------------------------*/

  TFourier::Tseries retseries;
  // apply taper
  if (!opt.taperfirst)
  {
    TFXX_debug(opt.debug, "tdtapertransformation", "apply taper");
    retseries=applytaper(convresult, taper, par.offsetfactor, par.dt,
                         par.offset, opt);
  }
  else
  {
    retseries=convresult;
  }
  return(retseries);
}

/* ----- END OF tdtapertransformation.cc ----- */
