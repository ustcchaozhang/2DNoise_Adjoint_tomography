/*! \file wnintegration.cc
 * \brief Fourier coefficients obtained by wavenumber integration (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/04/2013
 * 
 * Fourier coefficients obtained by wavenumber integration (implementation)
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
 * 
 * REVISIONS and CHANGES 
 *  - 19/04/2013   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_WNINTEGRATION_CC_VERSION \
  "TF_WNINTEGRATION_CC   V1.0"

#include "wnintegration.h"

double ExpCoefficients::maxp() const
{
  double retval=1.1/Mm.Vs;
  return(retval);
} // double ExpCoefficients::maxp()

/*----------------------------------------------------------------------*/

TFourier::Tcoeff FSECZ::coeff(const double& p) const
{
  TFourier::Tcoeff retval;
  TFourier::Tcoeff pq=p*p;
  TFourier::Tcoeff aq=(1./Malphaq)-pq;
  TFourier::Tcoeff bq=(1./Mbetaq)-pq;
  retval=std::sqrt(aq)+pq/std::sqrt(bq);
  return(retval);
} // TFourier::Tcoeff FSECZ::coeff(const double& k) const

/*----------------------------------------------------------------------*/

TFourier::Tcoeff HSEC::rayleigh(const double& p) const
{
  TFourier::Tcoeff retval;
  Mpq=p*p;
  Maq=(1./Malphaq)-Mpq;
  Mbq=(1./Mbetaq)-Mpq;
  Ma=std::sqrt(Maq);
  Mb=std::sqrt(Mbq);
  retval=(Mpq-Mbq)*(Mpq-Mbq)+4.*Mpq*Ma*Mb;
  return(retval);
} // TFourier::Tcoeff HSEC::rayleigh(const double& k) const

/*----------------------------------------------------------------------*/

TFourier::Tcoeff HSECZ::coeff(const double& p) const
{
  TFourier::Tcoeff retval;
  retval=1./this->rayleigh(p);
  retval *= Ma;
  return(retval);
} // TFourier::Tcoeff HSECZ::coeff(const double& k) const

/*----------------------------------------------------------------------*/

TFourier::Tcoeff HSECR::coeff(const double& p) const
{
  TFourier::Tcoeff retval;
  retval=1./this->rayleigh(p);
  retval *= p*(Mpq-Mbq+2.*Ma*Mb);
  return(retval);
} // TFourier::Tcoeff HSECR::coeff(const double& k) const

/*======================================================================*/
// destructors are placed in binary object
ExpCoefficients::~ExpCoefficients() { }
FSECZ::~FSECZ() { }
HSEC::~HSEC() { }
HSECZ::~HSECZ() { }
HSECR::~HSECR() { }
/*======================================================================*/

/*! create set of expansion coefficients
 *
 * expansion coefficients do not depend on frequency; they only depend on
 * phase slowness; hence they can be prepared at the very beginning
 *
 * This ist done here and the coefficients are stored in a series container
 * along with the slowness stepsize. We already apply the taper to the
 * coefficients as well as the slowness interval
 */
Exco::Exco(const ExpCoefficients& ec, const IntegParam& par):
  TFourier::Tspectrum(0,par.nsteps-1), Mdp(ec.maxp()*par.edge/par.nsteps)
{
  int ltap=par.nsteps*(1.-par.taper);
  for (int i=0; i<par.nsteps; ++i)
  {
    double p=Mdp*i;
    this->operator()(i)=ec.coeff(p)*Mdp;
    if (i>ltap)
    {
      double arg=M_PI*(i-ltap)/double(par.nsteps);
      this->operator()(i) *= 0.5*(std::cos(arg)+1.);
    }
  }
  this->operator()(0) *= 0.5;
} // Exco::Exco(const ExpCoefficients& ec, const IntegParam& par)

/*======================================================================*/

TFourier::Tcoeff wnintegration(const Exco& ec,
                               const double& f,
                               const double& offset,
                               const Ebasis& fb)
{
  TFourier::Tcoeff retval=0.;
  double dpxw=M_PI*2.*f*offset*ec.dp();
  if (4.*dpxw > M_PI)
  {
    cout << "          frequency: " << f << " Hz\n"
      <<    "             offset: " << offset << " m\n"
      <<    "  slowness stepsize: " << ec.dp() << " s/m\n"
      <<    "  argument stepsize: " << dpxw << std::endl;
  }
  TFXX_assert((4.*dpxw <= M_PI),
              "argument increase in each trapezoid step is too large; "
              "increase number of steps");
  switch(fb) 
  {
    case Fbessel0:
      for (unsigned int i=0; i< ec.size(); ++i)
      { retval += ec(i)*ec.dp()*double(i)*gsl_sf_bessel_J0(i*dpxw); }
      break;
    case Fbessel1:
      for (unsigned int i=0; i< ec.size(); ++i)
      { retval += ec(i)*double(i)*ec.dp()*gsl_sf_bessel_J1(i*dpxw); }
      break;
    case Fsin:
      for (unsigned int i=0; i< ec.size(); ++i)
      { retval += ec(i)*sin(i*dpxw); }
      break;
    case Fcos:
      for (unsigned int i=0; i< ec.size(); ++i)
      { retval += ec(i)*cos(i*dpxw); }
      break;
  }
  return(retval);
} // TFourier::Tcoeff wnintegration()

/* ----- END OF wnintegration.cc ----- */
