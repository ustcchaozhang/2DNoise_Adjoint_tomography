/*! \file wnintegration.h
 * \brief Fourier coefficients obtained by wavenumber integration (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/04/2013
 * 
 * Fourier coefficients obtained by wavenumber integration (prototypes)
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
 *  - 19/04/2013   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_WNINTEGRATION_H_VERSION

#define TF_WNINTEGRATION_H_VERSION \
  "TF_WNINTEGRATION_H   V1.0"

#include "lisousi.h"

/*! Model parameters
 */
struct Model {
  //! P-wave propagation velocity
  double Vp;
  //! S-wave propagation velocity
  double Vs;
  //! P-wave quality
  double Qp;
  //! S-wave quality
  double Qs;
}; // class Model

/*----------------------------------------------------------------------*/

/*! abstract base class for expansion coefficients
 */
class ExpCoefficients {
  protected:
    ExpCoefficients(const Model& m): 
      Mm(m) 
  { 
    // square of complex velocities according to recipe I
    // Forbriger and Friederich (2000) eq. (7)
    // for the calculations to be carried out, we will almost exclusively
    // require the squares of the complex frequencies
    Malphaq=Mm.Vp*Mm.Vp*(1.+IME/Mm.Qp);
    Mbetaq=Mm.Vs*Mm.Vs*(1.+IME/Mm.Qs);
  }
  public:
    virtual ~ExpCoefficients();
    //! return desired minimum upper limit for integration
    double maxp() const;
    //! return expansion coefficient for given phase slowness
    virtual TFourier::Tcoeff coeff(const double& p) const=0;
  protected:
    //! parameters for propagation model
    Model Mm;
    //! square of complex P-wave velocity
    TFourier::Tcoeff Malphaq;
    //! square of complex S-wave velocity
    TFourier::Tcoeff Mbetaq;
}; // class ExpCoefficients

/*----------------------------------------------------------------------*/

/*! Full space vertical component expansion coefficients
 * 
 * force and receiver both in z-direction and at z=0 in homogeneous full space
 */
class FSECZ: public ExpCoefficients {
  public:
    FSECZ(const Model& m): ExpCoefficients(m) { }
    virtual ~FSECZ();
    //! return expansion coefficient for given phase slowness
    virtual TFourier::Tcoeff coeff(const double& p) const;
}; // class FSECZ

/*----------------------------------------------------------------------*/

/*! Half-space base class
 * 
 * force and receiver both in z-direction and at z=0 on
 * the surface of homogeneous half space
 */
class HSEC: public ExpCoefficients {
  public:
    HSEC(const Model& m): ExpCoefficients(m) { }
    virtual ~HSEC();
    //! return expansion coefficient for given phase slowness
    virtual TFourier::Tcoeff coeff(const double& p) const=0;
  protected:
    //! return value of Rayleigh determinant
    TFourier::Tcoeff rayleigh(const double& p) const;
    //! derived values
    mutable TFourier::Tcoeff Ma, Mb, Maq, Mbq, Mpq;
}; // class HSEC

/*----------------------------------------------------------------------*/

/*! Half-space vertical component expansion coefficients
 * 
 * force and receiver both in z-direction and at z=0 on
 * the surface of homogeneous half space
 */
class HSECZ: public HSEC {
  public:
    HSECZ(const Model& m): HSEC(m) { }
    virtual ~HSECZ();
    //! return expansion coefficient for given phase slowness
    virtual TFourier::Tcoeff coeff(const double& p) const;
}; // class HSECZ

/*----------------------------------------------------------------------*/

/*! Half-space radial component expansion coefficients
 * 
 * force in z-direction and receiver in r-direction and both at z=0 on
 * the surface of homogeneous half space
 */
class HSECR: public HSEC {
  public:
    HSECR(const Model& m): HSEC(m) { }
    virtual ~HSECR();
    //! return expansion coefficient for given phase slowness
    virtual TFourier::Tcoeff coeff(const double& p) const;
}; // class HSECR

/*----------------------------------------------------------------------*/

/*! Series of expansion coefficients
 */
class Exco: public TFourier::Tspectrum {
  public:
    Exco(): Mdp(0.) { }
    Exco(const ExpCoefficients& ec, const IntegParam& par);
    //! return slowness interval
    double dp() const { return(Mdp); }
  private:
    //! slowness stepsize
    double Mdp;
}; // class Exco

/*----------------------------------------------------------------------*/

/*! Types of basis functions provided
 */
enum Ebasis {
  //! Bessel function of order 0
  Fbessel0,
  //! Bessel function of order 1
  Fbessel1,
  //! cosine function
  Fcos,
  //! sine function
  Fsin 
}; // enum Ebasis

/*----------------------------------------------------------------------*/

/*! Wavenumber integration function
 */
TFourier::Tcoeff wnintegration(const Exco& ec,
                               const double& f,
                               const double& offset,
                               const Ebasis& fb);

#endif // TF_WNINTEGRATION_H_VERSION (includeguard)

/* ----- END OF wnintegration.h ----- */
