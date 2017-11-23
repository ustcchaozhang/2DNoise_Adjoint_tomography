/*! \file lisousi.h
 * \brief prototypes and structs for lisousi (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/04/2013
 * 
 * prototypes and structs for lisousi (prototypes)
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

// include guard
#ifndef TF_LISOUSI_H_VERSION

#define TF_LISOUSI_H_VERSION \
  "TF_LISOUSI_H   V1.0"

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include <gsl/gsl_sf_bessel.h>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/error.h>
#include <tfxx/stringfunc.h>
#include <tfxx/rangestring.h>
#include <tfxx/rangelist.h>
#include <tfxx/misc.h>
#include <tsxx/anyfilter.h>
#include <tsxx/convolve.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <sffxx.h>
#include <sffostream.h>
#include <aff/dump.h>
#include <aff/seriesoperators.h>
#include <aff/subarray.h>
#include <tsxx/ovtaper.h>
#include <fourier/fftwaff.h>

using std::cout;
using std::cerr;
using std::endl;

typedef ts::filter::Ttimeseries Ttimeseries;
typedef Ttimeseries::Tseries Tseries;
typedef fourier::fft::DRFFTWAFF TFourier;

// the global Fourier processor will be instantiated in the main programmin
// unit
extern TFourier Fourier;

// global workspace and filters
extern Tseries filter, taper, workseries;

/*----------------------------------------------------------------------*/
// constants
  
// imaginary unit
const TFourier::Tcoeff IME=TFourier::Tcoeff(0.,1.);
// my Fourier transformation constant factor
const TFourier::Tcoeff CFTfac=exp(-IME*M_PI_4);

/*----------------------------------------------------------------------*/
// type of Fourier domain solution
enum Efdtype {
  Ffdfarfield,
  Ffdexplosion,
  Ffdzforce,
  Ffdwizforce,
  Ffdlamb
};

/*----------------------------------------------------------------------*/

/*! Parameters for wavenumber integration
 */
struct IntegParam {
  //! number of steps
  int nsteps;
  //! edge factor at upper limit of wavenumber range
  double edge;
  //! taper fraction at upper limit of wavenumber range
  double taper;
}; // struct IntegParam

/*----------------------------------------------------------------------*/

/*! program parameters as passed on command line
 */
struct Options {
  bool verbose, debug;
  bool overwrite, taperfirst, sqrttaper, limitlength, radial;
  bool fredomain, fdfilter, tdfilter, nointeg, transition, tapsloset;
  bool spatialdistance;
  double tfac, tshift, tlim, integshift, tapdel;
  double tapslo, transition1, transition2;
  int npad;
  std::string inputformat, outputformat;
  // single-velocity switches:
  Efdtype fdtype;
  // propagation parameters:
  double velocity, vpvsratio, pquality, squality;
  // trapezoid integration:
  IntegParam ipa;
}; // struct Options

/*! \brief time series parameters.
 *
 * This struct is used to store parameters for each time series under
 * processing.
 */
struct Parameters {
  Parameters():
    offsetfactor(0.), offset(0.), t0(0.), T(0.), dt(0.) { }
  /*! \brief scaling factor to be applied at given offset.
   *
   * factor is set according to selected transformation approach
   */
  double offsetfactor;
  /*! \brief either epicentral distance or hypocentral distance.
   *
   *  selectable by command line option -spatialdistance
   */
  double offset;
  /*! \brief delay time.
   *
   * time difference between time of first sample and source time
   */
  double t0;
  /*! \brief duration of total recording.
   *
   * number of samples times sampling interval
   */
  double T;
  /*! \brief sampling interval */
  double dt;
  /*! \brief number of samples */
  int nsamples;
}; // struct Parameters

#endif // TF_LISOUSI_H_VERSION (includeguard)

/* ----- END OF lisousi.h ----- */
