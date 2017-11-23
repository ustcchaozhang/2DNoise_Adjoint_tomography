/*! \file fftwaff.cc
 * \brief use fftw together with aff containers (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/07/2006
 * 
 * use fftw together with aff containers (implementation)
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 11/07/2006   V1.0   Thomas Forbriger
 *  - 12/09/2007   V1.1   first running version
 *  - 07/10/2010   V1.2   
 *                        - migrate to FFTW3
 *  - 13/05/2011   V1.3 
 *                        - properly delete plans in delete_arrays() function
 *  - 02/10/2012   V1.4  
 *                        - use size calculation functions
 *
 * ============================================================================
 */
#define TF_FFTWAFF_CC_VERSION \
  "TF_FFTWAFF_CC   V1.4"

#include <iostream>
#include <fourier/fftwaff.h>
#include <fourier/error.h>
#include <fourier/error.h>
#include <aff/seriesoperators.h>
#include <aff/dump.h>
#include <cmath>

namespace fourier {

  /*! All Fourier transform stuff is collected here.
   */
  namespace fft {

    //! create plan.
    void DRFFTWAFF::create_plan_forward() const
    {
      if (Mplan_forward==0)
      {
        //Mplan_forward=rfftw_create_plan(Msize, FFTW_FORWARD, 0);
#ifdef FFTWFALLBACK
        Mplan_forward=rfftw_create_plan(Msize, FFTW_REAL_TO_COMPLEX,
                                        FFTW_ESTIMATE);
#else
        this->create_arrays();
        Mplan_forward=fftw_plan_dft_r2c_1d(Msize, Mseriesarray,
                                           Mspectrumarray,
                                           FFTW_ESTIMATE);
#endif
        FOURIER_assert(Mplan_forward!=0, 
                    "Error (DRFFTWAFF::create_plan_forward): "
                    "could not create plan!")
      }
    } // void DRFFTWAFF::create_plan_forward() const

    /*----------------------------------------------------------------------*/

    //! create plan.
    void DRFFTWAFF::create_plan_backward() const
    {
      if (Mplan_backward==0)
      {
#ifdef FFTWFALLBACK
        Mplan_backward=rfftw_create_plan(Msize, FFTW_BACKWARD, 0);
#else
        this->create_arrays();
        Mplan_backward=fftw_plan_dft_c2r_1d(Msize, Mspectrumarray,
                                           Mseriesarray,
                                           FFTW_ESTIMATE);
#endif
        FOURIER_assert(Mplan_backward!=0, 
                    "Error (DRFFTWAFF::create_plan_backward): "
                    "could not create plan!")
      }
    } // void DRFFTWAFF::create_plan_backward() const

    /*----------------------------------------------------------------------*/

    //! delete plan.
    DRFFTWAFF::~DRFFTWAFF()
    {
      this->delete_plans();
#ifndef FFTWFALLBACK
      fftw_cleanup();
#endif
    } // DRFFTWAFF::~DRFFTWAFF()

    /*----------------------------------------------------------------------*/

    //! prepare FFT settings for size n.
    void DRFFTWAFF::set_size(const unsigned int& n) const
    {
      if (n != Msize) 
      {
        Msize=n;
        this->delete_plans();
#ifndef FFTWFALLBACK
        this->delete_arrays();
#endif
      }
    } // DRFFTWAFF::~DRFFTWAFF()

    /*----------------------------------------------------------------------*/

    //! delete plans.
    void DRFFTWAFF::delete_plans() const
    {
#ifdef FFTWFALLBACK
      if (Mplan_forward != 0) { rfftw_destroy_plan(Mplan_forward); }
      if (Mplan_backward != 0) { rfftw_destroy_plan(Mplan_backward); }
#else
      if (Mplan_forward != 0) { fftw_destroy_plan(Mplan_forward); }
      if (Mplan_backward != 0) { fftw_destroy_plan(Mplan_backward); }
#endif
      Mplan_forward=0;
      Mplan_backward=0;
    } // DRFFTWAFF::delete_plans()

    /*----------------------------------------------------------------------*/

#ifndef FFTWFALLBACK
    //! create plans.
    void DRFFTWAFF::create_arrays() const
    {
      this->delete_arrays();
      Mseriesarray = (double *) fftw_malloc(sizeof(double)*Msize);
      FOURIER_assert(Mseriesarray!=0, 
                  "Error (DRFFTWAFF::create_plan_forward): "
                  "could not create series array!")
      Mseries=Tseries(Tseries::Trepresentation(Mseriesarray, Msize));
      Mspectrumarray = (fftw_complex *)
        fftw_malloc(sizeof(fftw_complex)*this->ssize());
      FOURIER_assert(Mspectrumarray!=0, 
                  "Error (DRFFTWAFF::create_plan_forward): "
                  "could not create spectrum array!")
      Mspectrum=Tspectrum(Tspectrum::Trepresentation(
                                  reinterpret_cast<Tcoeff*>(Mspectrumarray),
                                                     this->ssize()));
    } // DRFFTWAFF::create_arrays()

    /*----------------------------------------------------------------------*/

    //! delete arrays.
    void DRFFTWAFF::delete_arrays() const
    {
      this->delete_plans();
      if (Mseriesarray != 0) { fftw_free(Mseriesarray); }
      if (Mspectrumarray != 0) { fftw_free(Mspectrumarray); }
      Mseries=Tseries(0);
      Mspectrum=Tspectrum(0);
      // Mplan_forward=0;
      // Mplan_backward=0;
    } // DRFFTWAFF::delete_arrays()
#endif

    /*----------------------------------------------------------------------*/

    /*! \brief Transform time series to Fourier coefficients.
     *
     * No scaling is applied.
     */
    DRFFTWAFF::Tspectrum DRFFTWAFF::operator()(const Tseries::Tcoc& s,
                                               const bool& debug) const
    {
      this->set_size(s.size());
#ifdef FFTWFALLBACK
      FOURIER_debug(debug, "DRFFTWAFF::operator()",
                 "use fallback code (FFTW2)");
      Tspectrum retval(this->Msize/2+1);
      aff::Series<fftw_real> out(this->Msize);
      aff::Series<fftw_real> in(this->Msize);
      fftw_real* pout=out.pointer();
      fftw_real* pin=in.pointer();
      FOURIER_debug(debug, "DRFFTWAFF::operator()",
                 "processing arrays are created; copy in series");
      in.copyin(s);
      
      FOURIER_debug(debug, "DRFFTWAFF::operator()",
                 "create plan forward");
      this->create_plan_forward();
      FOURIER_debug(debug, "DRFFTWAFF::operator()",
                 "call rfftw_one");
      rfftw_one(Mplan_forward, pin, pout);
      FOURIER_debug(debug, "DRFFTWAFF::operator()",
                 "copy results to output");
      retval(0)=out(0);
      for (int i=1; i<((Msize+1)/2); ++i)
      {
        retval(i)=Tcoeff(out(i),out(Msize-i));
      }
      if ((Msize % 2) == 0)
      {
        retval(Msize/2)=out(Msize/2);
      }
      /*
      if (debug)
      {
        DUMP(in);
        DUMP(out);
      }
      */
#else
      FOURIER_debug(debug, "DRFFTWAFF::operator()",
                 "use recent code (FFTW3)");
      Tspectrum retval(this->ssize());
      this->create_plan_forward();
      Mseries.copyin(s);
      /*
      for (int i=0; i<this->size(); ++i)
      { Mseries(Mseries.first()+i)=s(s.first()+i); }
      */
      fftw_execute(Mplan_forward);
      retval.copyin(Mspectrum);
      /*
      for (int i=0; <this->ssize(); ++i)
      { retval(retval.first()+i)=Mspectrum(Mspectrum.first()+i); }
      */
#endif
      /*
      if (debug)
      {
        DUMP(s);
        DUMP(retval);
      }
      */
      FOURIER_debug(debug, "DRFFTWAFF::operator()",
                 "finished; return");
      return(retval);
    } // Tspectrum DRFFTWAFF::operator()(const Tseries::Tcoc& s) const

    /*----------------------------------------------------------------------*/

    /*! \brief Transform Fourier coefficients to time series.
     *
     * No scaling is applied.
     */
    DRFFTWAFF::Tseries DRFFTWAFF::operator()(const Tspectrum::Tcoc& s,
                                             const bool& debug) const
    {
      // check number of expected Fourier coefficients
      if (this->ssize() != s.size())
      {
        // adjust FFT size
        int seriessize=DRFFTWAFF::seriessize(s.size());
        // is Nyquits coefficients real?
        // this measure can only be effective if the Nyquist coefficient is
        // finite, which will not be the case for most signals
        if (std::abs(s(s.size()).imag()) < 1.e-8*std::abs(s(s.size()).real()))
        { --seriessize; }
        this->set_size(seriessize);
      }

#ifdef FFTWFALLBACK
      Tseries retval(Msize);
      aff::Series<fftw_real> out(Msize);
      aff::Series<fftw_real> in(Msize);
      fftw_real* pout=out.pointer();
      fftw_real* pin=in.pointer();
      in(0)=s(0).real();
      for (int i=1; i<((Msize+1)/2); ++i)
      {
        in(i)=s(i).real();
        in(Msize-i)=s(i).imag();
      }
      if ((Msize % 2) == 0)
      {
        in(Msize/2)=s(Msize/2).real();
      }
      this->create_plan_backward();
      rfftw_one(Mplan_backward, pin, pout);
      retval.copyin(out);
#else
      Tseries retval(this->size());
      this->create_plan_backward();
      Mspectrum.copyin(s);
      fftw_execute(Mplan_backward);
      retval.copyin(Mseries);
#endif
      return(retval);
    } // Tseries DRFFTWAFF::operator()(const Tspectrum::Tcoc& s) const

    /*----------------------------------------------------------------------*/
    
    /*! \brief Return appropriate scaling factor for sampling interval dt.
     *
     * Factor to be applied when transforming to time domain.
     *
     * \param[in] dt sampling interval
     * \return scalar factor to be applied to all samples
     *
     * \sa \ref sec_fftw3_integral_transform
     */
    DRFFTWAFF::Tsample DRFFTWAFF::scale_series(const Tsample& dt) const
    {
      return(1./(Msize*dt));
    } // Tsample DRFFTWAFF::scale_series(const Tsample& dt) const

    /*----------------------------------------------------------------------*/
    
    /*! \brief Return appropriate scaling factor for sampling interval dt.
     *
     * Factor to be applied when transforming to Fourier domain.
     *
     * \param[in] dt sampling interval
     * \return scalar factor to be applied to all samples
     *
     * \sa \ref sec_fftw3_integral_transform
     */
    DRFFTWAFF::Tsample DRFFTWAFF::scale_spectrum(const Tsample& dt) const
    {
      return(dt);
    } // Tsample DRFFTWAFF::scale_spectrum(const Tsample& dt) const

    /*----------------------------------------------------------------------*/

    /*! \brief Transform time series to Fourier coefficients and scale.
     *
     * Appropriate scaling is applied for sampling interval dt.
     */
    DRFFTWAFF::Tspectrum DRFFTWAFF::operator()(const Tseries::Tcoc& s,
                            const double& dt,
                            const bool& debug) const
    {
      Tspectrum retval=this->operator()(s, debug);
      retval *= this->scale_spectrum(dt);
      return(retval);
    }

    /*----------------------------------------------------------------------*/

    /*! \brief Transform Fourier coefficients to time series and scale.
     *
     * Appropriate scaling is applied for sampling interval dt.
     */
    DRFFTWAFF::Tseries DRFFTWAFF::operator()(const Tspectrum::Tcoc& s,
                            const double& dt,
                            const bool& debug) const
    {
      return(this->scale_series(dt)*
             this->operator()(s, debug));
    }

  } // namespace ftt

} // namespace fourier

/* ----- END OF fftwaff.cc ----- */
