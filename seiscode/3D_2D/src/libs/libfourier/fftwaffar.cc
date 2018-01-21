/*! \file fftwaffar.cc
 * \brief engine to transfrom several signals at once (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/05/2011
 * 
 * engine to transfrom several signals at once (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 * REVISIONS and CHANGES 
 *  - 13/05/2011   V1.0   Thomas Forbriger
 *  - 27/05/2011   V1.1   added copy constructor (required for class member
 *                        initialization) and default constructor and
 *                        assignment operator
 * 
 * ============================================================================
 */
#define TF_FFTWAFFAR_CC_VERSION \
  "TF_FFTWAFFAR_CC   V1.1"

#include <iostream>
#include <fourier/fftwaffar.h>
#include <fourier/error.h>
#include <aff/Carray.h>
#include <aff/slice.h>
#include <aff/shaper.h>

namespace fourier {

  namespace fft {

    DRFFTWAFFArrayEngine::DRFFTWAFFArrayEngine(const int& nseis,
                                               const int& nsamp)
      : Mseriesarray(TAseries(aff::Shaper(0,nsamp-1)(0,nseis-1))),
      Mspectrumarray(TAspectrum(aff::Shaper(0,DRFFTWAFFArrayEngine::ncoeff(nsamp)-1)(0,nseis-1))),
      Mplanr2c(0), Mplanc2r(0) 
    {
      this->checkconsistency();
    }

    /*----------------------------------------------------------------------*/

    DRFFTWAFFArrayEngine::DRFFTWAFFArrayEngine(const TAseries& series,
                                               const TAspectrum& spec)
      : Mseriesarray(series), Mspectrumarray(spec),
      Mplanr2c(0), Mplanc2r(0) 
    { this->checkconsistency(); }

    /*----------------------------------------------------------------------*/

    DRFFTWAFFArrayEngine::DRFFTWAFFArrayEngine(const DRFFTWAFFArrayEngine& e)
      : Mseriesarray(e.Mseriesarray), Mspectrumarray(e.Mspectrumarray),
      Mplanr2c(0), Mplanc2r(0) 
    { }

    /*----------------------------------------------------------------------*/

    DRFFTWAFFArrayEngine::DRFFTWAFFArrayEngine()
      : Mseriesarray(TAseries(aff::Shaper(0,4-1)(0,0))),
      Mspectrumarray(TAspectrum(aff::Shaper(0,DRFFTWAFFArrayEngine::ncoeff(4)-1)(0,0))),
      Mplanr2c(0), Mplanc2r(0) 
    { }

    /*----------------------------------------------------------------------*/

    DRFFTWAFFArrayEngine& DRFFTWAFFArrayEngine::operator=(const DRFFTWAFFArrayEngine& e)
    {
      this->delete_plans();
      Mseriesarray=e.Mseriesarray;
      Mspectrumarray=e.Mspectrumarray;
      return (*this);
    } // DRFFTWAFFArrayEngine& DRFFTWAFFArrayEngine::operator=(const DRFFTWAFFArrayEngine& e)

    /*----------------------------------------------------------------------*/

    //! delete plan.
    DRFFTWAFFArrayEngine::~DRFFTWAFFArrayEngine()
    {
      this->delete_plans();
    } // DRFFTWAFFArrayEngine::~DRFFTWAFFArrayEngine()

    /*----------------------------------------------------------------------*/

    //! delete plans.
    void DRFFTWAFFArrayEngine::delete_plans() 
    {
      if (Mplanr2c != 0) { fftw_destroy_plan(Mplanr2c); }
      if (Mplanc2r != 0) { fftw_destroy_plan(Mplanc2r); }
      Mplanr2c=0;
      Mplanc2r=0;
    } // DRFFTWAFFArrayEngine::delete_plans()

    /*----------------------------------------------------------------------*/

    //! execute c2r plan
    void DRFFTWAFFArrayEngine::c2r() 
    {
      if (Mplanc2r == 0)
      {
        this->createplanc2r();
      }
      fftw_execute(Mplanc2r);
    } // DRFFTWAFFArrayEngine::c2r()

    /*----------------------------------------------------------------------*/

    //! execute r2c plan
    void DRFFTWAFFArrayEngine::r2c() 
    {
      if (Mplanr2c == 0)
      {
        this->createplanr2c();
      }
      fftw_execute(Mplanr2c);
    } // DRFFTWAFFArrayEngine::r2c()

    /*----------------------------------------------------------------------*/

    void DRFFTWAFFArrayEngine::checkconsistency()
    {
      FOURIER_assert(Mspectrumarray.size(0)==
                     DRFFTWAFFArrayEngine::ncoeff(Mseriesarray.size(0)),
                     "ERROR: sample size inconsistent");
      FOURIER_assert(Mseriesarray.size(1)==Mspectrumarray.size(1),
                     "ERROR: inconsistent number of signals");
      FOURIER_assert(((Mseriesarray.size(2)==1) && (Mspectrumarray.size(2)==1)),
                     "ERROR: two-dimensional arrays only");
      FOURIER_assert(((Mseriesarray.size(3)==1) && (Mspectrumarray.size(3)==1)),
                     "ERROR: two-dimensional arrays only");
    } // void DRFFTWAFFArrayEngine::checkconsistency()

    /*----------------------------------------------------------------------*/

    void DRFFTWAFFArrayEngine::createplanr2c()
    {
      if (Mplanr2c==0)
      {
        // overall parameters
        // ------------------
        // tranformation only along one dimension
        const int rank=1;
        // size of each series
        int n=Mseriesarray.size(0);
        // number of signals to be transformed
        const int howmany=Mseriesarray.size(1);
        // quick design
        const unsigned flags=FFTW_ESTIMATE;

        // input array
        // -----------
        aff::CArray<TAseries::Tvalue> Cseriesarray(Mseriesarray);
        // one-dimensional transform: use default
        int* inembed=0;
        // distance to next sample
        const int istride=Cseriesarray.stride(0);
        // distance to next signal
        const int idist=Cseriesarray.stride(1);
        // casted pointer
        double* in=Cseriesarray.castedpointer<double>();

        // output array
        // ------------
        aff::CArray<TAspectrum::Tvalue> Cspectrumarray(Mspectrumarray);
        // one-dimensional transform: use default
        int* onembed=0;
        // distance to next sample
        const int ostride=Cspectrumarray.stride(0);
        // distance to next signal
        const int odist=Cspectrumarray.stride(1);
        // casted pointer
        fftw_complex* out=Cspectrumarray.castedpointer<fftw_complex>();

        // create plan
        Mplanr2c=fftw_plan_many_dft_r2c(rank, &n, howmany,
                                        in, inembed, istride, idist,
                                        out, onembed, ostride, odist,
                                        flags);
        FOURIER_assert(Mplanr2c!=0, "ERROR: creating r2c plan");
      }
    } // void DRFFTWAFFArrayEngine::createplanr2c()

    /*----------------------------------------------------------------------*/

    void DRFFTWAFFArrayEngine::createplanc2r()
    {
      if (Mplanc2r==0)
      {
        // overall parameters
        // ------------------
        // tranformation only along one dimension
        const int rank=1;
        // size of each series
        int n=Mseriesarray.size(0);
        // number of signals to be transformed
        const int howmany=Mseriesarray.size(1);
        // quick design
        const unsigned flags=FFTW_ESTIMATE;

        // input array
        // -----------
        aff::CArray<TAspectrum::Tvalue> Cspectrumarray(Mspectrumarray);
        // one-dimensional transform: use default
        int* inembed=0;
        // distance to next sample
        const int istride=Cspectrumarray.stride(0);
        // distance to next signal
        const int idist=Cspectrumarray.stride(1);
        // casted pointer
        fftw_complex* in=Cspectrumarray.castedpointer<fftw_complex>();

        // output array
        // ------------
        aff::CArray<TAseries::Tvalue> Cseriesarray(Mseriesarray);
        // one-dimensional transform: use default
        int* onembed=0;
        // distance to next sample
        const int ostride=Cseriesarray.stride(0);
        // distance to next signal
        const int odist=Cseriesarray.stride(1);
        // casted pointer
        double* out=Cseriesarray.castedpointer<double>();

        // create plan
        Mplanc2r=fftw_plan_many_dft_c2r(rank, &n, howmany,
                                        in, inembed, istride, idist,
                                        out, onembed, ostride, odist,
                                        flags);
        FOURIER_assert(Mplanc2r!=0, "ERROR: creating c2r plan");
      }
    } // void DRFFTWAFFArrayEngine::createplanc2r()
    
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
    DRFFTWAFFArrayEngine::Tsample
      DRFFTWAFFArrayEngine::scale_series(const Tsample& dt) const
    {
      return(1./(Mseriesarray.size(0)*dt));
    } // Tsample DRFFTWAFFArrayEngine::scale_series(const Tsample& dt) const

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
    DRFFTWAFFArrayEngine::Tsample
      DRFFTWAFFArrayEngine::scale_spectrum(const Tsample& dt) const
    {
      return(dt);
    } // Tsample DRFFTWAFFArrayEngine::scale_spectrum(const Tsample& dt) const

    /*----------------------------------------------------------------------*/

    unsigned int DRFFTWAFFArrayEngine::nsamples() const
    {
      return Mseriesarray.size(0);
    } // unsigned int DRFFTWAFFArrayEngine::nsamples() const

    /*----------------------------------------------------------------------*/

    unsigned int DRFFTWAFFArrayEngine::nfrequencies() const
    {
      return Mspectrumarray.size(0);
    } // unsigned int DRFFTWAFFArrayEngine::nfrequencies() const

    /*----------------------------------------------------------------------*/

    unsigned int DRFFTWAFFArrayEngine::nseries() const
    {
      return Mseriesarray.size(1);
    } // unsigned int DRFFTWAFFArrayEngine::nseries() const

    /*----------------------------------------------------------------------*/

    //! \brief return a reference to the time series i
    DRFFTWAFFArrayEngine::TAseries 
      DRFFTWAFFArrayEngine::series(const unsigned int& i) const
    {
      FOURIER_assert(((i>=0) && (i<this->nseries())),
                     "ERROR: signal index is out of range");
      return(aff::slice(Mseriesarray)()(i));
    } // DRFFTWAFFArrayEngine::TAseries DRFFTWAFFArrayEngine::series(i) const

    /*----------------------------------------------------------------------*/

    //! \brief return a reference to the Fourier coefficients of series i
    DRFFTWAFFArrayEngine::TAspectrum 
      DRFFTWAFFArrayEngine::spectrum(const unsigned int& i) const
    {
      FOURIER_assert(((i>=0) && (i<this->nseries())),
                     "ERROR: signal index is out of range");
      return(aff::slice(Mspectrumarray)()(i));
    } // DRFFTWAFFArrayEngine::TAspectrum DRFFTWAFFArrayEngine::spectrum(i) const

  } // namespace fft

} // namespace fourier

/* ----- END OF fftwaffar.cc ----- */
