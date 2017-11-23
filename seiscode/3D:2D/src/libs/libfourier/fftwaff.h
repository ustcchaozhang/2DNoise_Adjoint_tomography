/*! \file fftwaff.h
 * \brief use fftw together with aff containers (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/07/2006
 * 
 * use fftw together with aff containers (prototypes)
 *
 * link with -lrfftw -lfftw -lm -laff
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
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 11/07/2006   V1.0   Thomas Forbriger
 *  - 12/09/2007   V1.1   first running version
 *  - 07/10/2010   V1.2   
 *                        - migrate to FFTW3:
 *                        - use different fftw header file (fftw3.h)
 *                        - type of FFTW plan has changed
 *  - 02/10/2012   V1.3  
 *                        - provide size calculation functions
 *
 * Migration to FFTW3:
 * - The location of the arrays in memory are part of the plan. 
 *   Consequently we have to either create a new plan for each transform or to
 *   allocate the working array together with the plan and keep it available in
 *   the background. The solution of choice is to keep arrays. We will
 *   introduce a control parameter to the arguments of the constructor which
 *   optionally allows to get rid of the arrays after each transformation.
 * - FFTW3 uses wisdom by default.
 *   \code
 *   void fftw_forget_wisdom(void);
 *   \endcode
 *   should be called by the destructor of the class. Alternatively
 *   \code
 *   void fftw_cleanup(void);
 *   \endcode 
 *   could be called which apparently is even more rigorous. Plans, however,
 *   must be destroyed prior to calling \c fftw_cleanup.
 * - The FFTW documentation states:
 *   To the extent that this is true, if you have a variable 
 *   \code
 *   complex<double> *x,
 *   \endcode
 *   you can pass it directly to FFTW via
 *   \code
 *   reinterpret_cast<fftw_complex*>(x). 
 *   \endcode
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FFTWAFF_H_VERSION

#define TF_FFTWAFF_H_VERSION \
  "TF_FFTWAFF_H   V1.3"

#include<complex>
#ifdef FFTWFALLBACK
#include<drfftw.h>
#else
#include<fftw3.h>
#endif
#include<aff/series.h>

namespace fourier {

  /*! All Fourier transform stuff is collected here.
   */
  namespace fft {

    /*! A rigid class to do simple transforms using libdrfftw.a.
     *
     * uses real double arrays
     *
     * How to use this class:
     *
     * You may create one instance of this class and use it to transform
     * several signals in both directions in turn. The class itself takes care
     * of the transform size and creates a new plan if necessary. FFTs are
     * invoked by the bracket operators. You should use the class object like
     * a function. The scaling operators (taking the sampling interval as one
     * of their arguments) return a series or spectrum scaled appropriately to
     * match the values of samples from the corresponding Fourier integral
     * transform (usual convention with \f$dt\f$ and \f$df\f$
     * integrals - not \f$d \omega\f$).
     *
     * \note
     * The appropriate number of samples for the time series obtained from a
     * given set of Fourier coefficients is non-unique, if only the Fourier
     * coefficients for positive frequencies are given, as is the case here.
     * A time series with \f$2n\f$ samples and a time series with \f$2n+1\f$
     * samples both will results in a set of \f$n+1\f$ Fourier coefficients.
     * The method to determine the required number of time samples from the
     * imaginary part of the Nyquist Fourier coefficients only works if the
     * Nyquist coefficient (at least real part) is finite - which is not the
     * case for most of our signals. This way we will obtain \f$n+1\f$ Fourier
     * coefficients from \f$2n\f$ time series samples. When reconstructing the
     * time series, we will obtain \f$2n+1\f$ samples with a sampling interval
     * now being \f$2n/(2n+1)\f$ of the original interval. For this reason it
     * is appropriate to set the size of the expected time series explicitely
     * either by using the constructor 
     * DRFFTWAFF::DRFFTWAFF(const unsigned int& n) or by
     * calling DRFFTWAFF::size(const unsigned int& s) prior to
     * DRFFTWAFF::operator()(const Tspectrum::Tcoc& s).
     *
     * \sa \ref page_fftw3
     */
    class DRFFTWAFF {
      public:
        typedef double Tsample;
        typedef std::complex<Tsample> Tcoeff;
        typedef aff::Series<Tsample> Tseries;
        typedef aff::Series<Tcoeff> Tspectrum;
#ifdef FFTWFALLBACK
        DRFFTWAFF(const unsigned int& n):
          Msize(n), Mplan_forward(0), Mplan_backward(0) { }
        DRFFTWAFF():
          Msize(0), Mplan_forward(0), Mplan_backward(0) { }
#else
        DRFFTWAFF(const unsigned int& n, const bool& deletearrays=false):
          Msize(n), Mplan_forward(0), Mplan_backward(0),
          Mseriesarray(0), Mspectrumarray(0),
          Mdeletearrays(deletearrays) { }
        DRFFTWAFF(const bool& deletearrays=false):
          Msize(0), Mplan_forward(0), Mplan_backward(0),
          Mseriesarray(0), Mspectrumarray(0),
          Mdeletearrays(deletearrays) { }
#endif
        ~DRFFTWAFF();
        Tspectrum operator()(const Tseries::Tcoc& s,
                             const bool& debug=false) const;
        Tseries operator()(const Tspectrum::Tcoc& s,
                           const bool& debug=false) const;
        Tspectrum operator()(const Tseries::Tcoc& s,
                             const Tsample& dt,
                             const bool& debug=false) const;
        Tseries operator()(const Tspectrum::Tcoc& s,
                           const Tsample& dt,
                           const bool& debug=false) const;
        Tsample scale_series(const Tsample& dt) const;
        Tsample scale_spectrum(const Tsample& dt) const;
        unsigned int size() const { return(Msize); }
        void size(const unsigned int& s) const { this->set_size(s); }

        //! return number of coefficients for given number of samples
        inline
        static unsigned int spectrumsize(const unsigned int& n) 
        { return(n/2+1); }

        //! return number of samples for given number of coefficients
        inline
        static unsigned int seriessize(const unsigned int& n) 
        { return(n*2-1); }

      private:
        void create_plan_forward() const;
        void create_plan_backward() const;
        void delete_plans() const;
#ifndef FFTWFALLBACK
        void create_arrays() const;
        void delete_arrays() const;
        unsigned int ssize() const 
        { return(DRFFTWAFF::spectrumsize(this->size())); }
#endif
        void set_size(const unsigned int& n) const;
        mutable unsigned int Msize;
#ifdef FFTWFALLBACK
        mutable rfftw_plan Mplan_forward;
        mutable rfftw_plan Mplan_backward;
#else
        mutable fftw_plan Mplan_forward;
        mutable fftw_plan Mplan_backward;
        mutable double *Mseriesarray;
        mutable fftw_complex *Mspectrumarray;
        mutable Tspectrum Mspectrum;
        mutable Tseries Mseries;
        bool Mdeletearrays;
#endif
    }; // class DRFFTWAFF

  } // namespace ftt

} // namespace fourier

#endif // TF_FFTWAFF_H_VERSION (includeguard)

/* ----- END OF fftwaff.h ----- */
