/*! \file fftwaffar.h
 * \brief engine to transfrom several signals at once (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/05/2011
 * 
 * engine to transfrom several signals at once (prototypes)
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
 * 
 * REVISIONS and CHANGES 
 *  - 13/05/2011   V1.0   Thomas Forbriger
 *  - 27/05/2011   V1.1   added copy constructor (required for class member
 *                        initialization) and default constructor and
 *                        assignment operator
 *  - 02/10/2012   V1.2 
 *                        - make container size calculation functions public
 * 
 * ============================================================================
 */

// include guard
#ifndef TF_FFTWAFFAR_H_VERSION

#define TF_FFTWAFFAR_H_VERSION \
  "TF_FFTWAFFAR_H   V1.2"

#include<complex>
#include<fftw3.h>
#include<aff/array.h>
#include<aff/series.h>

namespace fourier {

  namespace fft {

    /*! \brief Engine to transform several signals at once.
     * References to workspace are passed to the contstructor.
     *
     * The difference between DRFFTWAFF and DRFFTWAFFArrayEngine is that the
     * latter transforms more than on signal at the same time. It uses a
     * persistent workspace and makes full use of the reference semantics
     * implemented in aff::Array.
     *
     * The class is designed for strided arrays. aff::Array has to be used
     * together with aff::Strided. Currently (5/2011) there is no alternative
     * in the aff::Array class template.
     *
     * When initializing DRFFTWAFFArrayEngine either the dimensions of the
     * time series array must be passed or a readily prepared workspace is
     * passed to the engine. In both cases the engine assumes that the user
     * program holds references to the seismogram and the coefficient array
     * through instances of aff::Array. The engine is just called to do the
     * actual transform. New values to be transformed are received through the
     * common reference with the user program. A reference to the arrays is
     * provided through member functions.
     *
     * The first index to the array is the sample index, the second index is
     * the signal index. 
     * This is due to the column major arrangement used by aff::Array
     * containers by default (this is the common Fortran layout).
     *
     * \note
     * The c2r transform destroys its input data.
     * See
     * http://fftw.org/fftw3_doc/One_002dDimensional-DFTs-of-Real-Data.html#One_002dDimensional-DFTs-of-Real-Data
     */
    class DRFFTWAFFArrayEngine {
      public:
        typedef double Tsample;
        typedef std::complex<Tsample> Tcoeff;
        typedef aff::Array<Tsample> TAseries;
        typedef aff::Array<Tcoeff> TAspectrum;
        DRFFTWAFFArrayEngine(const int& nseis,
                             const int& nsamp);
        DRFFTWAFFArrayEngine(const TAseries& series,
                             const TAspectrum& spec);
        DRFFTWAFFArrayEngine(const DRFFTWAFFArrayEngine& engine);
        DRFFTWAFFArrayEngine();
        DRFFTWAFFArrayEngine& operator=(const DRFFTWAFFArrayEngine& e);
        ~DRFFTWAFFArrayEngine();
        void r2c();
        void c2r();
        Tsample scale_series(const Tsample& dt) const;
        Tsample scale_spectrum(const Tsample& dt) const;
        //! \brief return a reference to the time series arrays
        TAseries series() const { return Mseriesarray; }
        //! \brief return a reference to the Fourier coefficient arrays
        TAspectrum spectrum() const { return Mspectrumarray; }
        //! \brief return the number of series in the arrays
        unsigned int nseries() const;
        //! \brief return the number of samples in time series
        unsigned int nsamples() const;
        //! \brief return the number of positive frequencies used
        unsigned int nfrequencies() const;
        //! \brief return a reference to the time series i
        TAseries series(const unsigned int& i) const;
        //! \brief return a reference to the Fourier coefficients of series i
        TAspectrum spectrum(const unsigned int& i) const;

        //! return number of coefficients for given number of samples
        inline
        static unsigned int ncoeff(const unsigned int& nsamples)
        { return(nsamples/2+1); }

        //! return number of samples for given number of coefficients
        inline
        static unsigned int nsamples(const unsigned int& n) 
        { return(n*2-1); }

      private:
        void checkconsistency();
        void createplanr2c();
        void createplanc2r();
        void delete_plans();
        TAseries Mseriesarray;
        TAspectrum Mspectrumarray;
        fftw_plan Mplanr2c;
        fftw_plan Mplanc2r;
    }; // class DRFFTWAFFArrayEngine

  } // namespace fft

} // namespace fourier

#endif // TF_FFTWAFFAR_H_VERSION (includeguard)

/* ----- END OF fftwaffar.h ----- */
