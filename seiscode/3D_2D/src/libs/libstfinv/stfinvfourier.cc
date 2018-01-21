/*! \file stfinvfourier.cc
 * \brief a base class for all engines which operate in the Fourier domain (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/05/2011
 * 
 * a base class for all engines which operate in the Fourier domain (implementation)
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This program is free software; you can redistribute it and/or modify
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
 *  - 08/05/2011   V1.0   Thomas Forbriger
 *  - 30/09/2011   V1.1   implemented handling of additional time series pairs
 *  - 04/10/2011   V1.2   correction in debug message
 *  - 14/10/2015   V1.3   new end-user usage functions
 *  - 28/06/2016   V1.4   introduce taper parameter to taper correction filter
 *                        response in the time domain
 *  - 22/07/2016   V1.5   thof:
 *                        - provide separate FFT processor addressing just the
 *                          source time function correction filter
 *                        - implement taper function
 * 
 * ============================================================================
 */
#define STFINV_STFINVFOURIER_CC_VERSION \
  "STFINV_STFINVFOURIER_CC   V1.5"

#include <sstream>
#include <cmath>
#include <stfinv/stfinvfourier.h>
#include <stfinv/stfinvfourier_summary_usage.h>
#include <stfinv/stfinvfourier_description_usage.h>
#include <stfinv/debug.h>
#include <aff/subarray.h>
#include <aff/slice.h>
#include <aff/arrayoperators.h>

namespace stfinv {

  /*! 
   * - References to signal storage are available
   * - Parameters are parsed and available through the base class
   * What has to be done here:
   * -# The FFT engine must be initialized appropriately
   *    -# Time domain padding has to be read from the parameters
   *    -# Number of sample modulo has to be read from parameters
   *    -# number of samples must be calculated from these values
   *    -# based on these values workspace arrays must be constructed
   *    -# The FFT engine must be constructed from the workspace arrays.
   *       This requires a copy constructor for the FFT engine, which
   *       currently is not present.
   *       Alternatively, we could only use a pointer to the engine.
   */
  STFFourierDomainEngine::STFFourierDomainEngine(const stfinv::Tvectoroftriples& triples,
                                                 const stfinv::Waveform& stf,
                                                 const stfinv::Tvectorofpairs& pairs,
                                                 const std::string& parameters)
    :Tbase(triples, stf, pairs, parameters)
  { 
    this->initialize();
  } // STFFourierDomainEngine::STFFourierDomainEngine

  /*----------------------------------------------------------------------*/

  /*! 
   * - References to signal storage are available
   * - Parameters are parsed and available through the base class
   * What has to be done here:
   * -# The FFT engine must be initialized appropriately
   *    -# Time domain padding has to be read from the parameters
   *    -# Number of sample modulo has to be read from parameters
   *    -# number of samples must be calculated from these values
   *    -# based on these values workspace arrays must be constructed
   *    -# The FFT engine must be constructed from the workspace arrays.
   *       This requires a copy constructor for the FFT engine, which
   *       currently is not present.
   *       Alternatively, we could only use a pointer to the engine.
   */
  STFFourierDomainEngine::STFFourierDomainEngine(const stfinv::Tvectoroftriples& triples,
                                                 const stfinv::Waveform& stf,
                                                 const std::string& parameters)
    :Tbase(triples, stf, parameters)
  { 
    this->initialize();
  } // STFFourierDomainEngine::STFFourierDomainEngine

  /*----------------------------------------------------------------------*/

  void STFFourierDomainEngine::help(std::ostream& os) const
  {
    STFFourierDomainEngine::classhelp(os);
  } // void STFFourierDomainEngine::help(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  void STFFourierDomainEngine::usage(std::ostream& os) const
  {
    STFFourierDomainEngine::classusage(os);
  } // void STFFourierDomainEngine::usage(std::ostream& os) const


  /*----------------------------------------------------------------------*/

  const char* STFFourierDomainEngine::name() const
  {
    return("STFFourierDomainEngine");
  } //  const char const* STFFourierDomainEngine::name() const

  /*----------------------------------------------------------------------*/

  /*! \brief online help text giving information on options
   *
   * This must be kept synchronous with the options used by
   * STFFourierDomainEngine::initialize()
   */
  void STFFourierDomainEngine::classhelp(std::ostream& os)
  {
    os << stfinvfourier_summary_usage;
  } // void STFFourierDomainEngine::classhelp(std::ostream& os)

  /*----------------------------------------------------------------------*/

  /*! \brief online help text giving information on options
   *
   * This must be kept synchronous with the options used by
   * STFFourierDomainEngine::initialize()
   */
  void STFFourierDomainEngine::classusage(std::ostream& os)
  {
    os << stfinvfourier_description_usage;
    os << std::endl;
    Tbase::classusage(os);
  } // void STFFourierDomainEngine::classusage(std::ostream& os)

  /*----------------------------------------------------------------------*/

  /*! \brief Create FFT engines
   *
   * \par Number of samples
   * For the FFT we usually use a number of samples larger than that of the
   * underlying time series. 
   * This way we at least partially avoid the undesired effect of the cyclic
   * discrete Fourier transform, which can lead to wrap-around.
   * Currently there are three ways to modify the number of samples actually
   * used:
   * -# Option \c fpad specifies an padding factor. 
   *    The number of samples simply is larger than the number of samples of
   *    the underlying time series by this factor.
   * -# Option \c fpow2:
   *    If set, the next power of two larger than the fpad*nsamples is used.
   * -# Option \c fdiv specifies a divisor.
   *    If set and if \c fpow2 is not set the number of samples will be the
   *    next integer multiple of the divisor larger than fpad*nsamples.
   *
   * \par Workspace
   * Two FFT engines will be created.
   * They are only used one-way, since input data is not altered during
   * processing input time series only have to be transformed once to Fourier
   * domain.
   * Results of processing have to be transformed to time domain.
   * -# One engine (\c STFFourierDomainEngine::Mfftengineinput) being shared
   *  by recorded data and synthetic data, because both have to be transformed
   *  to Fourier domain at once.
   *  Transformation to Fourier domain takes place in
   *  STFFourierDomainEngine::fftinput.
   * -# One engine (\c STFFourierDomainEngine::Mfftengineoutput) being shared
   *  by the stf and the convolved synthetics, because both have to be
   *  transformed to the time domain at once.
   *  Transformation to time domain takes place in
   *  STFFourierDomainEngine::fftoutput.
   */
  void STFFourierDomainEngine::initialize()
  {
    // extract parameter values
    // ------------------------
    // padding factor
    double padfactor;
    {
      std::istringstream is(this->parameter("fpad","1.5"));
      is >> padfactor;
    }
    STFINV_assert(padfactor >= 1.,
                  "ERROR: parameter for option \"fpad\" not larger or equal 1");
    // flag: use power of two
    bool poweroftwo=(this->parameter("fpow2","false")=="true");
    // flag: use integer multiples of divisor
    bool divisorset=this->parameterisset("fdiv");
    // number of samples shall be integer power of divisor
    unsigned int divisor=1;
    if (divisorset)
    {
      std::istringstream is (this->parameter("fdiv","100")); 
      is >> divisor;
      STFINV_assert(divisor > 0,
                  "ERROR: parameter for option \"fdiv\" not larger than 0");
    }

    // define number of samples to be used by Fourier engine
    // -----------------------------------------------------
    // use at least the number of samples sepcified by the padfactor
    unsigned int nsamples=padfactor*this->nsamples();
    if (nsamples<this->nsamples()) { nsamples=this->nsamples(); }
    // power of two has precendence
    if (poweroftwo)
    {
      unsigned int n=2;
      while (n<nsamples) { n *= 2; }
      nsamples=n;
    }
    else if (divisorset)
    {
      unsigned int rest=nsamples % divisor;
      nsamples -= rest;
      nsamples += divisor;
    } // if (poweroftwo) or (divisorset)

    // allocate workspace by creating engines
    // --------------------------------------
    Mfftengineinput=fourier::fft::DRFFTWAFFArrayEngine(
                                     2*this->nreceivers()+this->npairs(),
                                                       nsamples);
    Mfftengineoutput=fourier::fft::DRFFTWAFFArrayEngine(
                                     1+this->nreceivers()+this->npairs(),
                                                        nsamples);
    Mfftenginestf=fourier::fft::DRFFTWAFFArrayEngine(this->stfseries(),
                                                     this->stfspec());

    // set time shift for STF if requested
    // -----------------------------------
    {
      std::istringstream is(this->parameter("tshift","0."));
      is >> Mtshift;
    }
    Mapplyshift=this->parameterisset("tshift");
      
    // taper definition
    // ----------------
    {
      std::istringstream is(stfinv::tools::secomtospace(
                            this->parameter("irtap","0.,1.,2.,3.")));
      is >> Mtt1 >> Mtt2 >> Mtt3 >> Mtt4;
    }
    Mapplystftaper=this->parameterisset("irtap");
    STFINV_assert((Mtt1<=Mtt2) && (Mtt2<=Mtt3) && (Mtt3<=Mtt4),
                  "ERROR: taper definition times not in increasing order");

    STFINV_debug(Mdebug&1, "STFFourierDomainEngine::initialize()",
                 STFINV_value(this->nsamples()) << "\n  " <<
                 STFINV_value(padfactor) << "\n  " <<
                 STFINV_value(divisor) << "\n  " <<
                 STFINV_value(nsamples) << "\n  " <<
                 STFINV_value(Mapplyshift) << "\n  " <<
                 STFINV_value(Mtshift) << "\n  " <<
                 STFINV_value(Mapplystftaper) << "\n  " <<
                 STFINV_value(Mtt1) << "\n  " <<
                 STFINV_value(Mtt2) << "\n  " <<
                 STFINV_value(Mtt3) << "\n  " <<
                 STFINV_value(Mtt4));
  } // void STFFourierDomainEngine::initialize() 

  /*----------------------------------------------------------------------*/

  /*!
   * Provide data passed (through the API) by the caller of the library to an
   * engine operating in the Fourier domain.
   * All input is available as time series data in first place.
   * -# Copy time series data to STFFourierDomainEngine::Mfftengineinput
   * -# Transform time series data to Fourier domain
   * -# Clear output coefficients
   *
   * This function should be called by the very first statement of the
   * exec-function of the derived class (e.g. STFEngineFDLeastSquares::exec).
   */
  void STFFourierDomainEngine::fftinput()
  {
    this->getinput();
    Mfftengineinput.r2c();
    Mfftengineoutput.series()=0.;
  } // void STFFourierDomainEngine::fftinput() 

  /*----------------------------------------------------------------------*/

  /*!
   * Provide results of Fourier domain operation to the caller of the library.
   * -# Apply time domain taper to correction filter, if requested
   * -# Apply convolution with correction filter response to all synthetic
   *    input data
   * -# Apply time shift to impulse response of correction filter, if
   *    requested
   * -# Transform Fourier series to time domain
   * -# Copy time series data to buffer array accessible through API
   *
   * This function should be called by the very last statement of the
   * exec-function of the derived class (e.g. STFEngineFDLeastSquares::exec).
   */
  void STFFourierDomainEngine::fftoutput()
  {
    if (this->Mapplystftaper) { this->taperstf(); }
    this->convolve();
    if (Mapplyshift) { this->stfshift(); }
    Mfftengineoutput.c2r();
    this->putoutput();
  } // void STFFourierDomainEngine::fftoutput() 

  /*----------------------------------------------------------------------*/

  STFFourierDomainEngine::TAspectrum
    STFFourierDomainEngine::recordingspec() const
  {
    TAspectrum inspecarray=Mfftengineinput.spectrum();
    TAspectrum subarray=aff::subarray(inspecarray)()(0,this->nreceivers()-1);
    subarray.shape().setfirst(0,0);
    subarray.shape().setfirst(1,0);
    return(subarray);
  } // STFFourierDomainEngine::TAspectrum STFFourierDomainEngine::recordingspec() const

  /*----------------------------------------------------------------------*/

  STFFourierDomainEngine::TAspectrum
    STFFourierDomainEngine::syntheticspec() const
  {
    TAspectrum inspecarray=Mfftengineinput.spectrum();
    TAspectrum subarray=aff::subarray(inspecarray)()(this->nreceivers(),
                                       (2*this->nreceivers())-1);
    subarray.shape().setfirst(0,0);
    subarray.shape().setfirst(1,0);
    return(subarray);
  } // STFFourierDomainEngine::TAspectrum STFFourierDomainEngine::syntheticspec() const

  /*----------------------------------------------------------------------*/

  STFFourierDomainEngine::TAspectrum STFFourierDomainEngine::stfspec() const
  {
    return(Mfftengineoutput.spectrum(this->nreceivers()));
  } // STFFourierDomainEngine::Tspectrum STFFourierDomainEngine::stfspec() const

  /*----------------------------------------------------------------------*/

  STFFourierDomainEngine::TAseries STFFourierDomainEngine::stfseries() const
  {
    return(Mfftengineoutput.series(this->nreceivers()));
  } // STFFourierDomainEngine::Tspectrum STFFourierDomainEngine::stfseries() const

  /*----------------------------------------------------------------------*/

  STFFourierDomainEngine::TAspectrum 
    STFFourierDomainEngine::recordingcoeff(const unsigned int& i) const
  {
    STFFourierDomainEngine::TAspectrum rec=this->recordingspec();
    STFFourierDomainEngine::TAspectrum slice=aff::slice(rec)(0,i);
    slice.shape().setfirst(0,0);
    slice.shape().setfirst(1,0);
    return(slice);
  } // STFFourierDomainEngine::recordingcoeff(const unsigned int& i) const

  /*----------------------------------------------------------------------*/

  STFFourierDomainEngine::TAspectrum 
    STFFourierDomainEngine::syntheticcoeff(const unsigned int& i) const
  {
    STFINV_debug(Mdebug&8, "STFFourierDomainEngine::syntheticcoeff",
                 "i=" << i);
    STFFourierDomainEngine::TAspectrum syn=this->syntheticspec();
    STFFourierDomainEngine::TAspectrum slice=aff::slice(syn)(0,i);
    slice.shape().setfirst(0,0);
    slice.shape().setfirst(1,0);
    STFINV_debug(Mdebug&8, "STFFourierDomainEngine::syntheticcoeff",
                 "slice.first(0)=" << slice.first(0) << " " <<
                 "slice.last(0)=" << slice.last(0) << " "
                 "slice.first(1)=" << slice.first(1) << " " <<
                 "slice.last(1)=" << slice.last(1) << " ");
    return(slice);
  } // STFFourierDomainEngine::syntheticcoeff(const unsigned int& i) const

  /*----------------------------------------------------------------------*/

  STFFourierDomainEngine::TAspectrum::Tvalue& 
    STFFourierDomainEngine::stfcoeff(const unsigned int& i) const
  {
    STFFourierDomainEngine::TAspectrum stffft=this->stfspec();
    return(stffft(i));
  } // STFFourierDomainEngine::stfcoeff(const unsigned int& i) const

  /*----------------------------------------------------------------------*/

  double STFFourierDomainEngine::frequency(const unsigned int& i) const
  {
    return(static_cast<double>(i)/(this->dt()*Mfftengineinput.nsamples()));
  } // double STFFourierDomainEngine::frequency(const unsigned int& i) const

  /*----------------------------------------------------------------------*/

  unsigned int STFFourierDomainEngine::nfreq() const
  {
    return(Mfftengineinput.nfrequencies());
  } // unsigned int STFFourierDomainEngine::nfreq() const

  /*----------------------------------------------------------------------*/

  void STFFourierDomainEngine::getinput()
  {
    // clear workspace through reference
    // (remark: return value sarray is a reference to an array addressing all
    // samples of all time series contained in Mfftengineinput)
    TAseries sarray=Mfftengineinput.series();
    sarray=0.;

    // cycle through receivers
    for (unsigned int i=0; i<this->nreceivers(); ++i)
    {
      // get references to time series in workspace
      TAseries recordingref=Mfftengineinput.series(i);
      TAseries syntheticref=Mfftengineinput.series(i+this->nreceivers());
      // copyin function copies as many elements as possible
      recordingref.copyin(this->recording(i));
      syntheticref.copyin(this->synthetic(i));
    } // for (unsigned int i=0; i<this->nreceivers(); ++i)

    // cycle through additional time series pairs
    for (unsigned int i=0; i<this->npairs(); ++i)
    {
      // get references to time series in workspace
      TAseries syntheticref=Mfftengineinput.series(i+2*this->nreceivers());
      // copyin function copies as many elements as possible
      syntheticref.copyin(this->series(i));
    } // for (unsigned int i=0; i<this->npairs(); ++i)
  } // void STFFourierDomainEngine::getinput() 

  /*----------------------------------------------------------------------*/

  /*!
   * Read out time series data from Mfftengineoutput.
   * The time series containers in Mfftengineoutput are expected to be larger
   * than those used for the API of the library (because of padding).
   */
  void STFFourierDomainEngine::putoutput()
  {
    // scaling factor for source correction filter
    double scalingstf=Mfftengineoutput.scale_series(this->dt());
    // scaling factor for convolved synthetics
    double scalingcs=Mfftengineoutput.scale_spectrum(this->dt())
      *scalingstf;

    // cycle through receivers
    for (unsigned int i=0; i<this->nreceivers(); ++i)
    {
      // get references to time series 
      stfinv::Tseries convolvedsynthetics=this->convolvedsynthetic(i);
      // copyin function copies as many elements as possible
      convolvedsynthetics.copyin(Mfftengineoutput.series(i)*scalingcs);
    } // for (unsigned int i=0; i<this->nreceivers(); ++i)
      
    // copy stf too; get reference to series and use copyin
    stfinv::Tseries stf=this->stf();
    stf.copyin(Mfftengineoutput.series(this->nreceivers())
               *scalingstf);

    // cycle through additional time series pairs
    for (unsigned int i=0; i<this->npairs(); ++i)
    {
      // get references to time series 
      stfinv::Tseries convolvedseries=this->convolvedseries(i);
      // copyin function copies as many elements as possible
      convolvedseries.copyin(Mfftengineoutput.series(1+i+this->nreceivers())
                             *scalingcs);
    } // for (unsigned int i=0; i<this->npairs(); ++i)

  } // void STFFourierDomainEngine::putoutput() 

  /*----------------------------------------------------------------------*/

  /*! \brief Convolve all synthetics with the source time function correction
   *         filter.
   *
   * The filter response is available in terms of the Fourier coefficients of
   * the correction filter as are returned by
   * STFFourierDomainEngine::stfspec().
   * Convolution takes place in the Fourier domain simply by a multiplication
   * of Fourier coefficients.
   * Since all Fourier coefficients are expected to be scaled appropriately to
   * represent samples for the Fourier integral transform, no scaling has to
   * be applied here.
   */
  void STFFourierDomainEngine::convolve()
  {
    TAspectrum synthetic=this->syntheticspec();
    TAspectrum convolvedsynthetic
      =aff::subarray(Mfftengineoutput.spectrum())()(0,this->nreceivers()-1);
    convolvedsynthetic.shape().setfirst(0,0);
    convolvedsynthetic.shape().setfirst(1,0);
    TAspectrum stfcoeff=this->stfspec();

    // cycle through all receivers
    for (unsigned int i=0; i<this->nreceivers(); ++i)
    {
      for (unsigned int j=0; j<this->nfreq(); ++j)
      {
        convolvedsynthetic(j,i)=synthetic(j,i)*stfcoeff(j);
      } // for (unsigned int j=0; j<this->nfreq(); ++j)
    } // for (unsigned int i=0; i<this->nreceivers(); ++i)

    // copy signals for additional pairs
    if (this->npairs()>0)
    {
      // reference to input series Fourier coefficients of additional pairs
      TAspectrum inspecarray=Mfftengineinput.spectrum();
      TAspectrum seriesspecarray
        =aff::subarray(inspecarray)()(2*this->nreceivers(),
                                         (2*this->nreceivers()
                                          +this->npairs()-1));
      seriesspecarray.shape().setfirst(0,0);
      seriesspecarray.shape().setfirst(1,0);

      // reference to convolved series Fourier coefficients of additional
      // pairs
      TAspectrum convolvedseries
        =aff::subarray(Mfftengineoutput.spectrum())()(this->nreceivers()+1,
                                                      this->nreceivers()
                                                      +this->npairs());
      convolvedseries.shape().setfirst(0,0);
      convolvedseries.shape().setfirst(1,0);
      // cycle through all pairs
      for (unsigned int i=0; i<this->npairs(); ++i)
      {
        for (unsigned int j=0; j<this->nfreq(); ++j)
        {
          convolvedseries(j,i)=seriesspecarray(j,i)*stfcoeff(j);
        } // for (unsigned int j=0; j<this->nfreq(); ++j)
      } // for (unsigned int i=0; i<this->npairs(); ++i)
    } // if (this->npairs()>0)

  } // void STFFourierDomainEngine::convolve() 

  /*----------------------------------------------------------------------*/

  void STFFourierDomainEngine::stfshift()
  {
    STFINV_debug(Mdebug&2, "STFFourierDomainEngine::stfshift()",
                 "apply time shift of " << Mtshift << " seconds");
    const STFFourierDomainEngine::TAspectrum::Tvalue sfac
      =-STFFourierDomainEngine::TAspectrum::Tvalue(0.,1.)
      *3.141592653589793115998*2.*Mtshift;
    // STFINV_DUMP( sfac );
    STFFourierDomainEngine::TAspectrum stffft=this->stfspec();
    for (unsigned int j=0; j<this->nfreq(); ++j)
    {
      stffft(j) *= std::exp(sfac*this->frequency(j));
      // STFINV_DUMP( std::exp(sfac*this->frequency(j)) );      
    } // for (unsigned int j=0; j<this->nfreq(); ++j)
  } // void STFFourierDomainEngine::stfshift() 

  /*----------------------------------------------------------------------*/

  /*!
   * Apply a time domain taper to the impulse response of the source time
   * function correction filter.
   */
  void STFFourierDomainEngine::taperstf()
  {
    STFINV_debug(Mdebug&16, "STFFourierDomainEngine::taperstf()",
                 STFINV_value(Mapplystftaper));
    if (this->Mapplystftaper)
    {
      // transform correction filter to time domain
      Mfftenginestf.c2r();
      Tfftengine::TAseries thestfseries=this->stfseries();
      thestfseries *= Mfftenginestf.scale_series(this->dt());
      // apply taper
      /*
       * Concept of application to periodic time series allowing for negative
       * values of taper times:
       *
       * All samples between t2 and t3 remain unaltered. It is reasonable to
       * process the taper starting at t3 passing t4 to t1 and then ending at
       * t2. Behind this is the concept that time series are implicitely
       * periodic in discrete Fourier transform.
       */
      // samples in series
      int nsamples=thestfseries.size(0);
      // duration of time series
      double T=this->dt()*nsamples;
      // make sure taper definition is not longer than time series.
      STFINV_assert((this->Mtt4-this->Mtt1)<T,
                    "Taper width is larger than length of time series");
      // set taper time values under conpect of periodic series
      double tt3=this->Mtt3;
      double tt4=this->Mtt4;
      double tt1=this->Mtt1+T;
      double tt2=this->Mtt2+T;
      // sample to start at
      int l3=int(std::floor(tt3/this->dt()));
      int l2=int(std::ceil(tt2/this->dt()));
      STFINV_debug(Mdebug&16, "STFFourierDomainEngine::taperstf()",
                   STFINV_value(T) << "\n   " <<
                   STFINV_value(this->dt()) << "\n   " <<
                   STFINV_value(nsamples) << "\n   " <<
                   STFINV_value(tt3) << "\n   " <<
                   STFINV_value(tt4) << "\n   " <<
                   STFINV_value(tt1) << "\n   " <<
                   STFINV_value(tt2) << "\n   " <<
                   STFINV_value(l3) << "\n   " <<
                   STFINV_value(l2));
      for (int l=l3; l<=l2; ++l)
      {
        // time of sample
        double t=l*this->dt();
        // index to series
        int rl = l%nsamples;
        if (rl < 0) { rl += nsamples; }
        STFINV_assert(((rl >= thestfseries.f(0)) && 
                       (rl <= thestfseries.l(0))),
                      "Index out of range. Internal programming error! "
                      "Report as bug!")
        // taper factor
        double factor=1.;
        if ( (t>=tt3) && (t<=tt4) && (tt4>tt3) )
        {
          factor=0.5+0.5*cos(M_PI*(t-tt3)/(tt4-tt3));
        }
        else if ( (t>tt4) && (t<tt1) )
        {
          factor=0.;
        }
        else if ( (t>=tt1) && (t<=tt2) && (tt2>tt1) )
        {
          factor=0.5-0.5*cos(M_PI*(t-tt1)/(tt2-tt1));
        }
        // apply to series
        thestfseries(rl) = thestfseries(rl)*factor;
        STFINV_debug(Mdebug&16, "STFFourierDomainEngine::taperstf()",
                     STFINV_value(t) << " " <<
                     STFINV_value(l) << " " <<
                     STFINV_value(rl) << " " <<
                     STFINV_value(factor));
      }
      // transform correction filter to Fourier domain
      Mfftenginestf.r2c();
      this->stfspec() *= Mfftenginestf.scale_spectrum(this->dt());
    } // if (this->Mapplystftaper)
  } // void STFFourierDomainEngine::taperstf()

} // namespace stfinv

/* ----- END OF stfinvfourier.cc ----- */
