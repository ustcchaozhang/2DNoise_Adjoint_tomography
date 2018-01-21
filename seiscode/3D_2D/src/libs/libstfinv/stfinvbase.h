/*! \file stfinvbase.h
 * \brief C++ interface and abstract base class (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * C++ interface and abstract base class (prototypes)
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
 *  - 06/05/2011   V1.0   Thomas Forbriger
 *  - 30/09/2011   V1.1   implemented handling of additional time series pairs
 *  - 14/10/2015   V1.2   new end-user usage functions
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_STFINVBASE_H_VERSION

#define STFINV_STFINVBASE_H_VERSION \
  "STFINV_STFINVBASE_H   V1.2"

#include<stfinv/waveformheader.h>
#include<stfinv/parameterhandler.h>
#include<stfinv/error.h>
#include<aff/series.h>
#include<iostream>
#include<vector>

namespace stfinv {

  /*! \brief Type of sample values.
   * \ingroup cxxinterface
   */
  typedef aff::Series<Tvalue> Tseries;

  /*----------------------------------------------------------------------*/

  /*! \brief A class to store the time series for a pair of time series.
   * \ingroup cxxinterface
  */
  struct WaveformPair {
    /*! \brief Temporal sampling
    */
    CWaveformHeader sampling;
    /*! \brief Time series of synthetic data.
     */
    Tseries::Tcoc synthetics;
    /*! \brief Time series of convolved synthetic data.
     * This will contain the synthetic data convolved with the obtained source
     * time function as a result of a call to the library functions.
     */
    Tseries convolvedsynthetics;
  }; // class WaveformPair

  /*----------------------------------------------------------------------*/

  /*! \brief A class to store the time series for a waveform triple.
   * \ingroup cxxinterface
  */
  struct WaveformTriple {
    /*! \brief The header is expected to be the same for all three time series.
     * In particular the number of samples must be appropriate for the memory
     * allocated for all three time series arrays.
     */
    CTripleHeader header;
    /*! \brief Time series of recorded data.
    */
    Tseries::Tcoc data;
    /*! \brief Time series of synthetic data.
     * This is understood as the impulse response of the subsurface.
     */
    Tseries::Tcoc synthetics;
    /*! \brief Time series of convolved synthetic data.
     * This will contain the synthetic data convolved with the obtained source
     * time function as a result of a call to the library functions.
     */
    Tseries convolvedsynthetics;
    /*! \brief Return source offset of this waveform
     */
    double offset() const;
  }; // class WaveformTriple

  /*----------------------------------------------------------------------*/

  /*! \brief A class to store a single waveform.
   * This will be used to pass the source correction filter.
   * \ingroup cxxinterface
   */
  struct Waveform {
    /*! \brief Temporal sampling
    */
    CWaveformHeader sampling;
    /*! \brief Time series of waveform.
    */
    Tseries series;
  }; // class Waveform

  /*----------------------------------------------------------------------*/

  /*! \brief Vector of pairs.
   * \ingroup cxxinterface
   */
  typedef std::vector<stfinv::WaveformPair> Tvectorofpairs;

  /*----------------------------------------------------------------------*/

  /*! \brief Vector of triples.
   * \ingroup cxxinterface
   */
  typedef std::vector<stfinv::WaveformTriple> Tvectoroftriples;

  /*----------------------------------------------------------------------*/

//! \brief Abort upon illegal call of base class function.
#define STFINV_baseillegal STFINV_abort("illegal call to abstract base class!")

  /*----------------------------------------------------------------------*/

  /*! \brief Abstract base class for engines to derive source correction
   * filter
   * \ingroup cxxinterface
   *
   * \par What STFBaseEngine does for you
   * The base class takes care of parameter reading.
   * It parses the parameter string passed to its constructor and provides the
   * values through protected member functions:
   *   - STFBaseEngine::parameter
   *   - STFBaseEngine::parameterisset
   * 
   * \par
   * The funtion STFBaseEngine::run is called by the user and passes execution
   * to the virtual function STFBaseEngine::exec which ensures that the exec()
   * funtion of the appropriate child class is called to initiate processing.
   *
   * \par Initialization of engines
   * The following happens in a sequence, when engines are initialized:
   * -# The desired engine ins initialized either by the user directly
   *    or within the frame of STFEngine.
   * -# The constructor of this engine immediatly calls teh constructor of its
   *    base class.
   *    This again calls the constructor of its base class such passing down
   *    initialization to STFBaseEngine::STFBaseEngine
   * -# The constructor of STFBaseEngine takes the references to the storage
   *    locations for signal data as well as a parameter string.
   *    -# It fills its own references to the data storage locations and checks
   *       the consistency of parameters passed (size of arrays, sampling rates,
   *       etc).
   *    -# It then parses the parameter string and places its contents in the
   *       Mparamap parameter map.
   *    -# Then the base class parameters are set either from defaults or form
   *       values in the parameter map.
   * -# Initialization then continues at the next higher level in the class
   *    hierarchy.
   *    -# In the next level constructor additional workspace can be allocated
   *       based on the already available references to signal data and 
   *       parameter values.
   *    -# Further level specific parameters are set due to defaults or
   *       values passed in the parameter string.
   *
   * \par Handling of parameters
   * See STFBaseEngine::parameter() and STFBaseEngine::parameterisset()
   * for a description on how parameters are handled within engine classes.
   *
   * \par Data
   *  - recordings are understood as recorded waveforms.
   *    The number of available traces is returned by nreceivers()
   *  - synthetics are understood as synthetic waveforms generated with a
   *    generic source time function.
   *    For each trace of recordings there must be a matching trace of
   *    synthetics and vice versa.
   *  - convolvedsynthetics are produced from synthetics as a result of the
   *    application of the source wavelet correction filter.
   *  - series are additional synthetic time series, not used to construct the
   *    source-wavelet correction filter. The number of additional series
   *    traces is returned by npairs().
   *  - convolvedseries are produced from series as a result of the
   *    application of the source wavelet correction filter.
   *
   * \sa
   * \ref page_i_subsec_design_initialization
   *
   * \todo
   * The base class should provide a "log to file" option such that applied
   * weights and signal energies could be reported.
   */
  class STFBaseEngine {
    protected:
      /*! \brief Constructor.
       */
      STFBaseEngine(const stfinv::Tvectoroftriples& triples,
                    const stfinv::Waveform& stf,
                    const std::string& parameters);
      /*! \brief Constructor.
       * This constructor additionally takes a vector of time series pairs.
       * In this vector references to synthetic time series data can be
       * passed, which are not used in the process of determining the optimal
       * source correction filter but which are convolved with the new source
       * correction filter on the fly.
       */
      STFBaseEngine(const stfinv::Tvectoroftriples& triples,
                    const stfinv::Waveform& stf,
                    const stfinv::Tvectorofpairs& pairs,
                    const std::string& parameters);
    public:
      //! \brief abstract base requires virtual destructor
      virtual ~STFBaseEngine() { }

      /*! \name Basic interface for users
       * \sa \ref page_users
       */
      //@{
      //! \brief Start engine and return reference to source correction filter.
      stfinv::Waveform run() 
      {
        this->exec();
        return(Mstf);
      }
      //! \brief print online help
      virtual void help(std::ostream& os=std::cout) const;
      //! \brief print detailed description
      virtual void usage(std::ostream& os=std::cout) const;
      //! \brief return name of engine
      virtual const char* name() const;
      //! \brief print online help
      static void classhelp(std::ostream& os=std::cout);
      //! \brief print detailed description
      static void classusage(std::ostream& os=std::cout);
      //@}
        
      /*! \name Shape query functions
       */
      //@{
      //! \brief return number of samples used in time series
      unsigned int nsamples() const
      { return (Mstf.sampling.n); }
      //! \brief return number of receiver signals in use
      unsigned int nreceivers() const
      { return (Mtriples.size()); }
      //! \brief return number of additional signals to be convolved
      unsigned int npairs() const
      { return (Mpairs.size()); }
      //! \brief return sampling interval
      double dt() const 
      { return (Mstf.sampling.dt); }
      //@}

      /*! \name Data query functions
       */
      //@{
      //! \brief return source correction filter series
      Tseries stf() const 
      { return (Mstf.series); }
      //! \brief return recorded data at receiver \c i
      Tseries::Tcoc recording(const unsigned int& i) const ;
      //! \brief return synthetic data at receiver \c i
      Tseries::Tcoc synthetic(const unsigned int& i) const ;
      //! \brief return synthetic data convolved with stf at receiver \c i
      Tseries convolvedsynthetic(const unsigned int& i) const ;
      //! \brief return synthetic data of pair \c i
      Tseries::Tcoc series(const unsigned int& i) const ;
      //! \brief return synthetic data convolved with stf for pair \c i
      Tseries convolvedseries(const unsigned int& i) const ;
      //@}
        
    protected:
      /*! \name Functions presented to derived classes
       */
      //@{
      //! \brief return the value of a parameters
      std::string parameter(const std::string& key,
                            const std::string& defvalue="false") const;
      //! \brief check is parameter was set by user
      bool parameterisset(const std::string& key) const;
      //! \brief check for vaid receiver index
      void checkreceiverindex(const unsigned int& i) const;
      //! \brief check for vaid index off additional time series pair
      void checkseriesindex(const unsigned int& i) const;
      //! \brief Virtual function to run engine
      virtual void exec() { STFINV_baseillegal; }
      //! \brief return weight for signal at receiver i
      double weight(const unsigned int& i) const 
      { return(Mweights(i)); }
      //! \brief return weights array
      aff::Series<double> weights() const 
      { return(Mweights); }
      //@}

    private:
      /*! \name Functions implementing base class operation
       */
      //@{
      //! \brief initialize base class
      void initialize(const std::string& parameters);
      //! \brief parse parameters and store them in Mparamap
      void parseparameters(std::string parameters);
      //! \brief Check consistency of data members.
      void checkconsistency() const;
      //! \brief Set weights according to exponent
      void setweights(const double& k);
      //@}
      
    // member data
    // -----------

    protected:
      // protected members are accessed directly from derived classes
      //! \brief Waveform triples.
      stfinv::Tvectoroftriples Mtriples;
      //! \brief source correction filter.
      stfinv::Waveform Mstf;
      //! \brief Waveform pairs.
      stfinv::Tvectorofpairs Mpairs;
      //! \brief debug level
      int Mdebug;
      //! \brief verbose level
      int Mverbose;
    private:
      /*! \brief Parameter map
       * \note must be declared mutable, since std::map does not provide a
       * const read function.
       */
      mutable stfinv::tools::Tparamap Mparamap;
      //! \brief Weights
      aff::Series<double> Mweights;
  }; // class STFBaseEngine

} // namespace stfinv

#endif // STFINV_STFINVBASE_H_VERSION (includeguard)

/* ----- END OF stfinvbase.h ----- */
