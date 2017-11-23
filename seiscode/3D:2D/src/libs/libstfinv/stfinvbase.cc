/*! \file stfinvbase.cc
 * \brief C++ interface and abstract base class (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * C++ interface and abstract base class (implementation)
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
#define STFINV_STFINVBASE_CC_VERSION \
  "STFINV_STFINVBASE_CC   V1.2"

#include <sstream>
#include <cmath>
#include <stfinv/stfinvbase.h>
#include <stfinv/stfinvbase_summary_usage.h>
#include <stfinv/stfinvbase_description_usage.h>
#include <stfinv/debug.h>

namespace stfinv {

  /*!
   * Constructor stores all references to time series data passed by the
   * caller.
   */
  STFBaseEngine::STFBaseEngine(const stfinv::Tvectoroftriples& triples,
                               const stfinv::Waveform& stf,
                               const stfinv::Tvectorofpairs& pairs,
                               const std::string& parameters)
    : Mtriples(triples), Mstf(stf), Mpairs(pairs),
      Mweights(0,triples.size()-1)
  {
    this->initialize(parameters);
  } // STFBaseEngine::STFBaseEngine(const stfinv::Tvectoroftriples& triples,

  /*----------------------------------------------------------------------*/

  /*!
   * Constructor stores all references to time series data passed by the
   * caller.
   */
  STFBaseEngine::STFBaseEngine(const stfinv::Tvectoroftriples& triples,
                               const stfinv::Waveform& stf,
                               const std::string& parameters)
    : Mtriples(triples), Mstf(stf), Mweights(0,triples.size()-1)
  {
    Mpairs.clear();
    this->initialize(parameters);
  } // STFBaseEngine::STFBaseEngine(const stfinv::Tvectoroftriples& triples,

  /*----------------------------------------------------------------------*/

  /*!
   * -# The STFBaseEngine::initialize() function parses the parameter string.
   *    Parsed values are stored in Mparamap.
   * -# Extract settings for parameters:
   *     - DEBUG: set debug output level
   *     - verbose: activate verbose output
   *     - exp: set power-law exponent and store weights in member data Mweights
   * -# Check consistency of data
   * -# Align index ranges of aff::Series objects such that the first element
   *    (sample) has index zero
   */
  void STFBaseEngine::initialize(const std::string& parameters)
  {
    this->parseparameters(parameters);

    // extract parameters
    {
      std::istringstream is(this->parameter("DEBUG","0"));
      is >> Mdebug;
    }
    STFINV_debug(Mdebug>0, "STFBaseEngine::STFBaseEngine",
                 "initializing base class");
    Mverbose=(this->parameter("verbose","false")=="true");
    if (this->parameterisset("exp")) 
    {
      double k;
      std::istringstream is(this->parameter("exp","0."));
      is >> k;
      for (unsigned int i=0; i<Mtriples.size(); ++i)
      {
        double offset=Mtriples[i].offset();
        Mweights(i)=pow(offset,k);
      }
    } 
    else
    {
      Mweights=1.;
    }

    this->checkconsistency();

    // align index ranges; first index will be zero
    if (Mstf.series.first()!=0) { Mstf.series.shift(-Mstf.series.first()); }

    {
      stfinv::Tvectoroftriples::iterator I=Mtriples.begin();
      while (I!=Mtriples.end())
      {
        if (I->data.first()!=0) { I->data.shift(-(I->data.first())); }
        if (I->synthetics.first()!=0) 
        { I->synthetics.shift(-(I->synthetics.first())); }
        if (I->convolvedsynthetics.first()!=0) 
        { I->convolvedsynthetics.shift(-(I->convolvedsynthetics.first())); }
        ++I;
      } // while (I!=Mtriples.end())
    }

    {
      stfinv::Tvectorofpairs::iterator I=Mpairs.begin();
      while (I!=Mpairs.end())
      {
        if (I->synthetics.first()!=0) 
        { I->synthetics.shift(-(I->synthetics.first())); }
        if (I->convolvedsynthetics.first()!=0) 
        { I->convolvedsynthetics.shift(-(I->convolvedsynthetics.first())); }
        ++I;
      } // while (I!=Mpairs.end())
    }
  } // void STFBaseEngine::initialize(const std::string& parameters)

  /*----------------------------------------------------------------------*/

  /*!
   * Check consistency of time series data passed to the STFBaseEngine
   * with respect to
   *  -# number of samples
   *  -# sampling interval
   *
   */
  void STFBaseEngine::checkconsistency() const
  {
    const unsigned int& n=Mstf.sampling.n;
    const double& dt=Mstf.sampling.dt;
    STFINV_assert(Mstf.sampling.n==Mstf.series.size(),
                  "inconsistent number of samples");
    const double tolerance=1.e-4;

    {
      stfinv::Tvectoroftriples::const_iterator I=Mtriples.begin();
      while (I!=Mtriples.end())
      {
        CTripleHeader header=I->header;
        STFINV_assert(header.sampling.n==n,
                      "inconsistent number of samples");
        STFINV_assert(header.sampling.n==I->data.size(),
                      "inconsistent number of samples");
        STFINV_assert(header.sampling.n==I->synthetics.size(),
                      "inconsistent number of samples");
        STFINV_assert(header.sampling.n==I->convolvedsynthetics.size(),
                      "inconsistent number of samples");
        STFINV_assert(std::abs(1.-header.sampling.dt/dt)<tolerance,
                      "inconsistent values of sampling interval");
        ++I;
      } // while (I!=Mtriples.end())
    }

    {
      stfinv::Tvectorofpairs::const_iterator I=Mpairs.begin();
      while (I!=Mpairs.end())
      {
        CWaveformHeader sampling=I->sampling;
        STFINV_assert(sampling.n==n,
                      "inconsistent number of samples");
        STFINV_assert(sampling.n==I->synthetics.size(),
                      "inconsistent number of samples");
        STFINV_assert(sampling.n==I->convolvedsynthetics.size(),
                      "inconsistent number of samples");
        STFINV_assert(std::abs(1.-sampling.dt/dt)<tolerance,
                      "inconsistent values of sampling interval");
        ++I;
      } // while (I!=Mpairs.end())
    }
  }

  /*----------------------------------------------------------------------*/

  void STFBaseEngine::help(std::ostream& os) const
  {
    STFBaseEngine::classhelp(os);
  } // void STFBaseEngine::help(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  void STFBaseEngine::classhelp(std::ostream& os) 
  {
    os << stfinvbase_summary_usage;
  } // void STFBaseEngine::classhelp(std::ostream& os)

  /*----------------------------------------------------------------------*/

  void STFBaseEngine::usage(std::ostream& os) const
  {
    STFBaseEngine::classusage(os);
  } // void STFBaseEngine::usage(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  void STFBaseEngine::classusage(std::ostream& os) 
  {
    os << stfinvbase_description_usage;
  } // void STFBaseEngine::classusage(std::ostream& os)

  /*----------------------------------------------------------------------*/

  /*!
   * Pass the parameter string (as usually passed along the chain of
   * constructors to the constructor and initialize() function of
   * STFBaseEngine).
   * The values are stored in the member data
   * STFBaseEngine::Mparamap
   */
  void STFBaseEngine::parseparameters(std::string parameters) 
  {
    while (parameters.length()>0)
    {
      std::string value=stfinv::tools::clipstring(parameters,":");
      std::string key
        =stfinv::tools::trimws(stfinv::tools::clipstring(value,"="));
      if (value.length()==0)
      {
        this->Mparamap[key]="true";
      }
      else
      {
        this->Mparamap[key]=value;
      }
    } // while (parameters.length()>0)
  } // void STFBaseEngine::parseparameters(std::string parameters)

  /*----------------------------------------------------------------------*/
  
  /*! \brief check is parameter was set
   *
   * \param key parameter key
   * \return true, if parameter was set by user
   */
  bool STFBaseEngine::parameterisset(const std::string& key) const
  {
    return (Mparamap.count(key)>0);
  } // bool STFBaseEngine::parameter(const std::string& key) const

  /*----------------------------------------------------------------------*/
  
  /*! \brief return the value of a parameters
   *
   * \param key parameter key
   * \param defvalue default value to be used is not set externally
   * \return parameter value or default value if key is not present.
   */
  std::string STFBaseEngine::parameter(const std::string& key,
                                       const std::string& defvalue) const
  {
    std::string retval;
    if (this->parameterisset(key))
    {
      retval=Mparamap[key];
    }
    else
    {
      retval=defvalue;
    }
    return retval;
  } // std::string STFBaseEngine::parameter()

  /*----------------------------------------------------------------------*/

  const char* STFBaseEngine::name() const
  {
    return("STFBaseEngine");
  } //  const char const* STFBaseEngine::name() const

  /*----------------------------------------------------------------------*/

  Tseries::Tcoc STFBaseEngine::recording(const unsigned int& i) const 
  {
    this->checkreceiverindex(i);
    return(Mtriples[i].data);
  }

  /*----------------------------------------------------------------------*/

  Tseries::Tcoc STFBaseEngine::synthetic(const unsigned int& i) const 
  {
    this->checkreceiverindex(i);
    return(Mtriples[i].synthetics);
  }

  /*----------------------------------------------------------------------*/

  Tseries STFBaseEngine::convolvedsynthetic(const unsigned int& i) const 
  {
    this->checkreceiverindex(i);
    return(Mtriples[i].convolvedsynthetics);
  }

  /*----------------------------------------------------------------------*/

  Tseries::Tcoc STFBaseEngine::series(const unsigned int& i) const 
  {
    this->checkseriesindex(i);
    return(Mpairs[i].synthetics);
  }

  /*----------------------------------------------------------------------*/

  Tseries STFBaseEngine::convolvedseries(const unsigned int& i) const 
  {
    this->checkseriesindex(i);
    return(Mpairs[i].convolvedsynthetics);
  }

  /*----------------------------------------------------------------------*/

  void STFBaseEngine::checkreceiverindex(const unsigned int& i) const 
  {
    STFINV_assert(i<Mtriples.size(),
                  "ERROR: receiver index out of range");
  }

  /*----------------------------------------------------------------------*/

  void STFBaseEngine::checkseriesindex(const unsigned int& i) const 
  {
    STFINV_assert(i<Mpairs.size(),
                  "ERROR: series index out of range");
  }

  /*======================================================================*/

  double WaveformTriple::offset() const
  {
    double dx=header.rx-header.sx;
    double dy=header.ry-header.sy;
    return (sqrt(dx*dx+dy*dy));
  } // double WaveformTriple::offset() const

} // namespace stfinv

/* ----- END OF stfinvbase.cc ----- */
