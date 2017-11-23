/*! \file stfinvany.cc
 * \brief a wrapper to any STF engine in the library (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * a wrapper to any STF engine in the library (implementation)
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
 *  - 04/10/2011   V1.2   renamed Fourier domain least squares engine
 *  - 14/10/2015   V1.3   new end-user usage functions
 * 
 * ============================================================================
 */
#define STFINV_STFINVANY_CC_VERSION \
  "STFINV_STFINVANY_CC   V1.3"

#include <stfinv/stfinvany.h>
#include <stfinv/stfinvfdleastsquares.h>
#include <stfinv/stfinvfixedstf.h>
#include <stfinv/stfinvidentity.h>
#include <stfinv/parameterhandler.h>
#include <stfinv/stfinvany_summary_usage.h>
#include <stfinv/stfinvany_description_usage.h>
#include <stfinv/stfinv_summary_usage.h>
#include <stfinv/stfinv_description_usage.h>
#include <stfinv/error.h>
#include <stfinv/tools.h>

namespace stfinv {
  
  //! \brief Constructor.
  STFEngine::STFEngine(const stfinv::Tvectoroftriples& triples,
                       const stfinv::Waveform& stf,
                       const std::string& parameters)
  {
    stfinv::Tvectorofpairs pairs;
    pairs.clear();
    this->initialize(triples, stf, pairs, parameters);
  }

  /*----------------------------------------------------------------------*/
  //! \brief Constructor.
  STFEngine::STFEngine(const stfinv::Tvectoroftriples& triples,
                       const stfinv::Waveform& stf,
                       const stfinv::Tvectorofpairs& pairs,
                       const std::string& parameters)
  {
    this->initialize(triples, stf, pairs, parameters);
  }

  /*----------------------------------------------------------------------*/

  void STFEngine::initialize(const stfinv::Tvectoroftriples& triples,
                             const stfinv::Waveform& stf,
                             const stfinv::Tvectorofpairs& pairs,
                             const std::string& parameters)
  {
    std::string para=parameters;
    std::string id=stfinv::tools::clipstring(para, ":");
    if (id == std::string(stfinv::STFEngineIdentity::ID))
    {
      Mengine=new stfinv::STFEngineIdentity(triples, stf, para);
    }
    /*
    else if (id == std::string(stfinv::STFEngineFixedWavelet::ID))
    {
      STFINV_assert(pairs.size()==0,
              "ERROR: engine does not support additional time series pairs");
      Mengine=new stfinv::STFEngineFixedWavelet(triples, stf, para);
    }
    */
    else if ((id == std::string(stfinv::STFEngineFDLeastSquares::ID))
             || (id == std::string("fbd")))
    {
      STFINV_report_assert((id == std::string(stfinv::STFEngineFDLeastSquares::ID)),
                           "The ID \"fbd\" for this engine is deprecated",
                           "The correct ID of the Fourier domain least "
                           "squares engine is \"" <<
                           stfinv::STFEngineFDLeastSquares::ID <<
                           "\".\n"
                           "The former ID \"fbd\" may vanish in the future "
                           "and should no longer be used.");
      if (pairs.size()>0)
      {
        Mengine=new stfinv::STFEngineFDLeastSquares(triples, stf, 
                                                        pairs, para);
      }
      else
      {
        Mengine=new stfinv::STFEngineFDLeastSquares(triples, stf, para);
      }
    }
    else
    {
      std::cerr << "ERROR: engine ID " << id << " is unkown!" << std::endl;
      STFINV_abort("aborting since engine ID is not recognized");
    }
    STFINV_assert(Mengine!=0, "engine was not created correctly");
  }

  /*----------------------------------------------------------------------*/

  STFEngine::~STFEngine() 
  {
    delete Mengine;
  } // STFEngine::~STFEngine()

  /*----------------------------------------------------------------------*/

  void STFEngine::engines(std::ostream& os)
  {
    os << "Available procedures:" << std::endl;
    os << "---------------------" << std::endl;
    tools::report_engine<STFEngineIdentity>(os);
    // tools::report_engine<STFEngineFixedWavelet>(os);
    tools::report_engine<STFEngineFDLeastSquares>(os);
  } // void STFEngine::help(std::ostream& os=std::cout)

  /*----------------------------------------------------------------------*/

  void STFEngine::help(std::ostream& os)
  {
    os << stfinv_summary_usage;
    os << std::endl;
    STFEngine::engines(os);
    os << std::endl;
    STFEngineIdentity::classhelp(os);
    os << std::endl;
    STFEngineFDLeastSquares::classhelp(os);
    os << std::endl;
    STFFourierDomainEngine::classhelp(os);
    os << std::endl;
    STFBaseEngine::classhelp(os);
    os << std::endl;
    os << stfinvany_summary_usage;
  } // void STFEngine::help(std::ostream& os=std::cout)

  /*----------------------------------------------------------------------*/

  void STFEngine::usage(const std::string& id, std::ostream& os)
  {
    os << stfinv_description_usage;
    os << std::endl;
    if (id == std::string(stfinv::STFEngineIdentity::ID))
    {
      stfinv::STFEngineIdentity::classusage(os);
    }
    else if ((id == std::string(stfinv::STFEngineFDLeastSquares::ID)))
    {
      stfinv::STFEngineFDLeastSquares::classusage(os);
    }
    else
    {
      std::cerr << "ERROR: engine ID " << id << " is unkown!" << std::endl;
      STFINV_abort("aborting since engine ID is not recognized");
    }
    os << std::endl;
    os << stfinvany_description_usage;
  } // void STFEngine::help(std::ostream& os=std::cout)

  /*======================================================================*/

  void engines(std::ostream& os)
  {
    STFEngine::engines(os);
  } // void engines(std::ostream& os)

  void help(std::ostream& os)
  {
    STFEngine::help(os);
  } // void help(std::ostream& os=std::cout)

  void usage(const std::string& id, std::ostream& os)
  {
    STFEngine::usage(id, os);
  } // void usage(const std::string& id, std::ostream& os)

} // namespace stfinv

/* ----- END OF stfinvany.cc ----- */
