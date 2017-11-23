/*! \file stfinvany.h
 * \brief a wrapper to any STF engine in the library (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * a wrapper to any STF engine in the library (prototypes)
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
#ifndef STFINV_STFINVANY_H_VERSION

#define STFINV_STFINVANY_H_VERSION \
  "STFINV_STFINVANY_H   V1.2"

#include<stfinv/stfinvbase.h>

namespace stfinv {

  /*! \brief Class to access any engine in the library
   *
   * \ingroup cxxinterface
   *
   * The interface of this class is identical to the interface defined by the
   * abstract base class stfinv::STFBaseEngine except that the first prefix
   * (up to the first colon) in the parameter string is understood to identify
   * the selected engine.
   *
   * It serves as a kind of handle to provide user selectable engines to
   * application programs.
   */
  class STFEngine {
    public:
      /*! \brief Constructor.
       *
       * \param triples
       *   This function expects a vector of triples containing references to
       *   the users workspace for recorded time series as well as synthetic
       *   time series. These will be used as input.
       *   As a third set a reference to a workspace for synthetic time series
       *   convolved with the source correction filter is expected. 
       *   The latter will be used as output.
       * \param stf
       *   The Waveform presents a reference to the users work space
       *   for the source correction filter time series.
       *   It will be used to present the result of the processing to the user.
       * \param parameters
       *   Parameters to select one of the engines as well as to control the
       *   engines are passed in a character sequence.
       *   See also \ref page_eu_subsec_parameters
       */
      STFEngine(const stfinv::Tvectoroftriples& triples,
                const stfinv::Waveform& stf,
                const std::string& parameters);
      /*! \brief Constructor.
       *
       * \param triples
       *   This function expects a vector of triples containing references to
       *   the users workspace for recorded time series as well as synthetic
       *   time series. These will be used as input.
       *   As a third set a reference to a workspace for synthetic time series
       *   convolved with the source correction filter is expected. 
       *   The latter will be used as output.
       * \param stf
       *   The Waveform presents a reference to the users work space
       *   for the source correction filter time series.
       *   It will be used to present the result of the processing to the user.
       * \param pairs
       *   The vector of pairs presents a reference to the users work space for
       *   additional synthetic time series. These time series will not be
       *   used to determine the optimal source correction filter, but will
       *   be convolved with the obtained source correction filter on the fly.
       *   This is useful in particular with forward modelling code which
       *   uses a band limited source wavelet for the initial synthetics
       *   already. This source time function can be passed through this
       *   argument and will then be convolved with the optimized source
       *   correction filter, such that the result of the convolution is
       *   appropriate to obtain synthetics which provide a reduced misift
       *   with respect to the data.
       * \param parameters
       *   Parameters to select one of the engines as well as to control the
       *   engines are passed in a character sequence.
       *   See also \ref page_eu_subsec_parameters
       */
      STFEngine(const stfinv::Tvectoroftriples& triples,
                const stfinv::Waveform& stf,
                const stfinv::Tvectorofpairs& pairs,
                const std::string& parameters);
      //! \brief Destructor must remove engine.
      ~STFEngine();
      //! \brief Return actual engine.
      stfinv::STFBaseEngine& STFBaseEngine() { return (*Mengine); }
      //! \brief Start engine and return source correction filter.
      stfinv::Waveform run() { return(Mengine->run()); }
      //! \brief List procedures (engines) currently recognized
      static void engines(std::ostream& os=std::cout);
      //! \brief List engines currently recognized and print summary
      static void help(std::ostream& os=std::cout);
      /*! \brief Print detailed usage description.
       *
       * \param id ID of procedure (engine) to be described.
       * \param os stream to send output to
       */
      static void usage(const std::string& id, std::ostream& os=std::cout);
    private:
      //! \brief initialize engine.
      void initialize(const stfinv::Tvectoroftriples& triples,
                      const stfinv::Waveform& stf,
                      const stfinv::Tvectorofpairs& pairs,
                      const std::string& parameters); 
      //! \brief Pointer to actual engine.
      stfinv::STFBaseEngine* Mengine;
  }; // class STFEngine

  /*----------------------------------------------------------------------*/

  /*! print list available procedures (engines)
   * \ingroup cxxinterface
   *
   * Just delegates to STFEngine::engines()
   */
  void engines(std::ostream& os=std::cout);

  /*! print print usage summary
   * \ingroup cxxinterface
   *
   * Just delegates to STFEngine::help()
   */
  void help(std::ostream& os=std::cout);

  /*! print print detailed description for selected engine
   * \ingroup cxxinterface
   *
   * Just delegates to STFEngine::usage()
   */
  void usage(const std::string& id, std::ostream& os=std::cout);

} // namespace stfinv

#endif // STFINV_STFINVANY_H_VERSION (includeguard)

/* ----- END OF stfinvany.h ----- */
