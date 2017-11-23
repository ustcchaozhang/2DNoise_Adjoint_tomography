/*! \file stfinvfixedstf.h
 * \brief always return a fixed stf as read from file (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * always return a fixed stf as read from file (prototypes)
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
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_STFINVFIXEDSTF_H_VERSION

#define STFINV_STFINVFIXEDSTF_H_VERSION \
  "STFINV_STFINVFIXEDSTF_H   V1.0"

#include<stfinv/stfinvfourier.h>

namespace stfinv {

  /*! \brief Engine to provide a fixed wavelet
   * \ingroup group_engines
   */
  class STFEngineFixedWavelet: public stfinv::STFFourierDomainEngine {
    public:
      //! \brief typedef to refer to base class
      typedef stfinv::STFFourierDomainEngine Tbase;
      //! \brief ID used to select thsi engine
      static const char* const ID;
      //! \brief short description of this engine
      static const char* const description;
      /*! \brief Constructor.
       */
      STFEngineFixedWavelet(const stfinv::Tvectoroftriples& triples,
                            const stfinv::Waveform& stf,
                            const std::string& parameters)
        :Tbase(triples, stf, parameters) { }
      //! \brief abstract base requires virtual destructor
      virtual ~STFEngineFixedWavelet() { }
      //! \brief Start engine 
      virtual void exec();
      //! \brief print online help
      virtual void help(std::ostream& os=std::cout) const;
      //! \brief print online help
      static void classhelp(std::ostream& os=std::cout);
      //! \brief return name of engine
      virtual const char* name() const;
    protected:
      //! \brief initialize work space
      void initialize() { }
  }; // class STFEngineFixedWavelet

} // namespace stfinv

#endif // STFINV_STFINVFIXEDSTF_H_VERSION (includeguard)

/* ----- END OF stfinvfixedstf.h ----- */
