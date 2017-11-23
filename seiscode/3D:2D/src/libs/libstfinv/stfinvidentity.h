/*! \file stfinvidentity.h
 * \brief just find a scaling factor (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/05/2011
 * 
 * just find a scaling factor (prototypes)
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
 *  - 07/05/2011   V1.0   Thomas Forbriger
 *  - 14/10/2015   V1.1   new end-user usage functions
 * 
 * ============================================================================
 */

// include guard
#ifndef STFINV_STFINVIDENTITY_H_VERSION

#define STFINV_STFINVIDENTITY_H_VERSION \
  "STFINV_STFINVIDENTITY_H   V1.1"

#include<stfinv/stfinvbase.h>

namespace stfinv {

  /*! \brief Engine to apply a scalar factor
   * \ingroup group_engines
   *
   * \par Concept behin this engine
   * This engine convolves the synthetic data with a discrete delta pulse so
   * to speak.
   * Optionally the delta-peak ist scale such that the convolved synthetics
   * will be of equal scaled energy as the recordings.
   */
  class STFEngineIdentity: public stfinv::STFBaseEngine {
    public:
      //! \brief typedef to refer to base class
      typedef stfinv::STFBaseEngine Tbase;
      //! \brief ID used to select thsi engine
      static const char* const ID;
      //! \brief short description of this engine
      static const char* const description;
      /*! \brief Constructor.
       */
      STFEngineIdentity(const stfinv::Tvectoroftriples& triples,
                        const stfinv::Waveform& stf,
                        const std::string& parameters)
        : Tbase(triples, stf, parameters),
        Mscaleenergy(false)
      { this->initialize(); }
      /*! \brief Constructor.
       */
      STFEngineIdentity(const stfinv::Tvectoroftriples& triples,
                        const stfinv::Waveform& stf,
                        const stfinv::Tvectorofpairs& pairs,
                        const std::string& parameters)
        : Tbase(triples, stf, pairs, parameters), 
        Mscaleenergy(false)
      { this->initialize(); }
      //! \brief abstract base requires virtual destructor
      virtual ~STFEngineIdentity() { }
      //! \brief Start engine 
      virtual void exec();
      //! \brief print online help
      virtual void help(std::ostream& os=std::cout) const;
      //! \brief print online help
      static void classhelp(std::ostream& os=std::cout);
      //! \brief print detailed description
      virtual void usage(std::ostream& os=std::cout) const;
      //! \brief print detailed description
      static void classusage(std::ostream& os=std::cout);
      //! \brief return name of engine
      virtual const char* name() const;
    private:
      //! \brief initialize work space
      void initialize();

      // member data
    private:
      //! \brief scale energy
      bool Mscaleenergy;
  }; // class STFEngineIdentity

} // namespace stfinv

#endif // STFINV_STFINVIDENTITY_H_VERSION (includeguard)

/* ----- END OF stfinvidentity.h ----- */
