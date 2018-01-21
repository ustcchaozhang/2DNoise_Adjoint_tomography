/*! \file stfinvfixedstf.cc
 * \brief always return a fixed stf as read from file (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * always return a fixed stf as read from file (implementation)
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
#define STFINV_STFINVFIXEDSTF_CC_VERSION \
  "STFINV_STFINVFIXEDSTF_CC   V1.0"

#include <stfinv/stfinvfixedstf.h>

namespace stfinv {

  const char* const STFEngineFixedWavelet::ID="fix";

  const char* const STFEngineFixedWavelet::description
    ="use fixed wavelet as read from file";

  /*----------------------------------------------------------------------*/

  void STFEngineFixedWavelet::exec() 
  {
    STFINV_abort("STFEngineFixedWavelet::run not yet implemented");
  } // void STFEngineFixedWavelet::exec() 

  /*----------------------------------------------------------------------*/

  void STFEngineFixedWavelet::help(std::ostream& os) const
  {
    STFEngineFixedWavelet::classhelp(os);
  } // void STFEngineFixedWavelet::help(std::ostream& os) const

  /*----------------------------------------------------------------------*/

  void STFEngineFixedWavelet::classhelp(std::ostream& os)
  {
    os << "class STFEngineFixedWavelet (" << STFEngineFixedWavelet::ID << ")\n";
    os << STFEngineFixedWavelet::description << "\n" << std::endl;
    os << "This class is not yet implemented." << std::endl;
    os << "Online help is not yet provided." << std::endl;
    Tbase::classhelp(os);
  } // void STFEngineFixedWavelet::classhelp(std::ostream& os)

  /*----------------------------------------------------------------------*/

  const char* STFEngineFixedWavelet::name() const
  {
    return("STFEngineFixedWavelet");
  } //  const char const* STFEngineFixedWavelet::name() const

} // namespace stfinv

/* ----- END OF stfinvfixedstf.cc ----- */
