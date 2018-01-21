/*! \file srcesynref.cc
 * \brief provide time reference for synthetic waveforms (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/01/2005
 * 
 * provide time reference for synthetic waveforms (implementation)
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 17/01/2005   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TF_SRCESYNREF_CC_VERSION \
  "TF_SRCESYNREF_CC   V1.0   "

#include <sffxx.h>

namespace sff {

  //! return ID string for synthtic time reference
  std::string srce_reference_ID()
  {
    return(std::string("synthetic.reference"));
  } // std::string srce_reference_ID()

  /*----------------------------------------------------------------------*/

  /*! \brief return synthetic time reference from nothing
   */
  sff::SRCE srce_reference()
  {
    sff::SRCE result;
    result.type=srce_reference_ID();
    result.date=libtime::now();
    return(result);
  } // sff::SRCE srce_reference()

} // namespace sff

/* ----- END OF srcesynref.cc ----- */
