/*! \file constants.cc
 * \brief constants (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/01/2008
 * 
 * constants (implementation)
 * 
 * Copyright (c) 2008 by Thomas Forbriger (BFO Schiltach) 
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
 * REVISIONS and CHANGES 
 *  - 29/01/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STUPLO_CONSTANTS_CC_VERSION \
  "STUPLO_CONSTANTS_CC   V1.0   "

#include "constants.h"

namespace stuplo {

  /*----------------------------------------------------------------------*/
  // constants
  
  //! pattern to be replaced by date of first samples
  const char* const datepattern="%D";
  //! pattern to be replaced by time of first samples
  const char* const timepattern="%T";
  //! pattern to be replaced by time of first sample including microsecond
  const char* const utimepattern="%UT";
  //! pattern to be replaced by station identifier
  const char* const stationpattern="%S";
  //! pattern to be replaced by channel identifier
  const char* const channelpattern="%C";
  //! pattern to be replaced by auxiliary identifier
  const char* const auxiliarypattern="%A";
  //! pattern to be replaced by instrument identifier
  const char* const instrumentpattern="%I";
  //! pattern to be replaced by filenumber
  const char* const filenumberpattern="%NF";
  //! pattern to be replaced by tracenumber
  const char* const tracenumberpattern="%NT";
  //! pattern to be replaced by file name
  const char* const filenamepattern="%F";
  //! double-per-cent to be replaced by per-cent
  const char* const percentpattern="%%";
    
  /*----------------------------------------------------------------------*/

    // index of first panel in vector of panels
    const int Cfirstpanel=0;
    // panel index to be used to indicate that no panel was selected
    // on the command line
    const unsigned int Cnopanelselected=99999;

    const char* const Cstringnotspecified="NSP";

} // namespace stuplo

/* ----- END OF constants.cc ----- */
