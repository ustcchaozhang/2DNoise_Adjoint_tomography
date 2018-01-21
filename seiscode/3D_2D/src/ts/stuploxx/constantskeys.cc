/*! \file constantskeys.cc
 * \brief command line keys (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/02/2008
 * 
 * command line keys (implementation)
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
 * 
 * REVISIONS and CHANGES 
 *  - 13/02/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STUPLO_CONSTANTSKEYS_CC_VERSION \
  "STUPLO_CONSTANTSKEYS_CC   V1.0   "

#include "constants.h"

namespace stuplo {

  //! key to select traces
  const char* const tracekey="t";
  //! key to select panel
  const char* const panelkey="p";
  //! key to select color index
  const char* const colorindexkey="ci";
  //! key to select line style
  const char* const linestylekey="ls";
  //! key to select line width
  const char* const linewidthkey="lw";
  //! key to select data label
  const char* const labelkey="n";
  //! key to select color in rgb mode
  const char* const setrgbkey="r";
  //! key to select color in hls mode
  const char* const sethlskey="h";
  //! key to select file format
  const char* const formatkey="f";
  //! key to set ordinate scale range
  const char* const scalerangekey="s";
  //! key to set ordinate scale range factor
  const char* const scalerangefactorkey="sf";
  //! key to set ordinate scale chart stepper width
  const char* const chartstepperkey="cs";
  //! key to set chart stepper hystheresis
  const char* const chartstepperhystkey="ch";
  //! key to set ordinate scale chart stepper width
  const char* const ordinatelabelkey="u";
  //! key to annotate graph (append)
  const char* const annotationkey="a";
  //! key to annotate graph (prepend)
  const char* const preannotationkey="A";

  //! list of keys for filename specific parameters
  const char* keys[]={
    //! select traces
    tracekey, 
    //! select panel
    panelkey, 
    //! select color index
    colorindexkey, 
    //! select line style
    linestylekey, 
    //! select line width
    linewidthkey, 
    //! select color in rgb mode
    setrgbkey, 
    //! select color in hls mode
    sethlskey, 
    //! select data label
    labelkey, 
    //! select file format
    formatkey, 
    //! select fixed ordinate range
    scalerangekey, 
    //! select scaled ordinate range
    scalerangefactorkey, 
    //! select chart stepper function
    chartstepperkey, 
    //! select chart stepper hystheresis
    chartstepperhystkey, 
    //! select ordinate label
    ordinatelabelkey, 
    //! key to annotate graph (append)
    annotationkey,
    //! key to annotate graph  (prepend)
    preannotationkey,
    0
  }; 

} // namespace stuplo

/* ----- END OF constantskeys.cc ----- */
