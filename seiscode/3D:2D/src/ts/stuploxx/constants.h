/*! \file constants.h
 * \brief constants (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/01/2008
 * 
 * constants (prototypes)
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

// include guard
#ifndef STUPLO_CONSTANTS_H_VERSION

#define STUPLO_CONSTANTS_H_VERSION \
  "STUPLO_CONSTANTS_H   V1.0   "

#include <tfxx/commandline.h>

namespace stuplo {

  /*----------------------------------------------------------------------*/
  // constants

  //! define commandline options
  extern tfxx::cmdline::Declare options[];

  /*----------------------------------------------------------------------*/

    //! key to select traces
    extern const char* const tracekey;
    //! key to select panel
    extern const char* const panelkey;
    //! key to select color index
    extern const char* const colorindexkey;
    //! key to select line style
    extern const char* const linestylekey;
    //! key to select line width
    extern const char* const linewidthkey;
    //! key to select data label
    extern const char* const labelkey;
    //! key to select color in rgb mode
    extern const char* const setrgbkey;
    //! key to select color in hls mode
    extern const char* const sethlskey;
    //! key to select file format
    extern const char* const formatkey;
    //! key to set ordinate scale range
    extern const char* const scalerangekey;
    //! key to set ordinate scale range factor
    extern const char* const scalerangefactorkey;
    //! key to set ordinate scale chart stepper width
    extern const char* const chartstepperkey;
    //! key to set hystheresis of chart stepper 
    extern const char* const chartstepperhystkey;
    //! key to set ordinate scale chart stepper width
    extern const char* const ordinatelabelkey;
    //! key to annotate graph (append text)
    extern const char* const annotationkey;
    //! key to annotate graph (prepend text)
    extern const char* const preannotationkey;

    //! array of keys to be passed to commandline class
    extern const char* keys[];

  /*----------------------------------------------------------------------*/
  
  //! pattern to be replaced by date of first sample
  extern const char* const datepattern;
  //! pattern to be replaced by time of first sample
  extern const char* const timepattern;
  //! pattern to be replaced by time of first sample including microsecond
  extern const char* const utimepattern;
  //! pattern to be replaced by station identifier
  extern const char* const stationpattern;
  //! pattern to be replaced by channel identifier
  extern const char* const channelpattern;
  //! pattern to be replaced by auxiliary identifier
  extern const char* const auxiliarypattern;
  //! pattern to be replaced by instrument identifier
  extern const char* const instrumentpattern;
  //! pattern to be replaced by filenumber
  extern const char* const filenumberpattern;
  //! pattern to be replaced by tracenumber
  extern const char* const tracenumberpattern;
  //! pattern to be replaced by file name
  extern const char* const filenamepattern;
  //! double-per-cent to be replaced by per-cent
  extern const char* const percentpattern;
    
  /*----------------------------------------------------------------------*/

    //! index of first panel in vector of panels
    extern const int Cfirstpanel;
    //! panel index to be used to indicate that no panel was selected
    //! on the command line
    extern const unsigned int Cnopanelselected;
    //! default for not specified string values
    extern const char* const Cstringnotspecified;

} // namespace stuplo

#endif // STUPLO_CONSTANTS_H_VERSION (includeguard)

/* ----- END OF constants.h ----- */
