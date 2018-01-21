/*! \file constantsoptions.cc
 * \brief command line options (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/02/2008
 * 
 * command line options (implementation)
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
#define STUPLO_CONSTANTSOPTIONS_CC_VERSION \
  "STUPLO_CONSTANTSOPTIONS_CC   V1.0   "

#include "constants.h"

namespace stuplo {

  // define commandline options
  using namespace tfxx::cmdline;
  Declare options[]= 
  {
    // 0: print help
    {"help",arg_opt,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: debug mode
    {"D",arg_no,"-"},
    // 3: interactive mode
    {"interactive",arg_no,"-"},
    // 4: name of primary plot device
    {"device",arg_yes,"/xserve"},
    // 5: name of hardcopy device
    {"hdevice",arg_yes,"pgp.ps/cps"},
    // 6: line width for time scale
    {"lwts",arg_yes,"1"},
    // 7: character height for time scale
    {"chts",arg_yes,"1."},
    // 8: line width for time scale grid lines
    {"lwtg",arg_yes,"1"},
    // 9: panel separation
    {"spp",arg_yes,"0.02"},
    // 10: line width of panel boxes
    {"lwpb",arg_yes,"1"},
    // 11: character height for ordinate scale
    {"chy",arg_yes,"1."},
    // 12: line width for ordinate scale grid lines
    {"lwyg",arg_yes,"1"},
    // 13: line width for ordinate scale 
    {"lwys",arg_yes,"1"},
    // 14: repeat mode 
    {"repeat",arg_opt,"60"},
    // 15: repeat command 
    {"repcmd",arg_yes,""},
    // 16: set title for plot
    {"title",arg_yes,""},
    // 17: title height for plot
    {"tstitle",arg_yes,"1."},
    // 18: underline graph label
    {"labu",arg_no,"-"},
    // 19: draw graph label with appropriate colour
    {"labc",arg_no,"-"},
    // 20: issue command after hardcopy
    {"hcommand",arg_yes,""},
    // 21: erase label box prior to plotting
    {"labe",arg_no,""},
    // 22: number of plot panels to be prepared
    {"npanels",arg_yes,"1"},
    // 23: print help text concerning interactive control keys
    {"keys",arg_no,""},
    // 24: set label height relative to panel height
    {"labh",arg_yes,"0.05"},
    // 25: reserve space for graph label
    {"labr",arg_no,"-"},
    // 26: separator for date values
    {"datesep",arg_no,"."},
    // 27: distance of ordinate unit label from graph box edge
    {"lyd",arg_yes,"2.2"},
    {NULL}
  };

} // namespace stuplo

/* ----- END OF constantsoptions.cc ----- */
