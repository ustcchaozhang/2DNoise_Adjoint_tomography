/*! \file globalsettings.cc
 * \brief class to hold and provide global settings (implementation)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/08/2008
 * 
 * class to hold and provide global settings (implementation)
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
 *  - 08/08/2008   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define STUPLO_GLOBALSETTINGS_CC_VERSION \
  "STUPLO_GLOBALSETTINGS_CC   V1.0   "

#include "globalsettings.h"

namespace stuplo {

  GlobalSettings::GlobalSettings():
    Mverbose(false),
    Mdebug(false),
    Minteractive(false),
    Mrepeatmode(false),
    Mrepeatinterval(60),
    Mdevice("/xserve"),
    Mhdevice("pgp.ps/cps"),
    Mrepcmd(""),
    Mhcommand(""),
    Mlwts(1),
    Mchts(1.),
    Mlwtg(1),
    Mlwpb(1),
    Mchy(1.),
    Mlwyg(1),
    Mlwys(1),
    Mspp(0.02),
    Mtstitle(0.),
    Mnpanels(1),
    Mlabu(false),
    Mlabc(false),
    Mlabe(false),
    Mlabr(false),
    Mlabh(0.05),
    Mtitle(""),
    Mdatesep(".")
    { } // GlobalSettings::Globalsettings()

  /*----------------------------------------------------------------------*/
  // set mode functions
  // ==================
  //! set command line option 1: verbose mode
  void GlobalSettings::set_verbose(const bool& f)
  { Mverbose=f; }
  //! set command line option 2: debug mode
  void GlobalSettings::set_debug(const bool& f)
  { Mdebug=f; }
  //! set command line option 3: interactive mode
  void GlobalSettings::set_interactive(const bool& f)
  { Minteractive=f; }
  //! set command line option 4: name of primary plot device 
  void GlobalSettings::set_device(const std::string& s)
  { Mdevice=s; }
  //! set command line option 5: name of hardcopy device
  void GlobalSettings::set_hdevice(const std::string& s)
  { Mhdevice=s; }
  //! set command line option 6: line width for time scale
  void GlobalSettings::set_lwts(const int& n)
  { Mlwts=n; }
  //! set command line option 7: character height for time scale
  void GlobalSettings::set_chts(const double &v)
  { Mchts=v; }
  //! set command line option 8: line width for time scale grid lines
  void GlobalSettings::set_lwtg(const int& n)
  { Mlwtg=n; }
  //! set command line option 9: panel separation
  void GlobalSettings::set_spp(const double &v)
  { Mspp=v; }
  //! set command line option 10: line width of panel boxes
  void GlobalSettings::set_lwpb(const int& n)
  { Mlwpb=n; }
  //! set command line option 11: character height for ordinate scale
  void GlobalSettings::set_chy(const double &v)
  { Mchy=v; }
  //! set command line option 12: line width for ordinate scale grid lines
  void GlobalSettings::set_lwyg(const int& n)
  { Mlwyg=n; }
  //! set command line option 13: line width for ordinate scale 
  void GlobalSettings::set_lwys(const int& n)
  { Mlwys=n; }
  //! set command line option 14: repeat mode 
  void GlobalSettings::set_repeat(const int& n)
  { 
    Mrepeatinterval=n; 
    Mrepeatmode=true; 
  }
  //! set command line option 15: repeat command 
  void GlobalSettings::set_repcmd(const std::string& s)
  { Mrepcmd=s; }
  //! set command line option 16: set title for plot
  void GlobalSettings::set_title(const std::string& s)
  { Mtitle=s; }
  //! set command line option 17: set title for plot
  void GlobalSettings::set_tstitle(const double &v)
  { Mtstitle=v; }
  //! set command line option 18: underline graph label
  void GlobalSettings::set_labu(const bool& f)
  { Mlabu=f; }
  //! set command line option 19: draw graph label with appropriate colour
  void GlobalSettings::set_labc(const bool& f)
  { Mlabc=f; }
  //! set command line option 20: issue command after hardcopy
  void GlobalSettings::set_hcommand(const std::string& s)
  { Mhcommand=s; }
  //! set command line option 21: erase label box prior to plotting
  void GlobalSettings::set_labe(const bool& f)
  { Mlabe=f; }
  //! set command line option 22: erase label box prior to plotting
  void GlobalSettings::set_npanels(const int& n)
  { Mnpanels=n; }
  //! set command line option 24: set label height relative to panel height
  void GlobalSettings::set_labh(const double &v)
  { Mlabh=v; }
  //! set command line option 25: reserve space for graph label
  void GlobalSettings::set_labr(const bool& f)
  { Mlabr=f; }
  //! set command line option 26: separator for date values
  void GlobalSettings::set_datesep(const std::string& s)
  { Mdatesep=s; }
  //! set command line option 27: distance between box and ordinate label
  void GlobalSettings::set_lyd(const double& d)
  { Mlyd=d; }
    
  /*----------------------------------------------------------------------*/

  // provide a global variable
  stuplo::GlobalSettings globalsettings;

} // namespace stuplo

/* ----- END OF globalsettings.cc ----- */
