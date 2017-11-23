/*! \file globalsettings.h
 * \brief class to hold and provide global settings (prototypes)
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 08/08/2008
 * 
 * class to hold and provide global settings (prototypes)
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
 *  - 17/03/2015   V1.1   adjust libpgplotxx interface
 * 
 * ============================================================================
 */

// include guard
#ifndef STUPLO_GLOBALSETTINGS_H_VERSION

#define STUPLO_GLOBALSETTINGS_H_VERSION \
  "STUPLO_GLOBALSETTINGS_H   V1.1   (17-03-2015)"

#include <string>
#include <pgplotxx/xpgplotxx.h>

namespace stuplo {

  /*! \brief global settings
   *
   * A global instance of this class will be available to obtain default
   * values and style parameters. The class offers a set-interface, which will
   * be controlled by commandline options. And it provides a get-interface
   * that answers specific questions. The link between both is internal to the
   * structure.
   */
  class GlobalSettings {
    public:
      //! default constructor sets default values
      GlobalSettings();
      //! set command line option 1: verbose mode
      void set_verbose(const bool& f);
      //! set command line option 2: debug mode
      void set_debug(const bool& f);
      //! set command line option 3: interactive mode
      void set_interactive(const bool& f);
      //! set command line option 4: name of primary output device 
      void set_device(const std::string& s);
      //! set command line option 5: name of hardcopy device
      void set_hdevice(const std::string& s);
      //! set command line option 6: line width for time scale
      void set_lwts(const int& n);
      //! set command line option 7: character height for time scale
      void set_chts(const double& v);
      //! set command line option 8: line width for time scale grid lines
      void set_lwtg(const int& n);
      //! set command line option 9: panel separation
      void set_spp(const double& v);
      //! set command line option 10: line width of panel boxes
      void set_lwpb(const int& n);
      //! set command line option 11: character height for ordinate scale
      void set_chy(const double& v);
      //! set command line option 12: line width for ordinate scale grid lines
      void set_lwyg(const int& n);
      //! set command line option 13: line width for ordinate scale 
      void set_lwys(const int& n);
      //! set command line option 14: repeat mode 
      void set_repeat(const int& n);
      //! set command line option 15: repeat command 
      void set_repcmd(const std::string& s);
      //! set command line option 16: set title for plot
      void set_title(const std::string& s);
      //! set command line option 17: set title for plot
      void set_tstitle(const double& v);
      //! set command line option 18: underline graph label
      void set_labu(const bool& f);
      //! set command line option 19: draw graph label with appropriate colour
      void set_labc(const bool& f);
      //! set command line option 20: issue command after hardcopy
      void set_hcommand(const std::string& s);
      //! set command line option 21: erase label box prior to plotting
      void set_labe(const bool& f);
      //! set command line option 22: erase label box prior to plotting
      void set_npanels(const int& n);
      //! set command line option 24: set label height relative to panel height
      void set_labh(const double& v);
      //! set command line option 25: reserve space for graph label
      void set_labr(const bool& f);
      //! set command line option 26: separator for date values
      void set_datesep(const std::string& s);
      //! set command line option 27: distance between box and ordinate label
      void set_lyd(const double& d);
      /*----------------------------------------------------------------------*/
      // read functions
      // --------------
      double lyd() const { return Mlyd; }
      double chy() const { return Mchy; }
    private:
      /*! mode flags
       *  ==========
       */
      //! be verbose
      bool Mverbose;
      //! provide debug output
      bool Mdebug;
      //! interactive mode
      bool Minteractive;
      //! repeat mode
      bool Mrepeatmode;
      //! repeat interval
      int Mrepeatinterval;

      /*! device and command strings
       *  ==========================
       */
      //! screen plot device
      std::string Mdevice;
      //! hardcopy plot device
      std::string Mhdevice;
      //! repeat mode command
      std::string Mrepcmd;
      //! hardcopy command
      std::string Mhcommand;

      /*! grid and frame style
       *  ====================
       */
      //! line width for time scale
      int Mlwts;
      //! character height for time scale
      double Mchts;
      //! line width for time scale grid
      double Mlwtg;
      //! line width of panel boxes
      int Mlwpb;
      //! character height for ordinate scale
      double Mchy;
      //! distance of ordinate unit label from edge of graph box
      double Mlyd;
      //! line width for ordinate scale grid lines
      int Mlwyg;
      //! line with for ordinate scale
      int Mlwys;
      
      /*! plot layout
       *  ===========
       */
      //! panel separation
      double Mspp;
      //! plot title height
      double Mtstitle;
      //! number of plot panels to prepare
      int Mnpanels;
      //! underline graph label
      bool Mlabu;
      //! draw graph label with appropriate color
      bool Mlabc;
      //! erase label box prior to plotting
      bool Mlabe;
      //! reserve space for graph label
      bool Mlabr;
      //! label height relative to viewport
      double Mlabh;

      /*! plot title and annotations
       *  ==========================
       */
      //! plot title string
      std::string Mtitle;
      //! separator for date values
      std::string Mdatesep;
  }; // class GlobalSettings

  /*----------------------------------------------------------------------*/

  /*! global instance of global settings
   *
   * This variable is presented globally. 
   * It is save to do so since it contains settings that are defined by
   * command line options, which can only be set once per instance of
   * stuploxx.
   */
  extern GlobalSettings globalsettings;

} // namespace stuplo

#endif // STUPLO_GLOBALSETTINGS_H_VERSION (includeguard)

/* ----- END OF globalsettings.h ----- */
