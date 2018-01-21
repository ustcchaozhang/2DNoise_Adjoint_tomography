/*! \file stuploxx.cc
 * \brief Plot time series data
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 30/01/2007
 * 
 * Plot time series data
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
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 30/01/2007   V1.0   Thomas Forbriger
 *  - 23/10/2009   V1.1   introduced prepend annotations
 *  - 05/03/2015   V1.2   take description texts from text files
 * 
 * ============================================================================
 */
#define STUPLOXX_VERSION \
  "STUPLOXX   V1.2   Plot time series data"

#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <iostream>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>

#include "utilitystructures.h"
#include "globalsettings.h"
#include "constants.h"
#include "datafile.h"
#include "datatrace.h"
#include "functions.h"
#include "panel.h"
#include "screen.h"

#include "usage_text.h"
#include "help_text.h"
#include "interactive_text.h"
#include "description_text.h"
#include "examples_text.h"

using std::cout;
using std::cerr;
using std::endl;

using stuplo::globalsettings;
  
/*======================================================================*/
// main function
/*======================================================================*/

int main(int iargc, char* argv[])
{
  /*----------------------------------------------------------------------*
   * step 1: provide help if requested
   * ---------------------------------
   */

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << STUPLOXX_VERSION << endl;
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  tfxx::cmdline::Commandline cmdline(iargc, argv, stuplo::options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << STUPLOXX_VERSION << endl;
    cerr << usage_text << endl;
    if (cmdline.string_arg(0)=="options")
    {
      cerr << help_text << endl;
    }
    else if (cmdline.string_arg(0)=="keys")
    {
      cerr << interactive_text << endl;
    }
    else if (cmdline.string_arg(0)=="pgplot")
    {
      pgplot::basic_device::ldev();
      cerr << endl;
      cerr << pgplot::usage_escape_sequences << endl;
    }
    else if (cmdline.string_arg(0)=="formats")
    {
      datrw::supported_input_data_types(cerr);
    }
    else if (cmdline.string_arg(0).substr(0,7)=="details")
    {
      datrw::online_help(cmdline.string_arg(0).substr(8,string::npos),cerr,true);
    }
    else 
    {
      cerr << description_text << endl;
    }
    exit(0);
  }

  // control key help requested? print help text...
  if (cmdline.optset(23))
  {
    cerr << STUPLOXX_VERSION << endl;
    cerr << interactive_text << endl;
    exit(0);
  }

  /*----------------------------------------------------------------------*
   * step 2: evaluate command line options
   * -------------------------------------
   */

  // PGPLOT style options
  stuplo::PGstyle pgstyle;
  // general options
  stuplo::Options opt;

  opt.verbose=cmdline.optset(1);
  opt.debug=cmdline.optset(2);
  opt.interactive=cmdline.optset(3);
  opt.device=cmdline.string_arg(4);
  opt.hdevice=cmdline.string_arg(5);
  opt.repeat=cmdline.optset(14);
  opt.repeatinterval=cmdline.int_arg(14);
  opt.issuerepcmd=cmdline.optset(15);
  opt.repcmd=cmdline.string_arg(15);
  pgstyle.title=STUPLOXX_VERSION;
  if (cmdline.optset(16)) { pgstyle.title=cmdline.string_arg(16); }
  pgstyle.tstitle=cmdline.float_arg(17);
  pgstyle.glstyle.underlinelabel=cmdline.optset(18);
  pgstyle.glstyle.colourlabel=cmdline.optset(19);
  opt.hardcopycommand=cmdline.string_arg(20);
  pgstyle.glstyle.eraselabelbox=cmdline.optset(21);
  opt.npanels=cmdline.int_arg(22);
  pgstyle.graphlabelheight=cmdline.float_arg(24);
  pgstyle.graphlabelreserve=cmdline.optset(25);
  pgstyle.datesep=cmdline.string_arg(26);
  /*----------------------------------------------------------------------*/
  // use new GlobalSettings here
  globalsettings.set_verbose(cmdline.optset(1));
  globalsettings.set_debug(cmdline.optset(2));
  globalsettings.set_interactive(cmdline.optset(3));
  globalsettings.set_device(cmdline.string_arg(4));
  globalsettings.set_hdevice(cmdline.string_arg(5));
  globalsettings.set_lwts(cmdline.int_arg(6));
  globalsettings.set_chts(cmdline.double_arg(7));
  globalsettings.set_lwtg(cmdline.int_arg(8));
  globalsettings.set_spp(cmdline.double_arg(9));
  globalsettings.set_lwpb(cmdline.int_arg(10));
  globalsettings.set_chy(cmdline.double_arg(11));
  globalsettings.set_lwyg(cmdline.int_arg(12));
  globalsettings.set_lwys(cmdline.int_arg(13));
  if (cmdline.optset(14))
  { globalsettings.set_repeat(cmdline.int_arg(14)); }
  globalsettings.set_repcmd(cmdline.string_arg(15));
  if (cmdline.optset(16))
  { globalsettings.set_title(cmdline.string_arg(16)); }
  else
  { globalsettings.set_title(STUPLOXX_VERSION); }
  globalsettings.set_tstitle(cmdline.double_arg(17));
  globalsettings.set_labu(cmdline.optset(18));
  globalsettings.set_labc(cmdline.optset(19));
  globalsettings.set_hcommand(cmdline.string_arg(20));
  globalsettings.set_labe(cmdline.optset(21));
  globalsettings.set_npanels(cmdline.int_arg(22));
  globalsettings.set_labh(cmdline.double_arg(24));
  globalsettings.set_labr(cmdline.optset(25));
  globalsettings.set_datesep(cmdline.string_arg(26));
  globalsettings.set_lyd(cmdline.double_arg(27));

  /*----------------------------------------------------------------------*
   * step 3: extract command line parameters
   * ---------------------------------------
   */

  TFXX_assert(cmdline.extra(), "missing input file");

  // data file parameters from command line
  tfxx::cmdline::Tparsed 
    filenames=tfxx::cmdline::parse_cmdline(cmdline, stuplo::keys, opt.debug);

  // fill data file parameter list
  TFXX_debug(opt.debug, "main", "create list of file parameters");
  stuplo::FileParametersList fpl=setparameters(filenames, pgstyle);

  /*----------------------------------------------------------------------*
   * step 4: initialize scaling values
   * ---------------------------------
   */

  // create handle for scaling
  stuplo::THScaling hscaling(new stuplo::Scaling());
  // re-init time scale upon re-read if in repeat mode
  if (opt.repeat) { hscaling->alwaysinittime=true; }

  /*----------------------------------------------------------------------*
   * step 5: open graphics deivce
   * ----------------------------
   */

  // create primary pgplot device
  TFXX_debug(opt.debug, "main", "open plot device");
  pgplot::device dev(opt.device.c_str());
  dev.ask(false);

  /*----------------------------------------------------------------------*
   * step 6: enter loop and read data
   * --------------------------------
   */

  // plotting will be hot first
  bool hot=true;

  while (hot)
  {
    // read data from file
    TFXX_debug(opt.debug, "main", "read data files");
    stuplo::DataFileList dfl=readdata(fpl);

    /*----------------------------------------------------------------------*
     * step 6: distribute time series data to panels
     * ---------------------------------------------
     */

    // fill data into panels
    TFXX_debug(opt.debug, "main", 
               "create list of panels and distribute traces");
    stuplo::PanelVector pl=setuppanels(dfl, opt.npanels, 
                                       opt.verbose, opt.debug);

    /*----------------------------------------------------------------------*
     * step 7: open appropriate screen and plot data
     * ---------------------------------------------
     */

    // create a data screen
    stuplo::Screen* pscreen;

    // place conditionals for different screens here
    // first version only supports chart stepper plots
    {
      TFXX_debug(opt.debug, "main", "init screen");
      pscreen = new stuplo::ChartStepperScreen(pl, hscaling, pgstyle,
                                               opt.debug);
    }

    // plot to screen
    TFXX_debug(opt.debug, "main", "plot screen");
    pscreen->plot(dev);

    /*----------------------------------------------------------------------*
     * step 8: process interactive response or repeat command
     * ------------------------------------------------------
     */

    // cycle only if in interactive or in repeat mode
    hot=(opt.interactive||opt.repeat);

    TFXX_debug(opt.debug, "main", "handle interactive or repeat mode");
    if (opt.interactive)
    {
      // cursor variables
      float x=0;
      float y=0;
      char ch;
      bool stayinloop=true;
      while (hot && stayinloop)
      {
        // call cursor function and wait for user's response
        float xref=x, yref=y;
        TFXX_assert(dev.band(7, 0, xref, yref, x, y, ch)==1, 
                    "PGPLOT cursor input failed!");
        // call the plot function if set after switch conditional
        bool replot=false;
        // evaluate user's response
        switch(ch) {
          // leave program
          case 'X':
          case 'x':
          case 'Q':
          case 'q':
            hot=false;
            break;
          // create hardcopy
          case 'h':
            {
              // place plot command in block to ensure that file will be
              // closed
              { 
                pgplot::device hdev(opt.hdevice.c_str());
                pscreen->plot(hdev);
              }
              if (opt.hardcopycommand.length()>0)
              {
                int ret=system(opt.hardcopycommand.c_str());
                if (WIFSIGNALED(ret) &&
                    (WTERMSIG(ret) == SIGINT || WTERMSIG(ret) == SIGQUIT))
                { 
                  cerr << "WARNING: command \"" << opt.hardcopycommand
                    << "\" returned with status " << ret << endl;
                }
                int status=WEXITSTATUS(ret);
                cout << "command \"" << opt.hardcopycommand
                    << "\" returned with status " << status << endl;
              }
            }
            replot=true;
            break;
          // reread data
          case 'r':
            if (opt.issuerepcmd)
            {
              int ret=system(opt.repcmd.c_str());
              if (WIFSIGNALED(ret) &&
                  (WTERMSIG(ret) == SIGINT || WTERMSIG(ret) == SIGQUIT))
              { hot=false; }

            }
            stayinloop=false;
            break;
          // default is to pass cursor action to the screen
          default:
            replot=pscreen->cursor(dev, ch, x, y);
            break;
        } // switch(ch)
        if (replot) { pscreen->plot(dev); }
      } // while(hot && stayinloop) loop
    } // if (opt.interactive)
    else if (opt.repeat)
    {
      dev.updt();
      sleep(opt.repeatinterval);
      if (opt.issuerepcmd)
      {
        int ret=system(opt.repcmd.c_str());
        if (WIFSIGNALED(ret) &&
            (WTERMSIG(ret) == SIGINT || WTERMSIG(ret) == SIGQUIT))
        { hot=false; }

      }
    }

    // screen is no longer used
    delete pscreen;

  } // while(hot) loop
  dev.updt();

  /*----------------------------------------------------------------------*
   * done
   * ----
   */

} // main

/* ----- END OF stuploxx.cc ----- */
