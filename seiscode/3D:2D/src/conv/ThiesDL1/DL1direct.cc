/*! \file DL1direct.cc
 * \brief directly send command to DL1 and receive the response
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/11/2008
 * 
 * directly send command to DL1 and receive the response
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
 *  - 24/11/2008   V1.0   Thomas Forbriger
 *  - 16/01/2009   V1.1   add timeout commandline parameter
 * 
 * ============================================================================
 */
/*! \page DL1direct The program DL1direct
 *
 * This program supports interactive control of the DL1 logger.
 * You can use it to send commands and read data to standard output.
 * Execute \code DL1direct -help\endcode to obtain a program description.
 */
#define DL1DIRECT_VERSION \
  "DL1DIRECT   V1.1   directly send command to DL1 and receive the response"
#define DL1DIRECT_CVSID \
  "$Id$"

#include <iostream>
#include <string>
#include <tfxx/commandline.h>
#include <serialxx/serialport.h>
#include "thiesdl1.h"
#include "functions.h"
#include "error.h"

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, debug;
  std::string device;
  int timeout;
};

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    DL1DIRECT_VERSION "\n"
    "usage: DL1direct [-v] [-D] [-d device] command [argument]" "\n"
    "                 [-timeout d]" "\n"
    "   or: DL1direct --help|-h | --Id|-I" "\n"
  };

  // define full help text
  char help_text[]=
  {
    DL1DIRECT_CVSID
    "\n"
    "This program submits exactly one command to the DL1 data logger" "\n"
    "and presents the response on standard output." "\n"
    "\n"
    "-v           verbose mode" "\n"
    "-D           debug mode" "\n"
    "-d dev       device file dev" "\n"
    "-timeout d   set timeout to \"d\" seconds" "\n"
    "-Id          report source code Id\n"
    "\n"
    "supported commands are:" "\n"
    "HH     display commands accepted by DL1" "\n"
    "LL     dump status" "\n"
    "PD     power down" "\n"
    "SS     dump entire data buffer" "\n"
    "MM     display current data value" "\n"
    "DD     display date" "\n"
    "ZZ     display time" "\n"
    "XX     display station code" "\n"
    "SD d   set date and time" "\n"
    "SCD    set current date and time" "\n"
    "ts d   request data of day d" "\n"
    "ds d   request data since date d" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: device file name
    {"device",arg_yes,"/dev/ttyS0"},
    // 3: debug mode
    {"D",arg_no,"-"},
    // 4: debug mode
    {"timeout",arg_yes,"2"},
    // 5: debug mode
    {"Id",arg_no,"-"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    exit(0);
  }

  // request source code Id
  if (cmdline.optset(5))
  {
    cerr << "Thies DL1 data logger source code version:" << endl;
    int i=0;
    while (dl1::CVSIDS[i]!=0)
    {
      cerr << dl1::CVSIDS[i] << endl;
      ++i;
    }
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.device=cmdline.string_arg(2);
  opt.debug=cmdline.optset(3);
  opt.timeout=cmdline.int_arg(4);

  if (opt.verbose) { cout << "open port " << opt.device << endl; }
  dl1::DL1 port(opt.device, opt.debug);

  int timeout=opt.timeout;
  if (opt.verbose) { cout << "timeout: " << timeout << "seconds" << endl; }

  DL1_assert(cmdline.extra(), "ERROR: missing command");
  std::string command=cmdline.next();
  if (opt.verbose) { cout << "issue command " << command << endl; }
  if (command == "LL") { port.sendLL(); }
  else if ( command == "HH") { port.sendHH(); }
  else if ( command == "PD") { port.sendPD(); }
  else if ( command == "SS") { port.sendSS(); }
  else if ( command == "MM") { port.sendMM(); }
  else if ( command == "DD") { port.sendDD(); }
  else if ( command == "ZZ") { port.sendZZ(); }
  else if ( command == "XX") { port.sendXX(); }
  else if ( command == "ts") 
  { 
    DL1_assert(cmdline.extra(), "ERROR: missing time string");
    libtime::TAbsoluteTime date(cmdline.next());
    if (opt.verbose) { cout << "request data for day " 
      << date.timestring() << endl; }
    port.sendts(date);
  }
  else if ( command == "ds") 
  { 
    DL1_assert(cmdline.extra(), "ERROR: missing time string");
    libtime::TAbsoluteTime date(cmdline.next());
    if (opt.verbose) { cout << "request all data since " 
      << date.timestring() << endl; }
    port.sendds(date);
    // increase timeout
    timeout=opt.timeout*10;
    if (opt.verbose) { cout << "increased timeout value to "
      << timeout << " seconds" << endl; }
  }
  else if ( command == "SD") 
  { 
    port.activate();
    DL1_assert(cmdline.extra(), "ERROR: missing time string");
    libtime::TAbsoluteTime date(cmdline.next());
    if (opt.verbose) { cout << "set time to " << date.timestring() << endl; }
    port.settime(date);
    port.sendLL();
  }
  else if ( command == "SCD") 
  { 
    if (opt.verbose) { cout << "set curent time" << endl; }
    libtime::TAbsoluteTime now=libtime::utc();
    cout << "waiting for " << 60-now.second() << " seconds." << endl;
    port.setcurrenttime();
    port.sendLL();
  }
  else
  {
    DL1_abort("ERROR: unknown command");
  }

  while (true) { cout << port.getline(timeout) << endl; }
}

/* ----- END OF DL1direct.cc ----- */
