/*! \file portread.cc
 * \brief read from port
 * 
 * ----------------------------------------------------------------------------
 * 
 * $Id$
 * \author Thomas Forbriger
 * \date 24/11/2008
 * 
 * read from port
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
 * 
 * ============================================================================
 */
#define PORTREAD_VERSION \
  "PORTREAD   V1.0   read from port"
#define PORTREAD_CVSID \
  "$Id$"

#include <iostream>
#include <string>
#include <tfxx/commandline.h>
#include <serialxx/serialport.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose;
  std::string device, delimiter;
};

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    PORTREAD_VERSION "\n"
    "usage: portread [-p device] [-v] [-d delimiter]" "\n"
    "   or: portread --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    PORTREAD_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: device name
    {"p",arg_yes,"/dev/ttyS0"},
    // 3: delimiter name
    {"d",arg_yes,"\r"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<1) 
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

  /*
  // dummy operation: print option settings
  for (int iopt=0; iopt<2; iopt++)
  {
    cout << "option: '" << options[iopt].opt_string << "'" << endl;
    if (cmdline.optset(iopt)) {  cout << "  option was set"; }
    else { cout << "option was not set"; }
    cout << endl;
    cout << "  argument (string): '" << cmdline.string_arg(iopt) << "'" << endl;
    cout << "     argument (int): '" << cmdline.int_arg(iopt) << "'" << endl;
    cout << "    argument (long): '" << cmdline.long_arg(iopt) << "'" << endl;
    cout << "   argument (float): '" << cmdline.float_arg(iopt) << "'" << endl;
    cout << "  argument (double): '" << cmdline.double_arg(iopt) << "'" << endl;
    cout << "    argument (bool): '";
    if (cmdline.bool_arg(iopt))
    { cout << "true"; } else { cout << "false"; }
    cout << "'" << endl;
  }
  while (cmdline.extra()) { cout << cmdline.next() << endl; }

  // dummy operation: print rest of command line
  while (cmdline.extra()) { cout << cmdline.next() << endl; }
  */

  Options opt;

  opt.verbose=cmdline.optset(1);
  opt.device=cmdline.string_arg(2);
  opt.delimiter=cmdline.string_arg(3);

  serialport::SerialPort port(opt.device);
//  port.debug(true);
  std::string text=port.read(opt.delimiter);
  cout << text << endl;
}

/* ----- END OF portread.cc ----- */
