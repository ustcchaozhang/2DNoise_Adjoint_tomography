/*! \file misctest.cc
 * \brief test miscellaneous functions and modules
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/06/2016
 * 
 * test miscellaneous functions and modules
 * 
 * Copyright (c) 2016 by Thomas Forbriger (BFO Schiltach) 
 * 
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
 * along with this program. If not, see <http://www.gnu.org/licenses/>.

 * ----
 *
 * REVISIONS and CHANGES 
 *  - 27/06/2016   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define MISCTEST_VERSION \
  "MISCTEST   V1.0   test miscellaneous functions and modules"

#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/hexdump.h>

using std::cout;
using std::cerr;
using std::endl;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    MISCTEST_VERSION "\n"
    "usage: misctest [--hexdump]" "\n"
    "   or: misctest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "--hexdump    test hex dump function"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: verbose mode
    {"hexdump",arg_no,"-"},
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

  // hexdump
  // -------
  if (cmdline.optset(2))
  {
    int v=12;
    char q[]="Hi there. This is a quite long character sequence.";
    TFXX_hexdump(v);
    std::cout << std::endl;
    TFXX_hexdump(q);
    std::cout << std::endl;
    std::cout << "raw output of 30 bytes of q:" << std::endl;
    tfxx::util::hexdump(q, 30);
    std::cout << std::endl;
    TFXX_hexdump(cmdline);
  } // hexdump
}

/* ----- END OF misctest.cc ----- */
