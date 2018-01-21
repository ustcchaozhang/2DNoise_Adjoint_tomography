/*! \file regextest.cc
 * \brief test regex match function
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/12/2007
 * 
 * test regex match function
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 05/12/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define REGEXTEST_VERSION \
  "REGEXTEST   V1.0   test regex match function"

#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/regexx.h>

using std::cout;
using std::cerr;
using std::endl;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    REGEXTEST_VERSION "\n"
    "usage: regextest" "\n"
    "   or: regextest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    REGEXTEST_CVSID
    "first string on command line: regular expression" "\n"
    "subsequent strings will be checked against expression" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
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

  TFXX_assert(cmdline.extra(), "missing regular expression");
  tfxx::string::regexx myregex(cmdline.next());
  cout << "regular expression: " << myregex.expression() << endl;

  while (cmdline.extra()) 
  { 
    std::string teststring(cmdline.next());
    if (myregex.match(teststring))
    {
      cout << "matches: ";
    }
    else
    {
      cout << "does NOT match: ";
    }
    cout << teststring << endl;
  }
}

/* ----- END OF regextest.cc ----- */
