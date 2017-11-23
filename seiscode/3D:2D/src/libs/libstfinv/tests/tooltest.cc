/*! \file tooltest.cc
 * \brief test internal tools
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/05/2011
 * 
 * test internal tools
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 07/05/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define TOOLTEST_VERSION \
  "TOOLTEST   V1.0   test internal tools"

#include <iostream>
#include <algorithm>
#include <tfxx/commandline.h>
#include <stfinv/parameterhandler.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool testclip, verbose, testmap;
  std::string clipstring, mapstring;
}; // struct Options

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    TOOLTEST_VERSION "\n"
    "usage: tooltest [-clip string] [-map string]" "\n"
    "   or: tooltest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    TOOLTEST_CVSID
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
    {"clip",arg_yes,"-"},
    // 3: verbose mode
    {"map",arg_yes,"-"},
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

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.testclip=cmdline.optset(2);
  opt.clipstring=cmdline.string_arg(2);
  opt.testmap=cmdline.optset(3);
  opt.mapstring=cmdline.string_arg(3);

  // dummy operation: print rest of command line
  if (opt.verbose)
  {
    cout << "part of command line which was not parsed:" << endl;
  }
  while (cmdline.extra()) { cout << cmdline.next() << endl; }

  /*----------------------------------------------------------------------*/

  if (opt.testclip)
  {
    if (opt.verbose)
    {
      cout << endl;
      cout << "Test stfinv::tools::clipstring with default delimiter"
        << endl;
    }
    std::string s=opt.clipstring;
    cout << "remainder is: " << s << endl;
    while (s.length()>0)
    {
      cout << "stripped off: " << stfinv::tools::clipstring(s) << endl;
      cout << "remainder is: " << s << endl;
    }
    std::string delim="++";
    if (opt.verbose)
    {
      cout << endl;
      cout << "Test stfinv::tools::clipstring with delimiter "
        << delim << endl;
    }
    s=opt.clipstring;
    cout << "remainder is: " << s << endl;
    while (s.length()>0)
    {
      cout << "stripped off: " << stfinv::tools::clipstring(s,delim) << endl;
      cout << "remainder is: " << s << endl;
    }
  } // if (opt.testclip)

  /*----------------------------------------------------------------------*/

  if (opt.testmap)
  {
    if (opt.verbose)
    {
      cout << endl;
      cout << "Test stfinv::tools::makeparamap with default delimiter"
        << endl;
    }
    stfinv::tools::Tparamap paramap=stfinv::tools::makeparamap(opt.mapstring);

    std::string liststring=paramap["list"];
    std::replace(liststring.begin(), liststring.end(),',',' ');
    cout << liststring << endl;

  } // if (opt.testmap)

}

/* ----- END OF tooltest.cc ----- */
