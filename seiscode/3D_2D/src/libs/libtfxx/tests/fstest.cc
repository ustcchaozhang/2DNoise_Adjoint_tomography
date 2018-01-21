/*! \file fstest.cc
 * \brief test file system utilities
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/12/2012
 * 
 * test file system utilities
 * 
 * Copyright (c) 2012 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This file is part of libtfxx.
 *
 * libtfxx is free software; you can redistribute it and/or modify
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
 *  - 16/12/2012   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define FSTEST_VERSION \
  "FSTEST   V1.0   test file system utilities"

#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/fs.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool dirname, mkdir, mkdirp;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    FSTEST_VERSION "\n"
    "usage: fstest [-d] [-m] [-p] path [path ...]" "\n"
    "   or: fstest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    FSTEST_CVSID
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: dirname
    {"d",arg_no,"-"},
    // 3: mkdir
    {"m",arg_no,"-"},
    // 4: mkdirp
    {"p",arg_no,"-"},
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

  opt.dirname=cmdline.optset(2);
  opt.mkdir=cmdline.optset(3);
  opt.mkdirp=cmdline.optset(4);

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
  */
  while (cmdline.extra()) 
  {
    std::string path=cmdline.next();
    cout << "\nnext on command line:\n" << path << endl; 

    if (opt.dirname)
    {
      cout << "dirname("<<path<<"): " << tfxx::fs::dirname(path) << endl;
    }

    if (opt.mkdir)
    {
      cout << "create directory " << path << endl;
      tfxx::fs::mkdir(path);
    }

    if (opt.mkdirp)
    {
      cout << "create directory path " << path << endl;
      tfxx::fs::mkdirp(path);
    }
  }
}

/* ----- END OF fstest.cc ----- */
