/*! \file pdastest.cc
 * \brief test pdas reading functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/04/2004
 * 
 * test pdas reading functions
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
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 07/04/2004   V1.0   Thomas Forbriger
 *  - 19/10/2004   V1.1   support different data types
 * 
 * ============================================================================
 */
#define PDASTEST_VERSION \
  "PDASTEST   V1.0   test pdas reading functions"

#include <fstream>
#include <iostream>
#include <tfxx/commandline.h>
#include <datrwxx/error.h>
#include <datrwxx/pdasread.h>
#include <datrwxx/pdas.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose;
  bool count;
  int nprint;
  bool streams;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    PDASTEST_VERSION "\n"
    "usage: pdastest [-v] [-n=val] [-c] [-s] filename ..." "\n"
    "   or: pdastest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "filename     file to read" "\n"
    "\n"
    "-v           be verbose" "\n"
    "-n=val       display only vale samples at start end end of file" "\n"
    "-c           only count samples" "\n"
    "-s           use streams" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: count samples
    {"c",arg_no,"-"},
    // 3: number of samples
    {"n",arg_yes,"10" },
    // 4: use streams
    {"s",arg_no,"-"},
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
  opt.count=cmdline.optset(2);
  opt.nprint=cmdline.int_arg(3);
  opt.streams=cmdline.optset(4);

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
  */
  while (cmdline.extra()) 
  {
    std::cout << std::endl;
    std::string filename=cmdline.next();
    std::cout << "file: " << filename << std::endl;

    std::ifstream ifs(filename.c_str());
    if (opt.streams)
    {
      datrw::ipdasstream is(ifs);
      if (opt.verbose) { is.free().write(std::cout); }
      datrw::Tiseries iseries;
      if (opt.count)
      {
        is.skipseries();
      }
      else
      {
        is >> iseries;
      }
      std::cout << is.wid2().line() << std::endl;
      if (!opt.count)
      {
        int npr=
          opt.nprint < int(iseries.size()/2) ?
          opt.nprint : iseries.size()/2;
        for (int i=0; i<npr; ++i)
        { std::cout << i << " " << iseries(i) << std::endl; }
        std::cout << " ... " << std::endl;
        for (int i=iseries.size()-npr; i<int(iseries.size()); ++i)
        { std::cout << i << " " << iseries(i) << std::endl; }
      }
    }
    else
    {
      datrw::pdas::Header header=datrw::pdas::readheader(ifs, opt.verbose);
      if (opt.count)
      {
        std::cout << "*** number of samples: " << 
          datrw::pdas::countdata(ifs, header.type) << " ***" << std::endl;
      }
      else
      {
        datrw::pdas::Tdata indata;
        datrw::pdas::readdata(ifs, indata, header.type);
        std::cout << "*** number of samples: " << 
          indata.size() << " ***" << std::endl;
        int npr=
          opt.nprint < int(indata.size()/2) ?
          opt.nprint : indata.size()/2;
        for (int i=0; i<npr; ++i)
        { std::cout << i << " " << indata[i] << std::endl; }
        std::cout << " ... " << std::endl;
        for (int i=indata.size()-npr; i<int(indata.size()); ++i)
        { std::cout << i << " " << indata[i] << std::endl; }
      }
    }
  }
}

/* ----- END OF pdastest.cc ----- */
