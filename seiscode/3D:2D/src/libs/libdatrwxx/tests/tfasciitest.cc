/*! \file tfasciitest.cc
 * \brief test tfascii reading
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Daniel Armbruster
 * \date 14/10/2010
 * 
 * Purpose: test tfascii reading
 *
 * ----
 * This file is part of libdatrwxx.
 *
 * libdatrwxx is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * libdatrwxx is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with libdatrwxx.  If not, see <http://www.gnu.org/licenses/>.
 * ----
 * 
 * Copyright (c) 2010 by Daniel Armbruster
 * 
 * REVISIONS and CHANGES 
 * 14/10/2010  V0.1  Daniel Armbruster
 * 
 * ============================================================================
 */
#define TFASCIITEST_VERSION \
  "TFASCIITEST   V1.0   test tfascii reading"
 
#include <iostream>
#include <fstream>
#include <iomanip>
#include <tfxx/commandline.h>
#include <datrwxx/error.h>
#include <datrwxx/tfascii.h>
#include <datrwxx/readany.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, dseries, fseries, iseries, skip, micsecs;
}; // struct Options

int main(int iargc, char* argv[])
{
  // define usage information
  char usage_text[]=
  {
    TFASCIITEST_VERSION "\n"
    "usage: tfasciitest [-v] [-dseries] [-fseries] [-iseries] [-skip]" "\n"
    "                   [-micsecs] filename" "\n"
    "   or: tfasciitest --help" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "filename     file to read" "\n"
    "-v           be verbose" "\n"
    "-dseries     test reading a double series" "\n"
    "-fseries     test reading a float series" "\n"
    "-iseries     test reading a integer series" "\n"
    "-skip        test to skip a series and read only the header data" "\n"
    "-micsecs     test the output of the microseconds in the date values" "\n"
  };
  
  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: read double series
    {"dseries",arg_no,"-"},
    // 3: read float series
    {"fseries",arg_no,"-"},
    // 4: read integer series
    {"iseries",arg_no,"-"},
    // 5: skip trace and read only header
    {"skip",arg_no,"-"},
    // 6: print date values including microseconds
    {"micsecs",arg_no,"-"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc < 2) 
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
  opt.dseries=cmdline.optset(2);
  opt.fseries=cmdline.optset(3);
  opt.iseries=cmdline.optset(4);
  opt.skip=cmdline.optset(5);
  opt.micsecs=cmdline.optset(6);

  DATRW_assert(cmdline.extra(), "missing filename!");


/*----------------------------------------------------------------------*/

  while (cmdline.extra()) 
  {
    std::string infile=cmdline.next();
    std::ifstream ifs(infile.c_str());
    DATRW_assert(ifs.good(), "invalid file!");
    datrw::itfasciistream is(ifs, opt.verbose);
    if (opt.verbose) 
    { 
      cout << "Reading file " << infile << endl; 
      if (opt.dseries)
      { 
        cout << endl << "TEST: double reading" << endl; 
      } else
      if (opt.fseries)
      { 
        cout << endl << "TEST: float reading" << endl;
      } else
      if (opt.iseries)
      { 
        cout << endl << "TEST: integer reading" << endl;
      }
      cout << "=====================" << endl; 
      if (is.hassrce()) 
      { 
        cout << is.srce().line();
        if (opt.micsecs)
        {
          cout << "Test micsecs in SRCE date: " 
            << is.srce().date.timestring() << endl;
        }
      }
      if (is.hasfree()) { is.free().write(cout); }
    }
    datrw::Tdseries dseries;
    datrw::Tfseries fseries;
    datrw::Tiseries iseries;
    if (opt.skip) { is.skipseries(); } else
    if (opt.dseries) { is >> dseries; } else
    if (opt.fseries) { is >> fseries; } else
    if (opt.iseries) { is >> iseries; }
    if (opt.verbose)
    {
      cout << endl;
      // print traceheader
      cout << is.wid2().line();
      if (opt.micsecs)
      {
        cout << "Test micsecs in WID2 date: " 
          << is.wid2().date.timestring() << endl;
      }
      if (is.hasinfo()) { cout << is.info().line(); }
      if (is.hasfree()) { is.free().write(cout); }
      cout << endl;

      // print series
      if (opt.dseries && !opt.skip)
      {
        cout << "data:" << endl;
        for (int isample=dseries.first(); isample<=dseries.last();
            ++isample)
        {
          cout << std::setprecision(7) << std::scientific << std::showpos
               << dseries(isample) << endl;
        }
      } else
      if (opt.fseries && !opt.skip)
      {
        cout << "data:" << endl;
        for (int isample=fseries.first(); isample<=fseries.last();
            ++isample)
        {
          cout << std::setprecision(7) << std::scientific << std::showpos
               << fseries(isample) << endl;
        }
      } else
      if (opt.iseries && !opt.skip)
      {
        cout << "data:" << endl;
        for (int isample=iseries.first(); isample<=iseries.last();
            ++isample)
        {
          cout << iseries(isample) << endl;
        }
      }
    }
  }
}
/* ----- END OF tfasciitest.cc ----- */
