/*! \file sfftest.cc
 * \brief test SFF reading
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/06/2007
 * 
 * test SFF reading
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
 *  - 19/06/2007   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define SFFTEST_VERSION \
  "SFFTEST   V1.0   test SFF reading"

#include <iostream>
#include <fstream>
#include <tfxx/commandline.h>
#include <datrwxx/error.h>
#include <datrwxx/sff.h>

using std::cout;
using std::cerr;
using std::endl;

typedef aff::Series<double> Tseries;

struct Options {
  bool debug, verbose, streams;
  bool count;
  int nprint;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SFFTEST_VERSION "\n"
    "usage: sfftest [-D] [-v] [-s] filename ..." "\n"
    "   or: sfftest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "filename ... SFF data files for reading" "\n"
    "\n"
    "-D           DEBUG mode" "\n"
    "-v           verbose mode" "\n"
    "-s           use streams" "\n"
    "-n=val       display only vale samples at start end end of file" "\n"
    "-c           only count samples" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: DEBUG mode
    {"D",arg_no,"-"},
    // 3: stream mode
    {"s",arg_no,"-"},
    // 4: count samples
    {"c",arg_no,"-"},
    // 5: number of samples
    {"n",arg_yes,"10" },
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
  opt.debug=cmdline.optset(2);
  opt.streams=cmdline.optset(3);
  opt.count=cmdline.optset(4);
  opt.nprint=cmdline.int_arg(5);

  while (cmdline.extra()) 
  {
    std::string filename=cmdline.next();
    if (opt.verbose)
    {
      std::cout << std::endl;
      std::cout << "file: " << filename << std::endl;
    }

    std::ifstream ifs(filename.c_str());
    if (opt.streams)
    {
      datrw::isffstream is(ifs);
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
      if (opt.verbose)
      {
        std::cout << is.wid2().line() << std::endl;
      }
      if ((!opt.count) && (opt.verbose))
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
      sff::FileHeader fileheader(ifs, opt.debug);
      if (opt.verbose)
      {
        cout << "read file header:" << endl;
        fileheader.write(cout);
        cout << "-----------------" << endl << endl;
      }
      bool last=false;
      while (!last)
      {
        sff::InputWaveform<Tseries> inputwaveform(ifs, opt.debug);
        if (opt.verbose)
        {
          cout << "read trace header:" << endl;
          inputwaveform.header().writeheader(cout);
          inputwaveform.header().writetrailer(cout);
          cout << "------------------" << endl << endl;
        }
        last=inputwaveform.last();
      }
    }
  }
}

/* ----- END OF sfftest.cc ----- */
