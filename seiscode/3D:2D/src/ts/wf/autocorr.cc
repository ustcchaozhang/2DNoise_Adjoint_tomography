/*! \file autocorr.cc
 * \brief autocorrelation
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 01/02/2005
 * 
 * autocorrelation
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 01/02/2005   V1.0   Thomas Forbriger
 *  - 22/11/2016   V1.1   use new assignment operators of time series
 *                        containers
 * 
 * ============================================================================
 */
#define AUTOCORR_VERSION \
  "AUTOCORR   V1.0   autocorrelation"

#include <fstream>
#include <iostream>
#include <list>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <sffxx.h>
#include <tsxx/tsxx.h>
#include <tsxx/correlate.h>
#include <tsxx/wid2timeseries.h>
#include <tsxx/wid2tsio.h>
#include <datrwxx/sff.h>
#include <aff/seriesoperators.h>

using std::cout;
using std::cerr;
using std::endl;

typedef std::list<std::string> Tnames;
typedef ts::TDsfftimeseries Tts;

struct Options {
  bool verbose, overwrite, debug;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    AUTOCORR_VERSION "\n"
    "usage: autocorr -v -o -D file [file [...]] outfile" "\n"
    "   or: autocorr --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v           be verbose" "\n"
    "-D           debug mode" "\n"
    "-o           overwrite existing output file" "\n"
    "file ...     input file(s)" "\n"
    "outfile      output file" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: overwrite mode
    {"o",arg_no,"-"},
    // 3: debug mode
    {"D",arg_no,"-"},
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

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.debug=cmdline.optset(3);

  TFXX_assert(cmdline.extra(), "missing input file!");
  std::string outfile=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing output file!");
  Tnames infiles;
  while (cmdline.extra())
  {
    infiles.push_back(outfile);
    outfile=cmdline.next();
  }

  if (opt.verbose) { cout << "open output file " << outfile << endl; }
  std::ofstream ofs(outfile.c_str());
  datrw::osffstream os(ofs);

  sff::SRCE srce=sff::srce_reference();
  os << srce;

  Tnames::const_iterator I=infiles.begin();
  while (I!=infiles.end())
  {
    if (opt.verbose) { cout << "open " << std::string(*I) << endl; }
    std::ifstream ifs(I->c_str());
    datrw::isffstream is(ifs, opt.debug);
    while (is.good())
    {
      if (opt.verbose) { cout << "read trace" << endl; }
      Tts data;
      Tts::Tseries samples;
      is >> samples >> data.header;
      data=samples;
      sff::INFO info;
      is >> info;
      if (opt.verbose) { cout << data.header.line() << endl; }
      Tts result;
      result=ts::correlate(data,data);
      result /= Tts::Tvalue(data.size());
      result.header=data.header;
      result.header.auxid="corr";
      result.header.date=srce.date
        +(result.first()*libtime::double2time(result.header.dt));
      if (is.hasinfo()) { os << info; }
      os << result;
    }
    ++I;
  }
}

/* ----- END OF autocorr.cc ----- */
