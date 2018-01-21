/*! \file sactest.cc
 * \brief test SAC reading functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2004
 * 
 * test SAC reading functions
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
 *  - 21/12/2004   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define SACTEST_VERSION \
  "SACTEST   V1.0   test SAC reading functions"

#include <iostream>
#include <string>
#include <fstream>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <datrwxx/sacread.h>
#include <datrwxx/bytesex.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, dumpheader, readdata;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SACTEST_VERSION "\n"
    "usage: sactest file [-h] [-d]" "\n"
    "   or: sactest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "file         name of SAC file to read" "\n"
    "\n"
    "-h           read and dump header only" "\n"
    "-d           read samples (implies -h)" "\n"
    "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: dump header
    {"h",arg_no,"-"},
    // 3: read data
    {"d",arg_no,"-"},
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
  opt.dumpheader=cmdline.optset(2);
  opt.readdata=cmdline.optset(3);

  TFXX_assert(cmdline.extra(), "missing SAC file name");
  std::string filename=cmdline.next();

  if (opt.verbose)
  {
    cout << "file to process: " << filename << endl;
  }

  if (opt.dumpheader) {
    if (opt.verbose) 
    { 
      cout << "dump header" << endl;
      cout << "-----------" << endl;
    }
    std::ifstream ifs(filename.c_str());
    datrw::sac::SACheader hd=datrw::sac::read_sac_header(ifs);
    cout << "  sampling interval: " << hd.delta << " s" << endl;
    cout << "      initial value: " << hd.b << endl;
    cout << "        final value: " << hd.e << endl;
    cout << "   station latitude: " << hd.stla << " °N" << endl;
    cout << "  station longitude: " << hd.stlo << " °E" << endl;
    cout << "  station elevation: " << hd.stel << " m" << endl;
    cout << "      station depth: " << hd.stdp << " m" << endl;
    cout << "               component azimuth: " << hd.cmpaz << " °" << endl;
    cout << "           component inclination: " << hd.cmpinc << " °" << endl;
    cout << "         zero time of file, year: " << hd.nzyear << endl;
    cout << "          zero time of file, doy: " << hd.nzjday << endl;
    cout << "         zero time of file, hour: " << hd.nzhour << endl;
    cout << "       zero time of file, minute: " << hd.nzmin << endl;
    cout << "       zero time of file, second: " << hd.nzsec << endl;
    cout << "  zero time of file, millisecond: " << hd.nzmsec << endl;
    cout << "    number of samples: " << hd.npts << endl;
    cout << "         station name: " << std::string(hd.kstnm,8) << endl;
    cout << "       component name: " << std::string(hd.kcmpnm,8) << endl;
    cout << "         network name: " << std::string(hd.knetwk,8) << endl;
    cout << "  man-made event name: " << std::string(hd.khole,8) << endl;
    cout << "         type of file: " << hd.iftype << endl;
    cout << "   even sampling flag: " << hd.leven << endl;

    if (opt.readdata)
    {
      datrw::sac::Tseries series(hd.npts);
      typedef char* Pchar;
      ifs.read(Pchar(series.pointer()), hd.npts*sizeof(datrw::sac::Tvalue));
      if (ifs.good())
      { cout << "input stream is good after reading samples" << endl; }
      else
      { cout << "input stream is NOT good after reading samples!" << endl; }
      if (opt.verbose)
      {
        cout << "read " << series.size() << " samples" << endl;
        int ifi=10 > series.last() ? series.last() : 10;
        int il=series.last()-10;
        il=series.first() > il ? series.first() : il;
        for (int i=series.f(); i<=ifi; ++i)
        { cout << i << " " << series(i) << endl; }
        cout << "." << endl << "." << endl << "." << endl;
        for (int i=il; i<=series.last(); ++i)
        { cout << i << " " << series(i) << endl; }
      }
    }
  }
}

/* ----- END OF sactest.cc ----- */
