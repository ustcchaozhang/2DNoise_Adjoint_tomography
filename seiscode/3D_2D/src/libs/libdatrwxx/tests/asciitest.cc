/*! \file asciitest.cc
 * \brief test ASCII format I/O functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 02/09/2011
 * 
 * test ASCII format I/O functions
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
 *  - 02/09/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define ASCIITEST_VERSION \
  "ASCIITEST   V1.0   test ASCII format I/O functions"

#include <iostream>
#include <fstream>
#include <string>
#include <tfxx/commandline.h>
#include <datrwxx/ascii.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, overwrite;
  bool writetest, debug;
  std::string writefilename;
  int ntraces, nsamples;
  double frequency;
}; // struct Options

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    ASCIITEST_VERSION "\n"
    "usage: asciitest [-v] [-overwrite] [-write name] [-ntraces n]" "\n"
    "                 [-nsamples n] [-frequency f] [-DEBUG]\n"
    "   or: asciitest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-v           verbose mode\n"
    "-DEBUG       debug mode\n"
    "-overwrite   overwrite existing output file\n"
    "-write name  write test traces to file \"name\"\n"
    "-ntraces n   write \"n\" traces\n"
    "-nsamples n  write \"n\" samples per trace\n"
    "-frequency f create test signal of frequency \"f\"\n"
    "\n"
    "File reading can be tested with libdatrwxxtests.cc\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: write test data
    {"write",arg_yes,"junk"},
    // 3: overwrite existing file
    {"overwrite",arg_no,"-"},
    // 4: ntraces
    {"ntraces",arg_yes,"1"},
    // 5: nsamples
    {"nsamples",arg_yes,"20"},
    // 6: frequency
    {"frequency",arg_yes,"10."},
    // 7: debug mode
    {"DEBUG",arg_no,"-"},
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
    cerr << endl;
    ::datrw::iasciistream::help(cerr);
    ::datrw::oasciistream::help(cerr);
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.writetest=cmdline.optset(2);
  opt.writefilename=cmdline.string_arg(2);
  opt.overwrite=cmdline.optset(3);
  opt.ntraces=cmdline.int_arg(4);
  opt.nsamples=cmdline.int_arg(5);
  opt.frequency=cmdline.double_arg(6);
  opt.debug=cmdline.optset(7);

  /*======================================================================*/

  if (opt.writetest)
  {
    cout << "test output module\n"
      <<    "==================\n";
    cout << "write to file " << opt.writefilename << endl;
    if (!opt.overwrite) { datrw::abort_if_exists(opt.writefilename); }
    std::ofstream ofs(opt.writefilename.c_str(), datrw::oasciistream::openmode);
    datrw::oasciistream os(ofs, "", opt.debug);

    cout << "file data is stored in ";
    // report output data format
    switch (os.seriestype()) {
      case datrw::Fint:
        cout << "integer";
        break;
      case datrw::Ffloat:
        cout << "single precision floating point";
        break;
      case datrw::Fdouble:
        cout << "double precision floating point";
        break;
      case datrw::Fall:
        cout << "any desired";
        break;
      default:
        TFXX_abort("output stream uses unknown variable type!");
    } // switch (os.seriestype())
    cout << " variable type" << endl;

    cout << "file FREE data can ";
    if (!os.handlesfilefree()) { cout << "NOT "; }
    cout << "be handled" << endl;

    cout << "SRCE data can ";
    if (!os.handlessrce()) { cout << "NOT "; }
    cout << "be handled" << endl;

    ::sff::FREE filefree;
    filefree.append("this is the FREE block for the file header");
    filefree.append(ASCIITEST_VERSION);
    filefree.append("output file name:");
    filefree.append(opt.writefilename);

    ::sff::SRCE srce;
    srce.date=libtime::now();
    srce.type="test source";
    srce.cx=-1.;
    srce.cy=-2.;
    srce.cz=-3.;
    srce.cs=::sff::CS_cartesian;

    if (os.handlesfilefree()) { os << filefree; }
    if (os.handlessrce()) { os << srce; }

    int ntraces=opt.ntraces;
    for (int itrace=0; itrace<ntraces; ++itrace)
    {
      cout << "write trace #" << itrace+1 << endl;

      cout << "trace FREE data can ";
      if (!os.handlestracefree()) { cout << "NOT "; }
      cout << "be handled" << endl;

      cout << "INFO data can ";
      if (!os.handlesinfo()) { cout << "NOT "; }
      cout << "be handled" << endl;

      ::sff::FREE free;
      free.append(ASCIITEST_VERSION);
      
      ::sff::INFO info;
      info.nstacks=itrace;
      info.cx=1.;
      info.cy=2.;
      info.cz=3.;
      info.cs=::sff::CS_cartesian;

      const double dt=2.e-3*(itrace+4);
      ::sff::WID2 wid2;
      wid2.dt=dt;
      wid2.nsamples=opt.nsamples;
      wid2.date=libtime::now();
      wid2.station="FEL";
      wid2.auxid="AUX";
      wid2.instype="SYNTH";

      int typeindex=itrace%3;
      if (typeindex==0)
      {
        aff::Series<double> sine(opt.nsamples);
        for (int isample=0; isample<opt.nsamples; ++isample)
        {
          sine(sine.f()+isample)=(itrace+2)*sin(2.*3.141592653589793115998
                                                *opt.frequency*dt*isample);
        }
        free.append("create double type data");
        wid2.channel="DBL";
        os << wid2;
        if (os.handlesinfo()) { os << info; }
        if (os.handlestracefree()) { os << free; }
        os << sine;
      }
      else if (typeindex==1)
      {
        aff::Series<float> sine(opt.nsamples);
        for (int isample=0; isample<opt.nsamples; ++isample)
        {
          sine(sine.f()+isample)=(itrace+2)*sin(2.*3.141592653589793115998
                                                *opt.frequency*dt*isample);
        }
        free.append("create float type data");
        wid2.channel="FLT";
        os << wid2;
        if (os.handlesinfo()) { os << info; }
        if (os.handlestracefree()) { os << free; }
        os << sine;
      }
      else
      {
        aff::Series<int> sine(opt.nsamples);
        for (int isample=0; isample<opt.nsamples; ++isample)
        {
          sine(isample)=isample%15;
        }
        wid2.channel="INT";
        free.append("create int type data");
        os << wid2;
        if (os.handlesinfo()) { os << info; }
        if (os.handlestracefree()) { os << free; }
        os << sine;
      }
    }
  }

}

/* ----- END OF asciitest.cc ----- */
