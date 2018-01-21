/*! \file binarytest.cc
 * \brief test binary format I/O functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 02/09/2011
 * 
 * test binary format I/O functions
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
 * REVISIONS and CHANGES 
 *  - 02/09/2011   V1.0   Thomas Forbriger
 *  - 19/02/2014 thof:    include datrwxx/binary.h
 * 
 * ============================================================================
 */
#define BINARYTEST_VERSION \
  "BINARYTEST   V1.0   test binary format I/O functions"

#include <iostream>
#include <fstream>
#include <string>
#include <tfxx/commandline.h>
#include <aff/seriesoperators.h>
#include <aff/dump.h>
#include <datrwxx/binary.h>
#include <datrwxx/ibinstream.h>
#include <datrwxx/obinstream.h>

/*----------------------------------------------------------------------*/

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool iotest, skipsamples, verbose;
  std::string iofile;
}; // struct Options

/*----------------------------------------------------------------------*/

void values(const char& out, const char& in)
{
  cout << "written to file: " << int(out) << endl;
  cout << "read from file: " << int(in) << endl;
  if (out&::datrw::binary::Ffree)
  { cout << "* flag set: free" << endl; }
  if (out&::datrw::binary::Fsrce)
  { cout << "* flag set: srce" << endl; }
  if (out&::datrw::binary::Finfo)
  { cout << "* flag set: info" << endl; }
  if (out&::datrw::binary::Fdouble)
  { cout << "* flag set: double" << endl; }
  if (out&::datrw::binary::Ffloat)
  { cout << "* flag set: float" << endl; }
  if (out&::datrw::binary::Fint)
  { cout << "* flag set: int" << endl; }
}

/*----------------------------------------------------------------------*/

template<class C>
void reportsff(const C& out, const C& in)
{
  cout << "written to file:" << endl;
  ::sff::verbose(cout, out);
  cout << "read from file:" << endl;
  ::sff::verbose(cout, in);
}

/*----------------------------------------------------------------------*/

template<class C>
void reportseries(const C& out, const C& in)
{
  cout << "written to file:" << endl;
  ::aff::dump(out);
  cout << "read from file:" << endl;
  ::aff::dump(in);
  cout << "residual:" << endl;
  ::aff::dump(out-in);
}

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    BINARYTEST_VERSION "\n"
    "usage: binarytest [-v] [-iotest file] [-skip]" "\n"
    "   or: binarytest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-v           be verbose\n"
    "-iotest file execute fixed I/O test with file \"file\"\n"
    "-skip        skip samples of series, just count\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: skip samples
    {"skip",arg_no,"-"},
    // 3: verbose mode
    {"iotest",arg_yes,"-"},
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
  opt.skipsamples=cmdline.optset(2);
  opt.iotest=cmdline.optset(3);
  opt.iofile=cmdline.string_arg(3);

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

  /*----------------------------------------------------------------------*/

  if (opt.iotest)
  {
    // prepare test data
    datrw::Tiseries iseries(-10,10);
    datrw::Tdseries dseries(15);
    datrw::Tfseries fseries(20);
    for (int i=iseries.f(); i<=iseries.l(); ++i)
    { iseries(i)=2*i; }
    for (int i=dseries.f(); i<=dseries.l(); ++i)
    { dseries(i)=double(i)*i; }
    for (int i=fseries.f(); i<=fseries.l(); ++i)
    { fseries(i)=sin((i-fseries.f())*2.
                     *3.141592653589793115998/(fseries.size()-1)); }
    ::sff::WID2 wid2;
    ::sff::INFO info;
    ::sff::SRCE srce;
    ::sff::FREE free;

    wid2.nsamples=50;
    wid2.dt=1.e-4;
    wid2.date=libtime::TAbsoluteTime(2010,5,6,18,23,12,13,14);
    wid2.station="STAT";
    wid2.channel="CHA";
    wid2.auxid="AUX";
    wid2.instype="my seismometer type";
    
    info.cx=1.;
    info.cy=2.;
    info.cz=3.;
    info.nstacks=49;
    
    srce.cx=-1.;
    srce.cy=-2.;
    srce.cz=-3.;
    srce.type="my source type for this test";
    srce.date=libtime::TAbsoluteTime(2006,1,1,9,15,12,345,678);

    free.append(BINARYTEST_VERSION);
    free.append("my free comment line");

    char fflags=
      datrw::binary::Finfo|
      datrw::binary::Fdouble;
    char tflags=
      datrw::binary::Fsrce;

    const char* const magic="B123";
    // write data
    {
      std::ofstream ofs(opt.iofile.c_str(),
                        std::ios_base::out|std::ios_base::binary);
      datrw::binary::obinstream os(ofs, magic);

      os << wid2;
      os << free;
      os << fseries;
      os << info;
      os << dseries;
      os << srce;
      os << char(fflags);
      os << char(tflags);
      os << iseries;
    }

    // reread file
    {
      std::ifstream ifs(opt.iofile.c_str(),
                        std::ios_base::in|std::ios_base::binary);
      datrw::binary::ibinstream is(ifs, magic);

      ::sff::WID2 inwid2;
      is >> inwid2;
      reportsff(wid2, inwid2);
      
      ::sff::FREE infree;
      is >> infree;
      reportsff(free, infree);

      if (opt.skipsamples) 
      {
        cout << "skipped " << is.skipfseries() << " samples" << endl; 
      }
      else
      {
        datrw::Tfseries infseries;
        is >> infseries;
        reportseries(fseries, infseries);
      }
      
      ::sff::INFO ininfo;
      is >> ininfo;
      reportsff(info, ininfo);

      if (opt.skipsamples) 
      {
        cout << "skipped " << is.skipdseries() << " samples" << endl; 
      }
      else
      {
        datrw::Tdseries indseries;
        is >> indseries;
        reportseries(dseries, indseries);
      }

      ::sff::SRCE insrce;
      is >> insrce;
      reportsff(srce, insrce);

      char inflags;
      is >> inflags;
      values(char(fflags), inflags);
      is >> inflags;
      values(char(tflags), inflags);

      if (opt.skipsamples) 
      {
        cout << "skipped " << is.skipiseries() << " samples" << endl; 
      }
      else
      {
        datrw::Tiseries iniseries;
        is >> iniseries;
        reportseries(iseries, iniseries);
      }
    }

  }
}

/* ----- END OF binarytest.cc ----- */
