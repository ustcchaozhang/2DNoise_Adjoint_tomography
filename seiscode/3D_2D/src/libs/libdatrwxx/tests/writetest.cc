/*! \file writetest.cc
 * \brief test writing a file
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 20/02/2010
 * 
 * test writing a file
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 20/02/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 *
 * \example writetest.cc 
 * \brief This program is an example on writing data with libdatrwxx.
 */
#define WRITETEST_VERSION \
  "WRITETEST   V1.0   test writing a file"

#include <iostream>
#include <fstream>
#include <tfxx/commandline.h>
#include <datrwxx/sff.h>
#include <datrwxx/writeany.h>
#include <datrwxx/util.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, debug;
  bool sfftest;
  std::string sfffilename;
  bool gsetest;
  std::string gsefilename;
  bool anytest;
  std::string anyfilename;
  std::string format;
  double amplitude;
  bool overwrite;
  bool testdigits;
  double digitstestvalue;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    WRITETEST_VERSION "\n"
    "usage: writetest [-v] [-D] [-sff n] [-gse n] [-any n]" "\n"
    "                 [-t type] [-A amp] [-o] [-digits v]" "\n"
    "   or: writetest --help|-h" "\n"
    "   or: writetest --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-v           be verbose" "\n"
    "-D           enter debug mode" "\n"
    "-sff n       write to SFF file \"n\"" "\n"
    "-gse n       write to GSE file \"n\"" "\n"
    "-any n       write to file \"n\"" "\n"
    "\n"
    "options to define \"any\" test:" "\n"
    "-t type      select output format type for any" "\n"
    "-A amp       select signal amplitude" "\n"
    "-o           overwrite existing output file" "\n"
    "-digits v    test digits functions" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: debug mode
    {"D",arg_no,"-"},
    // 3: write SFF data
    {"sff",arg_yes,"-"},
    // 4: write GSE data
    {"gse",arg_yes,"-"},
    // 5: write any format
    {"any",arg_yes,"-"},
    // 6: write any format
    {"t",arg_yes,"sff"},
    // 7: write any format
    {"A",arg_yes,"1."},
    // 8: write any format
    {"o",arg_no,"-"},
    // 9: write any format
    {"digits",arg_yes,"1."},
    // 10: write any format
    {"xhelp",arg_no,"-"},
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
  if (cmdline.optset(0) || cmdline.optset(10))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    datrw::supported_data_types(cerr);
    if (cmdline.optset(10))
    {
      datrw::online_help(cerr);
    }
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
  opt.debug=cmdline.optset(2);
  opt.sfftest=cmdline.optset(3);
  opt.sfffilename=cmdline.string_arg(3);
  opt.gsetest=cmdline.optset(4);
  opt.gsefilename=cmdline.string_arg(4);
  opt.anytest=cmdline.optset(5);
  opt.anyfilename=cmdline.string_arg(5);
  opt.format=cmdline.string_arg(6);
  opt.amplitude=cmdline.double_arg(7);
  opt.overwrite=cmdline.optset(8);
  opt.testdigits=cmdline.optset(9);
  opt.digitstestvalue=cmdline.double_arg(9);

  /*======================================================================*/

  if (opt.sfftest)
  {
    if (opt.verbose)
    { cout << "SFF test: write to file " << opt.sfffilename << endl; }
    std::ofstream ofs(opt.sfffilename.c_str());
    datrw::osffstream os(ofs, "", opt.debug);
    sff::FREE free;
    free.append("my file comment");
    sff::SRCE srce;
    os << free;
    os << srce;
    {
      sff::WID2 wid2;
      wid2.dt=10;
      wid2.channel="LIN";
      wid2.station="THOF";
      {
        datrw::Tiseries s(10);
        sff::FREE tracefree;
        tracefree.append("linear integer trace");
        sff::INFO info;
        for (int i=s.f(); i<=s.l(); ++i)
        {
          s(i)=i;
        }
        os << wid2;
        os << tracefree;
        os << info;
        os << s;
      }
      {
        datrw::Tdseries s(100);
        sff::FREE tracefree;
        tracefree.append("double sine trace");
        for (int i=s.f(); i<=s.l(); ++i)
        {
          s(i)=5.e4*sin(2*3.14159265358979311599*i/(s.size()-1));
        }
        wid2.dt=1.;
        wid2.channel="SIN";
        os << wid2;
        os << tracefree;
        os << s;
      }
      {
        datrw::Tfseries s(1000);
        sff::FREE tracefree;
        tracefree.append("float cosine trace");
        for (int i=s.f(); i<=s.l(); ++i)
        {
          s(i)=6.e-12*cos(2*3.14159265358979311599*i/(s.size()-1));
        }
        wid2.dt=.1;
        wid2.channel="COS";
        os << wid2;
        os << tracefree;
        os << s;
      }
    }
  }

  /*======================================================================*/

  if (opt.gsetest)
  {
    if (opt.verbose)
    { cout << "GSE test: write to file " << opt.gsefilename << endl; }
    std::ofstream ofs(opt.gsefilename.c_str());
    datrw::ogsestream os(ofs, opt.debug);
    sff::FREE free;
    free.append("my file comment");
    sff::SRCE srce;
    os << free;
    os << srce;
    {
      sff::WID2 wid2;
      wid2.dt=10;
      wid2.channel="LIN";
      wid2.station="THOF";
      {
        datrw::Tiseries s(100);
        sff::FREE tracefree;
        tracefree.append("linear integer trace");
        sff::INFO info;
        for (int i=s.f(); i<=s.l(); ++i)
        {
          s(i)=i/5-10;
        }
        os << wid2;
        os << tracefree;
        os << info;
        os << s;
      }
      {
        datrw::Tdseries s(100);
        sff::FREE tracefree;
        tracefree.append("double sine trace");
        for (int i=s.f(); i<=s.l(); ++i)
        {
          s(i)=30.*sin(2*3.14159265358979311599*i/(s.size()-1));
        }
        wid2.dt=1.;
        wid2.channel="SIN";
        os << wid2;
        os << tracefree;
        os << s;
      }
      {
        datrw::Tfseries s(1000);
        sff::FREE tracefree;
        tracefree.append("float cosine trace");
        for (int i=s.f(); i<=s.l(); ++i)
        {
          s(i)=6.*cos(2*3.14159265358979311599*i/(s.size()-1));
        }
        wid2.dt=.1;
        wid2.channel="COS";
        os << wid2;
        os << tracefree;
        os << s;
      }
    }
  }

  /*======================================================================*/

  if (opt.anytest)
  {
    if (opt.verbose)
    { 
      cout << "ANY test: write to file " << opt.anyfilename << endl; 
      cout << "using " << opt.format << endl; 
    }
    if (!opt.overwrite) { datrw::abort_if_exists(opt.anyfilename); }
    std::ofstream ofs(opt.anyfilename.c_str());
    datrw::oanystream os(ofs, opt.format, opt.debug);
    sff::FREE free;
    free.append("my file comment");
    sff::SRCE srce;
    os << free;
    os << srce;
    sff::WID2 wid2;
    wid2.dt=0.01;
    wid2.channel="SIN";
    wid2.station="THOF";
    datrw::Tdseries s(100);
    sff::FREE tracefree;
    tracefree.append("double sine trace");
    for (int i=s.f(); i<=s.l(); ++i)
    {
      s(i)=opt.amplitude*sin(2*3.14159265358979311599*i/(s.size()-1));
    }
    os << wid2;
    os << tracefree;
    os << s;
    tracefree.append("smaller sampling interval");
    wid2.dt=0.002;
    wid2.channel="SIN";
    os << wid2;
    os << tracefree;
    os << s;
  }

  /*======================================================================*/

  if (opt.testdigits)
  {
    cout << "test digits functions with value " << opt.digitstestvalue <<
      endl;
    cout << "number of siginificant digits "
      << datrw::util::nsignificantdigits(opt.digitstestvalue,
                                         opt.debug) << endl;
    cout << "number of trailing digits "
      << datrw::util::ntrailingdigits(opt.digitstestvalue,
                                      opt.debug) << endl;
  }

}

/* ----- END OF writetest.cc ----- */
