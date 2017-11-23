/*! \file seifetest.cc
 * \brief test seife format I/O functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 02/09/2011
 * 
 * test seife format I/O functions
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
 * 
 * ============================================================================
 */
#define SEIFETEST_VERSION \
  "SEIFETEST   V1.0   test seife format I/O functions"

#include <fstream>
#include <iostream>
#include <sstream>
#include <tfxx/commandline.h>
#include <datrwxx/seifeio.h>
#include <datrwxx/util.h>
#include <datrwxx/formats.h>
#include <datrwxx/datread.h>
#include <datrwxx/seife.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool parameterline, debug, datafile, testcommentlimit, streamread;
  bool verbose, countonly, moretraces, writesine;
  std::string parameterlinefile, datafilename, streamreadfile, sinefilename;
  int nprint, nsamples;
  double frequency;
  std::string modifiers;
}; // struct Options

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SEIFETEST_VERSION "\n"
    "usage: seifetest [-pl file] [-read file] [-DEBUG] [-stream file]" "\n"
    "                 [-cmmntlim] [-verbose] [-countonly] [-nprint n]\n"
    "                 [-sine file] [-freq f] [-nsamples n] [-moretraces]\n"
    "                 [-modifiers s]\n"
    "   or: seifetest --help|-h" "\n"
    "   or: seifetest --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-pl file     read parameter line from file" "\n"
    "-read file   read seife data file" "\n"
    "-stream file read seife data file through stream interface" "\n"
    "-DEBUG       produce debug output" "\n"
    "-xhelp       print complete online help available" "\n"
    "-cmmntlim    test automatic limiting of comment lines" "\n"
    "-verbose     be verbose" "\n"
    "-countonly   just count traces in stream mode (test skiptrace)" "\n"
    "-nprint n    print n samples in stream mode when verbose" "\n"
    "-sine file   write sine wave to file" "\n"
    "-freq f      produce sine wave with frequency f" "\n"
    "-nsamples n  write n samples for sine wave" "\n"
    "-moretraces  try to write more than one trace" "\n"
    "-modifiers s pass string as format modifiers to the I/O stream" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"verbose",arg_no,"-"},
    // 2: parameter line
    {"pl",arg_yes,"-"},
    // 3: debug output
    {"DEBUG",arg_no,"-"},
    // 4: parameter line
    {"read",arg_yes,"-"},
    // 5: detailed help
    {"xhelp",arg_no,"-"},
    // 6: test limiting of comment lines
    {"cmmntlim",arg_no,"-"},
    // 7: read through stream
    {"stream",arg_yes,"-"},
    // 8: count only
    {"countonly",arg_no,"-"},
    // 9: print n samples in stream mode
    {"nprint",arg_yes,"10"},
    // 10: nsamples for sine wave
    {"nsamples",arg_yes,"1000"},
    // 11: frequency of sine wave
    {"frequency",arg_yes,"25."},
    // 12: write sine wave
    {"sine",arg_yes,"-"},
    // 13: try to write more than one trace
    {"moretraces",arg_no,"-"},
    // 14: pass format modifiers
    {"modifiers",arg_yes,""},
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
  if (cmdline.optset(0) || cmdline.optset(5))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    if (cmdline.optset(5))
    {
      cerr << "\n";
      cerr << "help functions:" << endl;
      datrw::supported_data_types(cerr);
      cerr << "\n";
      datrw::online_help(cerr);
    }
    exit(0);
  }

  Options opt;

  opt.verbose=cmdline.optset(1);
  opt.parameterline=cmdline.optset(2);
  opt.parameterlinefile=cmdline.string_arg(2);
  opt.debug=cmdline.optset(3);
  opt.datafile=cmdline.optset(4);
  opt.datafilename=cmdline.string_arg(4);
  opt.testcommentlimit=cmdline.optset(6);
  opt.streamread=cmdline.optset(7);
  opt.streamreadfile=cmdline.string_arg(7);
  opt.countonly=cmdline.optset(8);
  opt.nprint=cmdline.int_arg(9);
  opt.nsamples=cmdline.int_arg(10);
  opt.frequency=cmdline.double_arg(11);
  opt.writesine=cmdline.optset(12);
  opt.sinefilename=cmdline.string_arg(12);
  opt.moretraces=cmdline.optset(13);
  opt.modifiers=cmdline.string_arg(14);

  /*======================================================================*/
  // go
  //

  if ( opt.parameterline )
  {
    cout << "read parameter line from file " << opt.parameterlinefile << endl;
    std::ifstream ifs(opt.parameterlinefile.c_str());
    std::string line;
    getline(ifs, line);
    cout << "read parameter line is:" << endl;
    cout << line << endl;
    datrw::seife::ParameterLine paraline(line, opt.debug);
    cout << "reproduced as:" << endl;
    cout << paraline.line() << endl;
    cout << "123456789012345678901234567890123456789012345678901234567890" <<
      endl;
    cout << "0        1         2         3         4         5         6" <<
      endl;
  } // if ( opt.parameterline )

  /*----------------------------------------------------------------------*/

  if ( opt.datafile )
  {
    cout << "read data file " << opt.datafilename << endl;
    std::ifstream ifs(opt.datafilename.c_str());
    datrw::seife::Header header(ifs);
    datrw::seife::ParameterLine parameters=header.parameters();
    ::sff::FREE free=header.comments();
    datrw::Tdseries series=
      datrw::util::readasciidouble<datrw::Tdseries>(ifs, parameters.nsamples(), 
                                                    "seifetest");
    cout << "the file is reproduced as:" << std::endl;
    parameters.format(datrw::seife::seife_standard_format);
    header.set(parameters);
    free.append(SEIFETEST_VERSION);
    header.set(free);
    header.write(cout);
    datrw::seife::write_series(cout, series);
  } // if ( opt.datafile )

  /*----------------------------------------------------------------------*/

  if (opt.testcommentlimit)
  {
    cout << "test automatic limiting of comment lines" << endl;
    ::sff::FREE comments;
    std::ostringstream oss;
    for (unsigned int i=0; i<60; ++i)
    {
      if (oss.str().length()>60) { oss.str(""); }
      oss << i << " ";
      comments.append(oss.str());
    }
    cout << "FREE block prepared is:" << endl;
    comments.write(cout);
    datrw::seife::Header header;
    header.set(comments);
    cout << "seife header produced from this:" << endl;
    header.write(cout);
  } // if (opt.testcommentlimit)

  /*----------------------------------------------------------------------*/

  if (opt.streamread)
  {

    std::ifstream ifs(opt.streamreadfile.c_str());
    datrw::iseifestream is(ifs, opt.modifiers);
    if (is.hasfree())
    {
      if (opt.verbose)
      {
        cout << "file FREE block:" << endl;
        is.free().write(std::cout);
      }
      else
      {
        cout << "file has FREE block" << endl;
      }
    }
    else
    {
      cout << "file has no FREE block" << endl;
    }
    if (is.hassrce())
    {
      if (opt.verbose)
      {
        cout << "file SRCE line:" << endl;
        cout << is.srce().line() << endl;;
      }
      else
      {
        cout << "file has SRCE line" << endl;
      }
    }
    else
    {
      cout << "file has no SRCE line" << endl;
    }
    datrw::Tfseries fseries;

    if (opt.countonly)
    {
      is.skipseries();
    }
    else
    {
      is >> fseries;
    }
    if (is.hasfree())
    {
      if (opt.verbose)
      {
        cout << "trace FREE block:" << endl;
        is.free().write(std::cout);
      }
      else
      {
        cout << "trace has FREE block" << endl;
      }
    }
    else
    {
      cout << "trace has no FREE block" << endl;
    }
    if (is.hasinfo())
    {
      if (opt.verbose)
      {
        cout << "trace INFO line:" << endl;
        cout << is.info().line() << endl;;
      }
      else
      {
        cout << "trace has INFO line" << endl;
      }
    }
    else
    {
      cout << "trace has no INFO line" << endl;
    }
    std::cout << is.wid2().line() << std::endl;
    if ((!opt.countonly) && (opt.verbose))
    {
      int npr=
        opt.nprint < int(fseries.size()/2) ?
        opt.nprint : fseries.size()/2;
      for (int i=0; i<npr; ++i)
      { std::cout << i << " " << fseries(i) << std::endl; }
      std::cout << " ... " << std::endl;
      for (int i=fseries.size()-npr; i<int(fseries.size()); ++i)
      { std::cout << i << " " << fseries(i) << std::endl; }
    }
  } // if (opt.streamread)

  /*----------------------------------------------------------------------*/

  if (opt.writesine)
  {
    cout << "write sine wave to file " << opt.sinefilename << endl;
    std::ofstream ofs(opt.sinefilename.c_str(), datrw::oseifestream::openmode);
    datrw::oseifestream os(ofs, opt.modifiers, opt.debug);

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

    int ntraces=1;
    if (opt.moretraces) { ntraces=10; }
    for (int itrace=0; itrace<ntraces; ++itrace)
    {
      cout << "write trace #" << itrace+1 << endl;
      aff::Series<double> sine(opt.nsamples);
      const double dt=2.e-3*(itrace+4);
      for (int isample=0; isample<opt.nsamples; ++isample)
      {
        sine(sine.f()+isample)=(itrace+2)*sin(2.*3.141592653589793115998
                                              *opt.frequency*dt*isample);
      }

      cout << "trace FREE data can ";
      if (!os.handlestracefree()) { cout << "NOT "; }
      cout << "be handled" << endl;

      cout << "INFO data can ";
      if (!os.handlesinfo()) { cout << "NOT "; }
      cout << "be handled" << endl;

      ::sff::FREE free;
      free.append(SEIFETEST_VERSION);
      ::sff::WID2 wid2;
      wid2.dt=dt;
      wid2.nsamples=sine.size();
      wid2.date=libtime::now();
      os << wid2;
      if (os.handlestracefree()) { os << free; }
      os << sine;
    }
  } // if (opt.writesine)

}

/* ----- END OF seifetest.cc ----- */
