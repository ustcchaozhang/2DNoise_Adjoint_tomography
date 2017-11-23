/*! \file thiesdl1test.cc
 * \brief Test Thies DL1 data reading module
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 13/09/2011
 * 
 * Test Thies DL1 data reading module
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
 *  - 13/09/2011   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define THIESDL1TEST_VERSION \
  "THIESDL1TEST   V1.0   Test Thies DL1 data reading module"

#include <iostream>
#include <fstream>
#include <tfxx/commandline.h>
#include <datrwxx/thiesdl1.h>
#include <datrwxx/thiesdl1file.h>
#include <aff/dump.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

void reportfilled(const datrw::thiesdl1::File& f)
{
  cout << "file structure is ";
  if (!f.isproperlyfilled()) { cout << "NOT "; }
  cout << "properly filled" << endl;
} // void reportfilled(const datrw::thiesdl1::file& f)

/*----------------------------------------------------------------------*/

void reportheader(const datrw::thiesdl1::FileHeader& header)
{
  std::cout << "File Header contents:\n"
            << "---------------------" << endl;
  std::cout << "  earliest: " <<
    header.earliestdate.timestring() << std::endl;
  std::cout << "  latest:   " << 
    header.latestdate.timestring() << std::endl;
  std::cout << "  creation: " <<
    header.creationdate.timestring() << std::endl;
  std::cout << "  expected initial:  >>" <<
    header.expectedinitialdataline << "<<" << std::endl;
  std::cout << "  expected final:    >>" <<
    header.expectedfinaldataline << "<<" << std::endl;
  std::cout << "  initial:           >>" <<
    header.initialdataline << "<<" << std::endl;
  ::sff::verbose(cout, header.lines);
  std::cout << "end of File Header" << endl;
} // void reportheader(const datrw::thiesdl1::Header& header)

/*----------------------------------------------------------------------*/

struct Options {
  bool readheader, readfile;
  bool tolerateredundant, toleratewrongtime;
  bool streamread, verbose;
  bool countonly, integer;
  int nprint;
  std::string modifiers;
};

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    THIESDL1TEST_VERSION "\n"
    "usage: thiesdl1test [-v] [-h] [-f] [-trs] [-twt] [-stream] [-count]\n"
    "                    [-int] [-np n] [-modifiers m]\n"
    "                    file [file ...]" "\n"
    "   or: thiesdl1test --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-v           be verbose\n"
    "-h           read file header and report entries\n"
    "-f           read complete file and report entries\n"
    "-trs         tolerate redundant samples\n"
    "-twt         tolerate wrong time\n"
    "-stream      use stream interface\n"
    "-count       only count samples in stream mode\n"
    "-int         use integer reading in stream mode\n"
    "-np n        display n samples in stream mode\n"
    "-modifiers m use modifiers m in stream mode\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: read file header
    {"h",arg_no,"-"},
    // 3: read complete file
    {"f",arg_no,"-"},
    // 4: tolerate redundant samples
    {"trs",arg_no,"-"},
    // 5: tolerate wrong time
    {"twt",arg_no,"-"},
    // 6: use stream reading
    {"stream",arg_no,"-"},
    // 7: count samples
    {"count",arg_no,"-"},
    // 8: use integer input
    {"int",arg_no,"-"},
    // 9: use integer input
    {"np",arg_yes,"10"},
    // 10: use integer input
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
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.readheader=cmdline.optset(2);
  opt.readfile=cmdline.optset(3);
  opt.tolerateredundant=cmdline.optset(4);
  opt.toleratewrongtime=cmdline.optset(5);
  opt.streamread=cmdline.optset(6);
  opt.countonly=cmdline.optset(7);
  opt.integer=cmdline.optset(8);
  opt.nprint=cmdline.int_arg(9);
  opt.modifiers=cmdline.string_arg(10);

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
  */

  // cycle through all file names
  while (cmdline.extra()) 
  { 
    std::string filename=cmdline.next();
    cout << "process file " << filename << endl; 

    /*----------------------------------------------------------------------*/

    if (opt.readheader)
    {
      cout << "  read header\n"
        <<    "  -----------" << endl;
      std::ifstream ifs(filename.c_str(), datrw::ithiesdl1stream::openmode); 
      datrw::thiesdl1::FileHeader header=
        datrw::thiesdl1::readheader(ifs);
      reportheader(header);
    } // if (opt.readheader)

    /*----------------------------------------------------------------------*/

    if (opt.readfile)
    {
      cout << "  read file\n"
        <<    "  ---------" << endl;
      std::ifstream ifs(filename.c_str(), datrw::ithiesdl1stream::openmode); 
      datrw::thiesdl1::File file;
      file.tolerateredundant(opt.tolerateredundant);
      file.toleratewrongtime(opt.toleratewrongtime);
      reportfilled(file);
      file.readwithheader(ifs);
      reportfilled(file);
      reportheader(file.header());
      datrw::Tdseries series=file.dseries();
      aff::dump(series);
    } // if (opt.readheader)

    /*----------------------------------------------------------------------*/

    if (opt.streamread)
    {

      std::ifstream ifs(filename.c_str());
      datrw::ithiesdl1stream is(ifs, opt.modifiers);
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
      datrw::Tiseries iseries;

      if (opt.countonly)
      {
        is.skipseries();
      }
      else
      {
        if (opt.integer)
        {
          is >> iseries;
        }
        else
        {
          is >> fseries;
        }
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
        if (opt.integer)
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
        else
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
      }
      DATRW_assert(is.last(), "last flag was not set");
    } // if (opt.streamread)

  } // while (cmdline.extra())
}

/* ----- END OF thiesdl1test.cc ----- */
