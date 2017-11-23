/*! \file hpmotest.cc
 * \brief test hpmo reading functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2004
 * 
 * test hpmo reading functions
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
#define HPMOTEST_VERSION \
  "HPMOTEST   V1.0   test hpmo reading functions"

#include <fstream>
#include <iostream>
#include <tfxx/commandline.h>
#include <datrwxx/error.h>
#include <datrwxx/readhpmo.h>
#include <datrwxx/hpmo.h>
#include <datrwxx/readany.h>
#include <aff/dump.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, stage1, stage2, stage3, extractsamples, stage4;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    HPMOTEST_VERSION "\n"
    "usage: hpmotest [-v] [-s1] [-s2] [-s3] [-x] filename" "\n"
    "   or: hpmotest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "filename     file to read" "\n"
    "-v           be verbose" "\n"
    "-s1          test reading functions of stage 1" "\n"
    "-s2          test reading functions of stage 2" "\n"
    "-s3          test reading functions of stage 3" "\n"
    "-s4          test reading functions of stage 4" "\n"
    "-x           extract samples in stage 3 and 4" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: test stage 1
    {"s1",arg_no,"-"},
    // 3: test stage 2
    {"s2",arg_no,"-"},
    // 4: test stage 3
    {"s3",arg_no,"-"},
    // 5: extract samples
    {"x",arg_no,"-"},
    // 6: test stage 4
    {"s4",arg_no,"-"},
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

  /* dummy operation: print option settings
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
  opt.stage1=cmdline.optset(2);
  opt.stage2=cmdline.optset(3);
  opt.stage3=cmdline.optset(4);
  opt.extractsamples=cmdline.optset(5);
  opt.stage4=cmdline.optset(6);

  DATRW_assert(cmdline.extra(), "missing filename!");
  std::string infile=cmdline.next();

  if (opt.verbose) { cout << "reading file " << infile << endl; }

/*----------------------------------------------------------------------*/

  if (opt.stage1)
  {
    if (opt.verbose) 
    {
      cout << endl << "TEST: stage 1" << endl
                   << "=============" << endl;
    }
    std::ifstream ifs(infile.c_str());
    DATRW_assert(ifs.good(), "invalid file!");
    datrw::hpmo::Header header;
    datrw::hpmo::SampleBlock samples;
    bool hot=true;
    while (ifs.good() && hot)
    {
      try {
        ifs >> header;
      } catch (datrw::hpmo::NoHeaderException)
      {
        cout << "End of file!" << endl;
        hot=false;
      }
      if (hot)
      {
        if (opt.verbose) { cout << header; }
        if (ifs.good())
        {
          ifs >> samples;
          if (opt.verbose) { cout << samples; }
        }
      }
    }
  }

/*----------------------------------------------------------------------*/

  if (opt.stage2)
  {
    if (opt.verbose) 
    {
      cout << endl << "TEST: stage 2" << endl
                   << "=============" << endl;
    }
    std::ifstream ifs(infile.c_str());
    DATRW_assert(ifs.good(), "invalid file!");
    if (opt.verbose) 
    {
      cout << endl << "read whole file" << endl;
    }
    datrw::hpmo::Tvecofblocks blocks
      =datrw::hpmo::readfile(ifs, opt.verbose);
    if (opt.verbose)
    {
      cout << endl << "read " << blocks.size() << " blocks" << endl;
    }
    datrw::hpmo::Tvecofblocks::const_iterator I(blocks.begin());
    while (I!=blocks.end())
    {
      cout << *I;
      ++I;
    }
  }

/*----------------------------------------------------------------------*/

  if (opt.stage3)
  {
    if (opt.verbose) 
    {
      cout << endl << "TEST: stage 3" << endl
                   << "=============" << endl;
    }
    std::ifstream ifs(infile.c_str());
    DATRW_assert(ifs.good(), "invalid file!");
    datrw::ihpmostream is(ifs, opt.verbose);
    sff::FREE filefree;
    is >> filefree;
    cout << filefree;
    while (is.good())
    {
      datrw::Tfseries data;
      if (opt.extractsamples)
      {
        is >> data;
      }
      else
      {
        is.skipseries();
      }
      sff::WID2 wid2line;
      cout << std::endl << "next trace: " << std::endl;
      is >> wid2line;
      cout << wid2line;
      sff::FREE tracefree;
      is >> tracefree;
      cout << tracefree;
      if (opt.extractsamples) { DUMP( data ); }
    }
  }

/*----------------------------------------------------------------------*/

  if (opt.stage4)
  {
    if (opt.verbose) 
    {
      cout << endl << "TEST: stage 4" << endl
                   << "=============" << endl;
    }
    std::ifstream ifs(infile.c_str());
    DATRW_assert(ifs.good(), "invalid file!");
    datrw::ianystream is(ifs, datrw::Fhpmo);
    sff::FREE filefree;
    is >> filefree;
    cout << filefree;
    while (is.good())
    {
      datrw::Tfseries data;
      if (opt.extractsamples)
      {
        is >> data;
      }
      else
      {
        is.skipseries();
      }
      sff::WID2 wid2line;
      cout << std::endl << "next trace: " << std::endl;
      is >> wid2line;
      cout << wid2line;
      sff::FREE tracefree;
      is >> tracefree;
      cout << tracefree;
      if (opt.extractsamples) { DUMP( data ); }
    }
  }
}

/* ----- END OF hpmotest.cc ----- */
