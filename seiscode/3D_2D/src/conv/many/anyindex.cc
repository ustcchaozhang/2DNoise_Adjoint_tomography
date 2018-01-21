/*! \file anyindex.cc
 * \brief create index of data files
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/09/2004
 * 
 * create index of data files
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * This file is part of the conv/many suite.
 *
 * The conv/many suite is free software; you can redistribute it and/or modify
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
 *  - 06/09/2004   V1.0   Thomas Forbriger
 *  - 23/12/2004   V1.1   added debugging output
 *  - 26/04/2006   V1.2   pass debugging flag to reading class
 *  - 11/05/2006   V1.3   skip series and flush output
 *  - 13/04/2010   V1.4   use WIDX for index files
 *  - 03/05/2010   V1.5   output WIDX to terminal for debugging
 *  - 25/11/2010   V1.6   
 *                       - use correct open mode upon ifstream open
 * 
 * ============================================================================
 */
#define ANYINDEX_VERSION \
  "ANYINDEX   V1.6   create index of data files"

#include <fstream>
#include <iostream>
#include <vector>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <tfxx/stringfunc.h>
#include <datrwxx/readany.h>
#include <datrwxx/debug.h>
#include <sffostream.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, debug, overwrite, append, readsamples;
  std::string format;
}; // struct Options

typedef std::vector<std::string> Tvecofstrings;

typedef datrw::Tdseries Tseries;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    ANYINDEX_VERSION "\n"
    "usage: anyindex [-v] [-a] [-o] [-t type] [-D] [-r]" "\n"
    "                [-Fbonjer] [-Fsff] [-Fpdas] [-Fhpmo]" "\n"
    "                file [file ...] outfile" "\n"
    "   or: anyindex --help|-h" "\n"
    "   or: anyindex --fhelp type" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-fhelp type  print online help for file format \"type\"\n"
    "\n"
    "-v           be verbose" "\n"
    "-a           append to outfile" "\n"
    "-o           overwrite outfile" "\n"
    "-r           explicitely read all samples (default: skip)" "\n"
    "-t format    select data format or use one of the shortcuts below" "\n"
    "-Fsff        input format is SFF" "\n"
    "-Fhpmo       input format is multiplexed ASCII format" "\n"
    "             defined by WG for BFO HP-MO data acquisition system" "\n"
    "-Fpdas       input format is PDAS100 (i.e. DaDisp)" "\n"
    "-Fbonjer     input format is defined by K. Bonjer for" "\n"
    "             K2 ASCII data" "\n"
    "-D           produce debugging output" "\n"
    "file ...     file(s) to read data from" "\n"
    "outfile      out file (index)" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: append mode
    {"a",arg_no,"-"},
    // 3: overwrite mode
    {"o",arg_no,"-"},
    // 4: original SFF data
    {"Fsff",arg_no,"-"},
    // 5: WG's multiplexed ASCII data (BFO format)
    {"Fhpmo",arg_no,"-"},
    // 6: PDAS 100 data (aka DaDisp)
    {"Fpdas",arg_no,"-"},
    // 7: K. Bonjer's ASCII data
    {"Fbonjer",arg_no,"-"},
    // 8: select any data type
    {"t",arg_yes,"-"},
    // 9: debugging output
    {"DEBUG",arg_no,"-"},
    // 10: read samples
    {"read",arg_no,"-"},
    // 11: format specific online help
    {"fhelp",arg_yes,"sff"},
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
  if (cmdline.optset(0) || cmdline.optset(11))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl << endl;
    if (cmdline.optset(11))
    {
      datrw::online_help(cmdline.string_arg(11), cerr, true);
    }
    else
    {
      datrw::supported_data_types(cerr);
    }
    exit(0);
  }

  // options
  Options opt;

  opt.verbose=cmdline.optset(1);
  opt.append=cmdline.optset(2);
  opt.overwrite=cmdline.optset(3);
  opt.format=datrw::anyID(datrw::Fsff);
  if (cmdline.optset(5)) { opt.format=datrw::anyID(datrw::Fhpmo); }
  if (cmdline.optset(6)) { opt.format=datrw::anyID(datrw::Fpdas); }
  if (cmdline.optset(7)) { opt.format=datrw::anyID(datrw::Fbonjer); }
  if (cmdline.optset(8)) { opt.format=cmdline.string_arg(8); }
  opt.debug=cmdline.optset(9);
  opt.readsamples=cmdline.optset(10);

  tfxx::string::trimws(opt.format);
  if (opt.verbose) 
  { cout << "selected data type: " << opt.format << endl; }

  TFXX_assert(opt.format.find(" ")==std::string::npos,
             "ERROR: using white space in format modifiers will fail "
             "when reading index file");

  // setup vector of input filenames and read command line
  Tvecofstrings infiles;
  std::string outfile;
  TFXX_assert(cmdline.extra(), "ERROR: missing input file!");
  outfile=cmdline.next();
  // collect input files
  while (cmdline.extra()) 
  { 
    infiles.push_back(outfile);
    outfile=cmdline.next();
  }
  TFXX_assert((infiles.size()>0), "ERROR: missing output file!");

  // open output file
  std::ios_base::openmode ofopenmode;
  if (opt.append)
  {
    ofopenmode=std::ios_base::out|std::ios_base::app|std::ios_base::app;
  }
  else 
  { 
    if (!opt.overwrite)
    {
      std::ifstream file(outfile.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    ofopenmode=std::ios_base::out;
  }
  std::ofstream os(outfile.c_str(), ofopenmode);

  Tvecofstrings::const_iterator infile=infiles.begin(); 
  while( infile!=infiles.end())
  {
    if (opt.verbose)
    { cout << "** open next file: " << *infile << endl; }
    std::ifstream ifs(infile->c_str(),
                      datrw::ianystream::openmode(opt.format));
    datrw::ianystream is(ifs, opt.format, opt.debug);

    int ntrace=0;
    if (is.last() && opt.debug) { cout << "LAST!" << endl; }
    while(is.good())
    {
      ntrace++;
      if (opt.verbose)
      { cout << "**** read next trace: #" << ntrace << endl; }
      if (opt.readsamples)
      {
        datrw::Tdseries series;
        is >> series;
      }
      else
      {
        is.skipseries();
      }
      sff::WID2 wid2;
      is >> wid2;
      os << *infile << " " << ntrace << " " << 
        opt.format << endl;
      DATRW_debug(opt.debug, "anyindex main()",
                    "prepare WIDX line");
      if (opt.debug) { sff::verbose(std::cerr, wid2); }
      std::string widx=sff::WIDXline(wid2, opt.debug);
      DATRW_debug(opt.debug, "anyindex main()",
                    widx);
      os << widx << endl;
      os.flush();
    }
    ++infile;
  }
}

/* ----- END OF anyindex.cc ----- */
