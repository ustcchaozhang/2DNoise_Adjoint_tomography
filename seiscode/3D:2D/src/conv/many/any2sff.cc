/*! \file any2sff.cc
 * \brief read any data format
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 31/03/2004
 * 
 * read any data format
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
 *  - 31/03/2004   V1.0   Thomas Forbriger
 *  - 21/04/2004   V1.1   write series first - then header
 *  - 27/03/2006   V1.2   provide GSE scaling mode
 *  - 25/11/2010   V1.3   
 *                       - use correct open mode upon ifstream open
 * 
 * ============================================================================
 */
#define ANY2SFF_VERSION \
  "ANY2SFF   V1.3   read any data format"

#include <fstream>
#include <iostream>
#include <vector>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <datrwxx/readany.h>
#include <sffostream.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, debug, overwrite, gsescaling;
  std::string format;
}; // struct Options

typedef std::vector<std::string> Tvecofstrings;

typedef datrw::Tdseries Tseries;

int main(int iargc, char* argv[])
{

  // options
  Options opt;

  // define usage information
  char usage_text[]=
  {
    ANY2SFF_VERSION "\n"
    "usage: any2sff [-v] [-o] [-Fbonjer] [-Fsff] [-Fpdas] [-Fhpmo]" "\n"
    "               [-t type] [-GSE]" "\n"
    "               file [ file [ ... ]] sff-file" "\n"
    "   or: any2sff --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "file         input file of selected format" "\n"
    "sff-file     output file in SFF" "\n"
    "\n"
    "-v           be verbose" "\n"
    "-o           overwrite output file" "\n"
    "-t type      select input data format" "\n"
    "-Fsff        input format is SFF" "\n"
    "-Fhpmo       input format is multiplexed ASCII format" "\n"
    "             defined by WG for BFO HP-MO data acquisition system" "\n"
    "-Fpdas       input format is PDAS100 (i.e. DaDisp)" "\n"
    "-Fbonjer     input format is defined by K. Bonjer for" "\n"
    "             K2 ASCII data" "\n"
    "-GSE         use GSE compatible scaling, when writing data" "\n"
    "             notice: the dynamic range will be reduced to integer values" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: original SFF data
    {"Fsff",arg_no,"-"},
    // 3: WG's multiplexed ASCII data (BFO format)
    {"Fhpmo",arg_no,"-"},
    // 4: PDAS 100 data (aka DaDisp)
    {"Fpdas",arg_no,"-"},
    // 5: K. Bonjer's ASCII data
    {"Fbonjer",arg_no,"-"},
    // 6: debug mode
    {"D",arg_no,"-"},
    // 7: overwrite mode
    {"o",arg_no,"-"},
    // 8: input data format 
    {"t",arg_yes,"-"},
    // 9: GSE scaling mode
    {"GSE",arg_no,"-"},
    {NULL}
  };

  /* ---------------------------------------------------------------------- */

  cerr << ANY2SFF_VERSION "\n"
    "******************************************************************\n"
    "THIS PROGRAM IS OBSOLETE\n"
    "any2sff is no longer maintained and may vanish in the future\n"
    "consider to use any2any instead\n"
    "******************************************************************\n";

  /* ---------------------------------------------------------------------- */

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
    datrw::supported_data_types(cerr);
    datrw::online_help(cerr);
    exit(0);
  }

  opt.verbose=cmdline.optset(1);
  opt.format=datrw::anyID(datrw::Fsff);
  if (cmdline.optset(3)) { opt.format=datrw::anyID(datrw::Fhpmo); }
  if (cmdline.optset(4)) { opt.format=datrw::anyID(datrw::Fpdas); }
  if (cmdline.optset(5)) { opt.format=datrw::anyID(datrw::Fbonjer); }
  opt.debug=cmdline.optset(6);
  opt.overwrite=cmdline.optset(7);
  if (cmdline.optset(8)) { opt.format=cmdline.string_arg(8); }
  opt.gsescaling=cmdline.optset(9);

  /* ---------------------------------------------------------------------- */

  sff::FREE filefree;
  filefree.append(ANY2SFF_VERSION);
  filefree.append("input files:");
  Tvecofstrings infiles;
  std::string outfile;
  TFXX_assert(cmdline.extra(), "ERROR: missing input file!");
  outfile=cmdline.next();
  // collect input files
  while (cmdline.extra()) 
  { 
    infiles.push_back(outfile);
    filefree.append(outfile);
    outfile=cmdline.next();
  }
  TFXX_assert((infiles.size()>0), "ERROR: missing output file!");

  filefree.append("output file:");
  filefree.append(outfile);
  filefree.append("any SRCE line or file FREE block in input data is ignored!");

  if (opt.verbose) { filefree.write(cout); }

  // check if output file exists and open
  if (!opt.overwrite)
  {
    std::ifstream file(outfile.c_str(),std::ios_base::in);
    TFXX_assert((!file.good()),"ERROR: output file exists!");
  }
  std::ofstream ofs(outfile.c_str());
  sff::SFFostream<Tseries> os(ofs, opt.debug);
  if (opt.gsescaling)
  {
    if (opt.verbose)
    { cout << "select GSE compatible scaling" << endl; }
    os.setnormmode(sff::NM_one);
  }
  os << filefree;

  Tvecofstrings::const_iterator infile=infiles.begin(); 
  while( infile!=infiles.end())
  {
    if (opt.verbose)
    { cout << "** open next file: " << *infile << endl; }
    std::ifstream ifs(infile->c_str(), datrw::ianystream::openmode(opt.format));
    datrw::ianystream is(ifs, opt.format, opt.debug);

    while(is.good())
    {
      if (opt.verbose)
      { cout << "**** convert next trace" << endl; }
      datrw::Tdseries series;
      if (opt.debug) { cerr << "DEBUG: read series" << endl; }
      is >> series;
      if (opt.debug) { cerr << "DEBUG: write series" << endl; }
      os << series;
      sff::WID2 wid2;
      if (opt.debug) { cerr << "DEBUG: read WID2" << endl; }
      is >> wid2;
      if (opt.debug) { cerr << "DEBUG: write WID2" << endl; }
      os << wid2;
      if (opt.debug) { cerr << "DEBUG: read and write INFO"
        << " if available" << endl; }
      if (is.hasinfo()) { sff::INFO info; is >> info; os << info; }
      if (opt.debug) { cerr << "DEBUG: read and write FREE"
        << " if available" << endl; }
      if (is.hasfree()) { sff::FREE free; is >> free; os << free; }
    }
    ++infile;
  }

}

/* ----- END OF any2sff.cc ----- */
