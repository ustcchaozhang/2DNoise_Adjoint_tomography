/*! \file sehefixx.cc
 * \brief set header field
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/11/2016
 * 
 * set header field
 * 
 * Copyright (c) 2013, 2016 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 07/01/2013   V1.0   Thomas Forbriger
 *                 V1.1   set time
 *  - 14/11/2016   V1.2   refer to cooset for coordinates
 * 
 * ============================================================================
 */
#define SEHEFIXX_VERSION \
  "SEHEFIXX   V1.2   set header field"

#include <iostream>
#include <fstream>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <libtime++.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <datrwxx/datatypes.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  std::string itype, otype;
  bool verbose, overwrite, debug, integer, single;
  std::string newchannel, newstation, newinstype, newauxid, newtime;
  bool        setchannel, setstation, setinstype, setauxid, settime;
}; // struct Options

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SEHEFIXX_VERSION "\n"
    "usage: sehefixx infile outfile [-verbose] [-overwrite]\n"
    "                [-itype f] [-otype f]" "\n"
    "                [-ss station] [-sc channel] [-si instype]\n"
    "                [-sa auxid] [-st time]\n"
    "                [-integer] [-float]\n"
    "   or: sehefixx --help|-h" "\n"
    "   or: sehefixx --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-xhelp       print I/O format details\n"
    "\n"
    "infile       name of input data file\n"
    "outfile      name of output data file\n"
    "\n"
    "-verbose     be verbose\n"
    "-overwrite   overwrite existing output file\n"
    "-itype f     input file data format type\'f\'\n"
    "-otype f     output file data format type\'f\'\n"
    "-integer     use integer sample values rather than double\n"
    "-float       use single precision sample values rather than double\n"
    "-ss station  set station header field to \'station\'\n"
    "-ss channel  set channel header field to \'channel\'\n"
    "-ss instype  set instype header field to \'instype\'\n"
    "-ss auxid    set auxid header field to \'auxid\'\n"
    "-st time     set time of first sample\n"
    "\n"
    "Use program \'cooset\' to modify source or station coordinates.\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"verbose",arg_no,"-"},
    // 2: overwrite output
    {"overwrite",arg_no,"-"},
    // 3: input file type
    {"itype",arg_yes,"sff"},
    // 4: output file type
    {"otype",arg_yes,"sff"},
    // 5: set station
    {"sstaion",arg_yes,"-"},
    // 6: set channel
    {"schannel",arg_yes,"-"},
    // 7: set instype
    {"sinstype",arg_yes,"-"},
    // 8: set auxid
    {"sauxid",arg_yes,"-"},
    // 9: extra help
    {"xhelp",arg_no,"-"},
    // 10: integer values
    {"integer",arg_no,"-"},
    // 11: single precision values
    {"float",arg_no,"-"},
    // 12: set time
    {"stime",arg_yes,"-"},
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
  if (cmdline.optset(0) || cmdline.optset(9))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    cerr << endl;
    datrw::supported_data_types(cerr);
    cerr << endl;
    cerr << libtime::usage_time_format_string;
    cerr << endl;
    if (cmdline.optset(9))
    {
      cerr << endl;
      datrw::online_help(cerr);
    }
    exit(0);
  }
  
  // extract command line options
  Options opt;
  opt.debug=false;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.itype=cmdline.string_arg(3);
  opt.otype=cmdline.string_arg(4);
  opt.setstation=cmdline.optset(5);
  opt.newstation=cmdline.string_arg(5);
  opt.setchannel=cmdline.optset(6);
  opt.newchannel=cmdline.string_arg(6);
  opt.setinstype=cmdline.optset(7);
  opt.newinstype=cmdline.string_arg(7);
  opt.setauxid=cmdline.optset(8);
  opt.newauxid=cmdline.string_arg(8);
  opt.integer=cmdline.optset(10);
  opt.single=cmdline.optset(11);
  opt.settime=cmdline.optset(12);
  opt.newtime=cmdline.string_arg(12);

  TFXX_assert(cmdline.extra(), "missing input file name"); 
  std::string infile=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing output file name"); 
  std::string outfile=cmdline.next();

  /*----------------------------------------------------------------------*/
  // full action!
  // check whether output file exists
  if (opt.verbose)
  {
    cout << "open output file " << outfile 
      << " with format " << opt.otype << endl;
  }
  if (!opt.overwrite) { datrw::abort_if_exists(outfile); }
  std::ofstream ofs(outfile.c_str(),
                    datrw::oanystream::openmode(opt.otype));
  datrw::oanystream os(ofs, opt.otype, opt.debug);

  if (opt.verbose) 
  { 
    cout << "open input file " << infile
      << " of format " << opt.itype << endl; 
  }
  std::ifstream ifs(infile.c_str(),
                    datrw::ianystream::openmode(opt.itype));
  datrw::ianystream is(ifs, opt.itype, opt.debug);


  /*----------------------------------------------------------------------*/
  // pass file header data

  if (is.hasfree())
  {
    if (os.handlesfilefree())
    {
      sff::FREE filefree;
      is >> filefree;
      os << filefree;
    }
    else
    {
      if (opt.verbose)
      {
        cout << "  file FREE block is discarded." << endl;
      }
    }
  } // if (is.hasfree())

  if (is.hassrce())
  {
    if (os.handlessrce())
    {
      sff::SRCE srceline;
      is >> srceline;
      os << srceline;
    }
    else
    {
      if (opt.verbose)
      {
        cout << "  SRCE line is discarded." << endl;
      }
    }
  }

  int itrace=0;
  while (is.good())
  {
    ++itrace;
    if (opt.verbose) { std::cout << "  edit trace #" << itrace << std::endl; }

    // check output data format
    switch (os.seriestype()) {
      case datrw::Fint:
        if (!opt.integer)
        {
          cout << "  WARNING: converting floating point data to integer!" 
            << endl;
        }
        break;
      case datrw::Ffloat:
        if (!(opt.single || opt.integer))
        {
          cout << "  WARNING: "
            "converting double precision to single precision data!" 
            << endl;
        }
        break;
      case datrw::Fdouble:
        // that's just fine
        break;
      case datrw::Fall:
        // that's just fine
        break;
      default:
        TFXX_abort("output stream uses unknown variable type!");
    } // switch (os.seriestype())

    datrw::Tiseries iseries;
    datrw::Tfseries fseries;
    datrw::Tdseries dseries;

    // read time series
    if (opt.integer) 
    {
      TFXX_assert(is.providesi(),
                  "ERROR: input data is not provided as integer values");
      is >> iseries;
    }
    else if (opt.single)
    {
      TFXX_assert(is.providesf(),
                  "ERROR: input data is not provided as "
                  "single precision floats");
      is >> fseries;
    }
    else
    {
      TFXX_assert(is.providesd(),
                  "ERROR: input data is not provided as "
                  "double precision floats");
      is >> dseries;
    }

    // pass WID2
    sff::WID2 wid2;
    is >> wid2;

    if (opt.setstation) { wid2.station=opt.newstation; }
    if (opt.setchannel) { wid2.channel=opt.newchannel; }
    if (opt.setinstype) { wid2.instype=opt.newinstype; }
    if (opt.setauxid) { wid2.auxid=opt.newauxid; }
    if (opt.settime) { wid2.date=libtime::TAbsoluteTime(opt.newtime); }

    os << wid2;

    // pass INFO
    if (is.hasinfo())
    {
      if (os.handlesinfo())
      {
        sff::INFO infoline;
        is >> infoline;
        os << infoline;
      }
      else
      {
        if (opt.verbose)
        {
          cout << "  INFO line is discarded." << endl;
        }
      }
    }

    // pass trace FREE
    if (is.hasfree())
    {
      if (os.handlestracefree())
      {
        sff::FREE freeblock;
        is >> freeblock;
        freeblock.append("Edited with: ");
        freeblock.append(SEHEFIXX_VERSION);
        os << freeblock;
      }
      else
      {
        if (opt.verbose)
        {
          cout << "  trace FREE block is discarded." << endl;
        }
      }
    }

    // write time series
    if (opt.integer) 
    {
      os << iseries;
    }
    else if (opt.single)
    {
      os << fseries;
    }
    else
    {
      os << dseries;
    }
  }
}

/* ----- END OF sehefixx.cc ----- */
