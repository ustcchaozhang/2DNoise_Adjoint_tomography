/*! \file cooset.cc
 * \brief set coordinates
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 14/11/2016
 * 
 * set coordinates
 * 
 * Copyright (c) 2004, 2016 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 08/09/2004   V1.0   Thomas Forbriger
 *  - 14/11/2016   V1.1   support all libdatrwxx I/O formats
 *  - 01/12/2016   V1.2   create SRCE header, if not yet present
 *  - 18/08/2017   V1.3   new features:
 *                        - automatically convert comma to whitespace in
 *                          coordinate parameters
 *                        - support creation of info data
 * 
 * ============================================================================
 */
#define COOSET_VERSION \
  "COOSET   V1.3   set coordinates"

#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>
#include <algorithm>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <aff/series.h>
#include <aff/subarray.h>
#include <libtime++.h>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <tfxx/stringfunc.h>
#include <sffxx.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, overwrite, dostations, debug, single, integer;
  std::string stationsystem, stationfile, sourcetime, sourcecoo;
  bool sesot, sesoc, createsrce, createinfo;
  std::string itype, otype;
}; // struct Options

// structure to hold station coordinates
struct Station {
  std::string id;
  double x,y,z;
}; // struct Station

// vector of stations
typedef std::vector<Station> Tvecofstations;

// container to handle samples
typedef aff::Series<double> Tdseries;
typedef aff::Series<int>    Tiseries;
typedef aff::Series<float>  Tfseries;

/* ---------------------------------------------------------------------- */

std::ostream& operator<<(std::ostream& os, const sff::FREE& free)
{
  os << "contents of SFF FREE block:" << std::endl;
  sff::FREE::Tlines::const_iterator I=free.lines.begin();
  while (I != free.lines.end())
  {
    os << "  " << *I << std::endl;;
    ++I;
  }
  return(os);
} // std::ostream& operator<<(std::ostream& os, const sff::FREE& free)

/* ====================================================================== */

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    COOSET_VERSION "\n"
    "usage: cooset [-stc s] [-stf file] [-soc \"C x y z\"] [-sot date]" "\n"
    "              [-itype f] [-otype f] [-integer] [-float] [-screate]\n"
    "              [-v] [-o] infile outfile" "\n"
    "   or: cooset --help|-h" "\n"
    "   or: cooset --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "set source and station coordinates" "\n"
    "\n"
    "-xhelp       print I/O format details\n"
    "\n"
    "-stc s       station coordinates system" "\n"
    "-stf file    station coordinates file" "\n"
    "-soc C,x,y,z source coordinates" "\n"
    "-sot date    source time" "\n"
    "-screate     create source header, if not yet present\n"
    "             without this option, the program will only modify\n"
    "             existing source header data\n"
    "-icreate     create info trace header, if not yet present\n"
    "             without this option, the program will only modify\n"
    "             existing info trace header data\n"
    "-v           be verbose" "\n"
    "-o           overwrite" "\n"
    "-itype f     input file data format type\'f\'\n"
    "-otype f     output file data format type\'f\'\n"
    "-integer     use integer sample values rather than double\n"
    "-float       use single precision sample values rather than double\n"
    "\n"
    "station coordinate file:\n"
    "  Each line provides coordinates for one station. The coordinate\n"
    "  system to be used for all stations is selected by the argument\n"
    "  to option \"-stc\" on the command line. Each line has four columns:\n"
    "  1st column: name of station (will be matched against station name\n"
    "              found in trace header)\n"
    "  2nd column: value for x-coordinate or latitude field\n"
    "  3rd column: value for y-coordinate or longitude field\n"
    "  4th column: value for z-coordinate or height field\n"
    "\n"
    "Use program sehefixx to modify other header fields.\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: overwrite mode
    {"o",arg_no,"-"},
    // 3: station coordinates system
    {"stc",arg_yes,"S"},
    // 4: station coordinates file
    {"stf",arg_yes,"-"},
    // 5: source coordinates
    {"soc",arg_yes,"-"},
    // 6: source time
    {"sot",arg_yes,"-"},
    // 7: debug mode
    {"D",arg_no,"-"},
    // 8: input file type
    {"itype",arg_yes,"sff"},
    // 9: output file type
    {"otype",arg_yes,"sff"},
    // 10: extra help
    {"xhelp",arg_no,"-"},
    // 11: integer values
    {"integer",arg_no,"-"},
    // 12: single precision values
    {"float",arg_no,"-"},
    // 13: create source header if not yet present
    {"screate",arg_no,"-"},
    // 14: create info trace header if not yet present
    {"icreate",arg_no,"-"},
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
    datrw::supported_data_types(cerr);
    cerr << endl;
    cerr << libtime::usage_time_format_string;
    cerr << endl;
    if (cmdline.optset(10))
    {
      cerr << endl;
      datrw::online_help(cerr);
    }
    exit(0);
  }

  /*----------------------------------------------------------------------*/

  // read command line
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.stationsystem=cmdline.string_arg(3);
  opt.stationfile=cmdline.string_arg(4);
  opt.dostations=cmdline.optset(4);
  opt.sourcecoo=cmdline.string_arg(5);
  opt.sesoc=cmdline.optset(5);
  opt.sourcetime=cmdline.string_arg(6);
  opt.sesot=cmdline.optset(6);
  opt.debug=cmdline.optset(7);
  opt.itype=cmdline.string_arg(8);
  opt.otype=cmdline.string_arg(9);
  opt.integer=cmdline.optset(11);
  opt.single=cmdline.optset(12);
  opt.createsrce=cmdline.optset(13);
  opt.createinfo=cmdline.optset(14);

  TFXX_assert(cmdline.extra(),"ERROR: missing input file name!");
  std::string infile=cmdline.next();
  TFXX_assert(cmdline.extra(),"ERROR: missing output file name!");
  std::string outfile=cmdline.next();

  std::replace(opt.sourcecoo.begin(), opt.sourcecoo.end(), ',', ' ');

  /*----------------------------------------------------------------------*/

  // read station file if needed
  Tvecofstations station;
  if (opt.dostations)
  {
    if (opt.verbose) { cout << "read station list" << endl;}
    std::ifstream is(opt.stationfile.c_str());
    Station nextstation;
    is >> nextstation.id;
    while(is.good())
    {
      is >> nextstation.x >> nextstation.y >> nextstation.z;
      station.push_back(nextstation);
      if (opt.verbose)
      {
        cout << nextstation.id << " "
             << nextstation.x << " "
             << nextstation.y << " "
             << nextstation.z << endl; 
      }
      is >> nextstation.id;
    }
  }

  /*----------------------------------------------------------------------*/
  
  // open files
  if (opt.verbose) 
  { 
    cout << "open input file " << infile
      << " of format " << opt.itype << endl; 
  }
  std::ifstream ifs(infile.c_str());
  TFXX_assert(ifs.good(),"ERROR: opening input file");
  datrw::ianystream is(ifs, opt.itype, opt.debug);

  if (!opt.overwrite) { datrw::abort_if_exists(outfile); }

  if (opt.verbose)
  {
    cout << "open output file " << outfile 
      << " with format " << opt.otype << endl;
  }
  std::ofstream ofs(outfile.c_str());
  TFXX_assert(ofs.good(),"ERROR: opening output file");
  datrw::oanystream os(ofs, opt.otype, opt.debug);

  /*----------------------------------------------------------------------*/
  // pass file header data

  sff::FREE filefree;
  // read file FREE block
  if (is.hasfree())
  {
    is >> filefree;
    if (!os.handlesfilefree())
    {
      if (opt.verbose)
      {
        cout << "  Output format cannot handle FREE block." << endl;
        cout << "  File FREE block will be discarded." << endl;
      }
    }
  } // if (is.hasfree())

  // leave a note
  filefree.append(COOSET_VERSION);

  // read and modify SRCE line
  sff::SRCE srceline;
  if (is.hassrce())
  {
    is >> srceline;
    // modify file header
    filefree.append("input file has SRCE line"); 
  } // if (is.hassrce())
  if (opt.createsrce || is.hassrce())
  {
    if (opt.sesot)
    {
      filefree.append("set source time");
      libtime::TAbsoluteTime newsot(opt.sourcetime);
      srceline.date=newsot;
    }
    if (opt.sesoc)
    {
      filefree.append("set source coordinates");
      std::istringstream socospec(opt.sourcecoo);
      cout << opt.sourcecoo << endl;
      char syst;
      double x,y,z;
      socospec >> syst >> x >> y >> z;
      std::ostringstream message;
      message << "new source coordinates: " << syst << ","
        << x << "," << y << "," << z;
      srceline.cs=sff::coosysID(syst);
      srceline.cx=x;
      srceline.cy=y;
      srceline.cz=z;
      filefree.append(message.str());
    }
    if (opt.verbose) { sff::verbose(cout, srceline); }
  } // if (opt.createsrce || is.hassrce())

  // write file FREE block
  if (os.handlesfilefree())
  {
    os << filefree;
  }

  // write SRCE line
  if (is.hassrce() || opt.createsrce)
  {
    if (os.handlessrce())
    {
      os << srceline;
    }
    else
    {
      if (opt.verbose)
      {
        cout << "  Output format cannot handle SRCE line." << endl;
        cout << "  SRCE line is discarded." << endl;
      }
    }
  } // if (is.hassrce() || opt.createsrce)
  
  // report
  if (opt.verbose) 
  { 
    cout << endl << "output file FREE block:" << endl;
    cout << filefree; 
  }

  /*----------------------------------------------------------------------*/

  // handle traces
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
    os << wid2;

    // read trace FRRE
    sff::FREE freeblock;
    if (is.hasfree())
    {
      is >> freeblock;
      freeblock.append("Edited with: ");
      freeblock.append(COOSET_VERSION);
    } // if (is.hasfree())
      
    // modify INFO
    if (opt.createinfo || is.hasinfo())
    {
      if (os.handlesinfo())
      {
        sff::INFO infoline;
        is >> infoline;
        bool infoset=false;
        if (opt.dostations)
        {
          if (opt.verbose) { cout << "station: " << wid2.station << endl; }
          Station thestation;
          for (unsigned int i=0; i<station.size(); ++i)
          {
            thestation=station[i];
            if (opt.debug) { cout << "'" << thestation.id << "' '"
              << wid2.station << "'" << endl; }
            if (tfxx::string::trimws_end(thestation.id)
                == tfxx::string::trimws_end(wid2.station))
            { 
              infoset=true; 
              char syst=opt.stationsystem[0];
              std::ostringstream message;
              message << "new coordinates: " << syst << ","
                << thestation.x << "," << thestation.y << "," << thestation.z;
              infoline.cs=sff::coosysID(syst);
              infoline.cx=thestation.x;
              infoline.cy=thestation.y;
              infoline.cz=thestation.z;
              freeblock.append(message.str());
              i=station.size();
            }
          }
          if (infoset) 
          {
            if (opt.verbose)
            {
              cout << freeblock << endl;
            }
          }
          else
          { 
            cerr << "could not find station " << wid2.station << endl; 
            freeblock.append("could not find station!");
          }
        } // if (opt.dostations)
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

    ///////////////////
      
    // write trace FREE block
    if (is.hasfree())
    {
      if (os.handlestracefree())
      {
        os << freeblock;
      }
      else
      {
        if (opt.verbose)
        {
          cout << "  Output format cannot handle trace FREE block." << endl;
          cout << "  Trace FREE block is discarded." << endl;
        }
      }
    } // if (is.hasfree())

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

/* ----- END OF cooset.cc ----- */
