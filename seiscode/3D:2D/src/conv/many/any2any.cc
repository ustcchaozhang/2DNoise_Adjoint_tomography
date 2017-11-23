/*! \file any2any.cc
 * \brief conversion between libdatrwxx data file formats
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 26/11/2010
 * 
 * conversion between libdatrwxx data file formats
 * 
 * Copyright (c) 2010 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 26/11/2010   V1.0   Thomas Forbriger
 * 
 * ============================================================================
 */
#define ANY2ANY_VERSION \
  "ANY2ANY   V1.0   conversion between libdatrwxx data file formats"

#include <iostream>
#include <fstream>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>
#include <tfxx/rangestring.h>
#include <tfxx/rangelist.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <datrwxx/datatypes.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, overwrite, debug, integer, single;
  bool verboseconversion;
  std::string inputformat, outputformat;
}; // struct Options

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    ANY2ANY_VERSION "\n"
    "usage: any2any [--verbose] [--overwrite] [--integer] [--single]" "\n"
    "               [--itype t] [--otype t] [--vconversion]" "\n"
    "               outfile infile [t:T] [f:F] [infile [t:T] [f:F] ... ]" "\n"
    "   or: any2any --help|-h" "\n"
    "   or: any2any --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "Options may be abbreviated to a short string as long as they" "\n"
    "remain unique. \"-v\" is identical to \"--verbose\"." "\n"
    "\n"
    "--verbose    be verbose" "\n"
    "--vconversion be verbose on type conversion" "\n"
    "--overwrite  overwrite output if file already exists" "\n"
    "--integer    use integer values for copying" "\n"
    "--single     use single precision floats for copying" "\n"
    "             default data type for copying is double presicion floats\n"
    "--itype t    standard format of input file(s) (see below)" "\n"
    "--otype t    data format of output file (see below)" "\n"
    "\n"
    "outfile      output data file name" "\n"
    "infile       input data file name" "\n"
    "\n"
    "File specific options:" "\n"
    "t:T          select specfic traces from input file" "\n"
    "             T can be a list of traces like \"1,4,5\" or" "\n"
    "             a range like \"6-19\" or mixed like \"5,8,12-17,20\"" "\n"
    "f:F          specify file format (overrides --itype setting)" "\n"
    "\n"
    "The output format might not be able to store all header information" "\n"
    "from the input data." "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: print help
    {"xhelp",arg_no,"-"},
    // 2: verbose mode
    {"verbose",arg_no,"-"},
    // 3: overwrite output
    {"overwrite",arg_no,"-"},
    // 4: overwrite output
    {"itype",arg_yes,"sff"},
    // 5: overwrite output
    {"otype",arg_yes,"sff"},
    // 6: overwrite output
    {"DEBUG",arg_no,"-"},
    // 7: read integer data
    {"integer",arg_no,"-"},
    // 8: read single precision data
    {"single",arg_no,"-"},
    // 9: read single precision data
    {"vconversion",arg_no,"-"},
    {NULL}
  };
  
  static const char tracekey[]="t";
  static const char formatkey[]="f";
  
  // define commandline argument modifier keys
  static const char* cmdlinekeys[]={tracekey, formatkey, 0};

  /*----------------------------------------------------------------------*/
  // action!

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0) || cmdline.optset(1))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    datrw::supported_data_types(cerr);
    if (cmdline.optset(1))
    {
      cerr << endl;
      datrw::online_help(cerr);
    }
    exit(0);
  }

  // extract command line options
  Options opt;
  opt.verbose=cmdline.optset(2);
  opt.overwrite=cmdline.optset(3);
  opt.inputformat=cmdline.string_arg(4);
  opt.outputformat=cmdline.string_arg(5);
  opt.debug=cmdline.optset(6);
  opt.integer=cmdline.optset(7);
  opt.single=cmdline.optset(8);
  opt.verboseconversion=cmdline.optset(9);

  if (opt.verbose) { cout << ANY2ANY_VERSION "\n"; }

  if (opt.verboseconversion)
  {
    ::datrw::report_type_conversion();
  }

  // extract commandline arguments
  TFXX_assert(cmdline.extra(), "missing output file");
  std::string outfile=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing input file");
  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);
  if ((arguments.size()>1) && opt.verbose)
  {
    cout << "NOTICE: file specific information (SRCE line and file FREE)\n" 
      <<    "        of the second and subsequent files might get lost!\n";
  }

  /*----------------------------------------------------------------------*/
  // full action!

  // check whether output file exists
  if (opt.verbose)
  {
    cout << "open output file " << outfile 
      << " with format " << opt.outputformat << endl;
  }
  if (!opt.overwrite) { datrw::abort_if_exists(outfile); }
  std::ofstream ofs(outfile.c_str(),
                    datrw::oanystream::openmode(opt.outputformat));
  datrw::oanystream os(ofs, opt.outputformat, opt.debug);
    
  if (opt.verbose) {
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
  }

  // cycle through all input files
  // -----------------------------
  bool firstfile=true;
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile != arguments.end())
  {
    // open input file
    std::string inputformat=opt.inputformat;
    if (infile->haskey(formatkey))
    { inputformat=infile->value(formatkey); }
    if (opt.verbose) 
    { 
      cout << "open input file " << infile->name
        << " of format " << inputformat << endl; 
    }
    std::ifstream ifs(infile->name.c_str(),
                      datrw::ianystream::openmode(inputformat));
    datrw::ianystream is(ifs, inputformat, opt.debug);


    /*----------------------------------------------------------------------*/
    // pass file header data

    // handle file header
    if (firstfile)
    {
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
    } // if (firstfile)

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

    /*----------------------------------------------------------------------*/

    // cycle through traces of input file
    // ----------------------------------
    // setup trace selection
    typedef tfxx::RangeList<int> Trangelist;
    bool doselect=infile->haskey(tracekey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(tracekey));
    int itrace=0;
    while (is.good())
    {
      ++itrace;
      if ((!doselect) || traceranges.contains(itrace))
      {
        if (opt.verbose)
        { std::cout << "  copy trace #" << itrace << std::endl; }

        datrw::Tfseries fseries;
        datrw::Tdseries dseries;
        datrw::Tiseries iseries;

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

        TFXX_debug(opt.debug, "any2any: trace header data availability",
                   "  is.hasinfo(): " << is.hasinfo() <<
                   "  is.hasfree(): " << is.hasfree());

        // pass INFO
        if (is.hasinfo())
        {
          if (os.handlesinfo())
          {
            sff::INFO infoline;
            is >> infoline;
            os << infoline;
            TFXX_debug(opt.debug, "any2any: INFO data",
                       infoline.line());
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

        // write output
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
      } // if ((!doselect) || traceranges.contains(itrace))
      else
      {
        if (opt.verbose)
        { std::cout << "  skip trace #" << itrace << std::endl; }
        is.skipseries();
      } // if ((!doselect) || traceranges.contains(itrace))

      // end of this trace
    } // while (is.good())

    // end of this file
    ++infile;
  } // while (infile != arguments.end())

}

/* ----- END OF any2any.cc ----- */
