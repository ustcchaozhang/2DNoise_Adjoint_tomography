/*! \file teseco.cc
 * \brief time series corrections
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 29/11/2016
 * 
 * time series corrections
 * 
 * Copyright (c) 2005, 2016 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 13/07/2005   V1.0   Thomas Forbriger
 *  - 13/11/2012   V1.1   support additional file formats
 *  - 30/07/2015   V1.2   provide option -datetolerance
 *  - 28/11/2016   V1.3   allow scaling of signal
 *  - 29/11/2016   V1.4   fix: make use of datetolerance
 *                 V1.5   provide new tolerance option -trim
 *
 * ============================================================================
 */
#define TESECO_VERSION \
  "TESECO   V1.5   time series corrections/ linear combination of signals"

#include <fstream>
#include <sstream>
#include <iostream>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>
#include <tfxx/rangestring.h>
#include <tsioxx/operators.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <aff/series.h>
#include <aff/seriesoperators.h>

using std::cout;
using std::cerr;
using std::endl;

typedef aff::Series<double> Tseries;

struct Options {
  bool verbose, overwrite, add, debug, trimseries;
  double datetolerance;
  std::string inputformat, outputformat;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    TESECO_VERSION "\n"
    "usage: teseco [-v] [-o] [-a] [-type type] [-Type type]" "\n"
    "              [-datetolerance t] [-trim]" "\n"
    "              outfile signal [t:T] [f:F]\n"
    "              infile [t:T] [f:F] [infile [t:T] [f:F] ... ]" "\n"
    "   or: teseco --help|-h" "\n"
    "   or: teseco --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "The program reads time series traces and adds or subtracts other" "\n"
    "time series scaled by individual factors. This way a barometric" "\n"
    "pressure correction or magentic field correction may be applied" "\n"
    "to seismic recordings." "\n"
    "\n"
    "All input traces (all traces in signal and correction files) are\n"
    "expected to cover the same time window. This program is not able\n"
    "to handle time series containing gaps properly.\n"
    "\n"
    "outfile      output filename" "\n"
    "signal       signal filename (multiple traces may be selected;\n"
    "             each correction will be applied to each of them)\n"
    "infile       name of file containing correction; scaled traces" "\n"
    "             will be subtraced from (or added to) the signals" "\n"
    "             t:T select traces T, where T may be any range" "\n"
    "                 specification like \'3-4\' or \'5,6,7-12,20\'" "\n"
    "             f:F each of the traces will be scaled by the factor \'F\'\n"
    "                 (default: F=1.)" "\n"
    "\n"
    "-v           verbose mode" "\n"
    "-o           overwrite existing output file" "\n"
    "-a           rather add than subtract correction signals" "\n"
    "-type type   input file type (default: sff)" "\n"
    "-Type type   output file type (default: sff)" "\n"
    "-datetolerance f\n"
    "             set a tolerance level f for comparison of date of\n"
    "             input time series; dates will be considered as different\n"
    "             if they differ by more than a fraction f of the average\n"
    "             sampling interval\n"
    "-trim        trim all series to the shortest series provided\n"
    "             if not set, providing series of different number of samples\n"
    "             is considered to be an error\n"
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
    // 3: add mode
    {"a",arg_no,"-"},
    // 4: input file type
    {"type",arg_yes,"sff"},
    // 5: debug mode
    {"D",arg_no,"-"},
    // 6: output file type
    {"Type",arg_yes,"sff"},
    // 7: report details regarding data types
    {"xhelp",arg_no,"-"},
    // 8: report details regarding data types
    {"datetolerance",arg_yes,"0."},
    // 9: trim size of series to the least number of samples
    {"trim",arg_no,"-"},
    {NULL}
  };

  static const char tracekey[]="t";
  static const char factorkey[]="f";

  // define commandline argument modifier keys
  static const char* cmdlinekeys[]={
    tracekey, 
    factorkey,
    0
  }; // cmdlinekeys

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
    exit(0);
  }

  // help on file format details requested? 
  if (cmdline.optset(7))
  {
    cerr << usage_text << endl;
    cerr << endl;
    datrw::online_help(cerr); 
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.add=cmdline.optset(3);
  opt.inputformat=cmdline.string_arg(4);
  opt.debug=cmdline.optset(5);
  opt.outputformat=cmdline.string_arg(6);
  opt.datetolerance=cmdline.double_arg(8);
  opt.trimseries=cmdline.optset(9);

  if (opt.verbose)
  { cout << TESECO_VERSION << endl; }

  // extract commandline arguments
  TFXX_assert(cmdline.extra(), "missing output file");
  std::string outfile=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing signal file");
  tfxx::cmdline::Tparsed infiles=parse_cmdline(cmdline, cmdlinekeys);
  TFXX_assert((infiles.size()>1), "missing correction file");
  tfxx::cmdline::Filename sigfile=infiles.front();
  infiles.pop_front();

  /*======================================================================*/
  // start processing

  // open output file
  // ----------------
  if (opt.verbose) { cout << "open output file " << outfile << endl; }
  // check if output file exists and open
  if (!opt.overwrite)
  {
    std::ifstream file(outfile.c_str(),std::ios_base::in);
    TFXX_assert((!file.good()),"ERROR: output file exists!");
  }
  std::ios_base::openmode oopenmode
    =datrw::oanystream::openmode(opt.outputformat);
  std::ofstream ofs(outfile.c_str(), oopenmode);
  datrw::oanystream os(ofs, opt.outputformat);

  // prepare the largest number of samples found in one of the input series so
  // far
  unsigned int maxsamples=0;

  // prepare file FREE block
  sff::FREE filefree;
  filefree.append(TESECO_VERSION);

  // read signal file
  typedef ts::sff::File<Tseries> Tfile;
  typedef Tfile::Trangelist Trangelist;
  Tfile signal;
  {
    bool doselect=sigfile.haskey(tracekey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(sigfile.value(tracekey));

    if (opt.verbose) { cout << "open signal file " << sigfile.name << endl; }

    std::ios_base::openmode iopenmode
      =datrw::ianystream::openmode(opt.inputformat);
    std::ifstream ifs(sigfile.name.c_str(), iopenmode);
    datrw::ianystream is(ifs, opt.inputformat);

    // read selected traces
    if (doselect)
    { signal.read(is.idatstream(), traceranges, opt.verbose); }
    else
    { signal.read(is.idatstream(), opt.verbose); }

    TFXX_assert(signal.size()>0,
                "set of input signals is empty");

    // apply factor to each of the traces, if selected
    if (sigfile.haskey(factorkey))
    {
      double factor=1.;
      std::istringstream sfactor(sigfile.value(factorkey));
      sfactor >> factor;
      for (Tfile::Ttracevector::iterator isig=signal.begin();
           isig != signal.end(); ++isig)
      {
        *isig *= factor;
      }
    } // if (sigfile.haskey(factorkey))

    // set max samples if series are to be trimmed
    if (opt.trimseries)
    {
      for (Tfile::Ttracevector::iterator isig=signal.begin();
           isig != signal.end(); ++isig)
      {
        TFXX_assert(isig->size()>0, "missing samples in input signal");
        if ((maxsamples == 0) || (maxsamples > isig->size()))
        {
          maxsamples = isig->size();
        }
      }
    } // if (opt.trimseries)

  }
  TFXX_assert((signal.size()>0), "missing signal");
  signal.fileheader.append(filefree);


  // SFF WID2 compare
  sff::WID2compare compare(sff::Fdt | sff::Fdate);
  compare.setdatetolerance(opt.datetolerance);

  // cycle through all input files
  // -----------------------------
  tfxx::cmdline::Tparsed::const_iterator infile=infiles.begin();
  while (infile != infiles.end())
  {
    // open input file
    if (opt.verbose) { cout << "open correction file " 
                            << infile->name << endl; }

    std::ios_base::openmode iopenmode
      =datrw::ianystream::openmode(opt.inputformat);
    std::ifstream ifs(infile->name.c_str(), iopenmode);
    datrw::ianystream is(ifs, opt.inputformat);

    // cycle through traces of input file
    // ----------------------------------
    
    // setup trace selection
    typedef tfxx::RangeList<int> Trangelist;
    bool doselect=infile->haskey(tracekey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(tracekey));
    
    // setup correction factor
    double factor=1.;
    if (infile->haskey(factorkey))
    {
      std::istringstream sfactor(infile->value(factorkey));
      sfactor >> factor;
    }
    if (opt.add) { factor *= -1.; }

    int itrace=0;
    while (is.good())
    {
      ++itrace;
      if ((!doselect) || traceranges.contains(itrace))
      {
        { std::cout << "  process trace #" << itrace << std::endl; }
        Tseries series;
        is >> series;
        sff::WID2 wid2;
        is >> wid2;

        series *= factor;
        for (Tfile::Ttracevector::iterator isig=signal.begin();
             isig != signal.end(); ++isig)
        {
          // check matching headers
          if (!compare (isig->header.wid2(),wid2))
          {
            cerr << "ERROR: header signature mismatch:" << endl;
            cerr << "signal:" << endl;
            cerr << isig->header.wid2().line();
            cerr << "correction signal:" << endl;
            cerr << wid2.line();

            // check start date explicitly and provide a hint
            sff::WID2compare cmpdate(sff::Fdate);
            cmpdate.setdatetolerance(opt.datetolerance);
            if (!cmpdate(isig->header.wid2(),wid2))
            { 
              cerr << "*** date mismatch "
                   << "(consider to use option -datetolerance)" << endl; 
              cerr << "***            signal: ";
              cerr << isig->header.wid2().date.timestring() << endl;
              cerr << "*** correction signal: ";
              cerr << wid2.date.timestring() << endl;
            }
            TFXX_abort("bailing out...");
          } // if (!compare (isig->header.wid2(),wid2))

          // check matching size
          if (isig->size() != series.size())
          {
            if (opt.trimseries)
            {
              if (maxsamples > isig->size()) { maxsamples = isig->size(); }
              if (maxsamples > series.size()) { maxsamples = series.size(); }
              isig->setlastindex(isig->first()-1+maxsamples);
              series.setlastindex(series.first()-1+maxsamples);
            }
            else
            {
              cerr << "ERROR: inconsistent number of samples:" << endl;
              cerr << isig->size() << " samples for signal" << endl;
              cerr << isig->header.wid2().line();
              cerr << series.size() << " samples for correction signal" 
                << endl;
              cerr << wid2.line();
              cerr << "consider to use option -trim" << endl;
              TFXX_abort("bailing out...");
            } // else, if (opt.trimseries)
          } // (isig->size() != series.size())

          // apply correction
          *isig -= series;
        }

      }
      else
      {
        TFXX_debug(opt.debug, "main", "skip trace #" << itrace );
        if (opt.verbose)
        { std::cout << "     skip trace #" << itrace << std::endl; }
        is.skipseries();
      }
    }
    
    // go to next file
    ++infile;
  }
  os << signal;
}

/* ----- END OF teseco.cc ----- */
