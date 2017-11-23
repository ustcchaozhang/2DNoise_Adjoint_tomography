/*! \file gatherdiff.cc
 * \brief calculate difference between shot gathers
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 15/03/2005
 * 
 * calculate difference between shot gathers
 * 
 * Copyright (c) 2005 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 15/03/2005   V1.0   Thomas Forbriger
 *  - 09/03/2010   V1.1   added normalization
 *  - 22.12.2010   V1.2   implement libdatrwxx writing functions
 *  - 08.09.2011   V1.3   support format modifiers
 *  - 12.10.2012   V1.4   relax, ignore coordinates if users requests this
 * 
 * ============================================================================
 */
#define GATHERDIFF_VERSION \
  "GATHERDIFF   V1.4   calculate difference between shot gathers"

#include <fstream>
#include <iostream>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <aff/series.h>
#include <aff/seriesoperators.h>
#include <aff/functions/absmax.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, debug, normalize, overwrite, ignorecoo;
  std::string inputformat, outputformat;
  double srtol;
}; // struct Options

typedef aff::Series<double> Tseries;

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    GATHERDIFF_VERSION "\n"
    "usage: gatherdiff infile1 infile2 outfile [-v] [-n]" "\n"
    "                  [--overwrite] [--type f] [--Type f]" "\n"
    "                  [--srtol r] [--ignorecoo]" "\n"
    "   or: gatherdiff --help|-h" "\n"
    "   or: gatherdiff --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "This program takes two data files as input. It assumes" "\n"
    "that both contain full record gathers with identical recording" "\n"
    "parameters. Typically these will be two sets of synthetic" "\n"
    "seismograms that were calculated for different subsurface" "\n"
    "models. The residual of these two sets is calculated and written" "\n"
    "to the output, together with shot an receiver coordinates of" "\n"
    "the first input set." "\n"
    "\n"
    "infile1      name of first SFF input file" "\n"
    "infile2      name of second SFF input file" "\n"
    "outfile      name of output file (outfile=infile1-infile2)" "\n"
    "\n"
    "-v           be verbose" "\n"
    "-n           normalize input time series to their absolute maximum" "\n"
    "--overwrite  overwrite out put file (in case it already exists)" "\n"
    "--type f     input files have data file format \"f\"" "\n"
    "--Type f     output file will be written in data file format \"f\"" "\n"
    "--srtol r    set sampling rate tolerance for comaprison of" "\n"
    "             input data" "\n"
    "--ignorecoo  ignore coordinates when checking trace consistency\n"
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
    {"DEBUG",arg_no,"-"},
    // 3: normalize
    {"n",arg_no,"-"},
    // 4: overwrite output
    {"overwrite",arg_no,"-"},
    // 5: input data format
    {"type",arg_yes,"sff"},
    // 6: output data format
    {"Type",arg_yes,"sff"},
    // 7: output data format
    {"srtol",arg_yes,"0.001"},
    // 8: ignore coordinates
    {"ignorecoo",arg_no,"-"},
    {NULL}
  };

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    exit(0);
  }

  // no arguments? print usage...
  if (iargc<4) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.debug=cmdline.optset(2);
  opt.normalize=cmdline.optset(3);
  opt.overwrite=cmdline.optset(4);
  opt.inputformat=cmdline.string_arg(5);
  opt.outputformat=cmdline.string_arg(6);
  opt.srtol=cmdline.double_arg(7);
  opt.ignorecoo=cmdline.optset(8);

  TFXX_assert(cmdline.extra(), "ERROR: missing parameter: infile1!");
  std::string infile1=cmdline.next();
  TFXX_assert(cmdline.extra(), "ERROR: missing parameter: infile2!");
  std::string infile2=cmdline.next();
  TFXX_assert(cmdline.extra(), "ERROR: missing parameter: outfile!");
  std::string outfile=cmdline.next();

  if (opt.verbose)
  {
    cout << "tolerance when comparing sampling interval: " << opt.srtol <<
      "\n";
    cout << "input format: " << cmdline.string_arg(5) << "\n";
    cout << "output format: " << cmdline.string_arg(6) << "\n";
  }

  if (opt.verbose) { cout << "open input file (#1) " << infile1 << endl; }
  std::ios_base::openmode iopenmode
    =datrw::ianystream::openmode(opt.inputformat);
  std::ifstream ifs1(infile1.c_str(), iopenmode);
  datrw::ianystream is1(ifs1, opt.inputformat, opt.debug);
  sff::FREE filefree1;
  sff::SRCE source1;
  is1 >> filefree1;
  is1 >> source1;

  if (opt.verbose) { cout << "open input file (#2) " << infile2 << endl; }
  std::ifstream ifs2(infile2.c_str(), iopenmode);
  datrw::ianystream is2(ifs2, opt.inputformat, opt.debug);
  sff::FREE filefree2;
  sff::SRCE source2;
  is2 >> filefree2;
  is2 >> source2;

  if (!opt.ignorecoo)
  {
    TFXX_assert((source1.cs == source2.cs) &&
                (source1.cx == source2.cx) &&
                (source1.cy == source2.cy) &&
                (source1.cz == source2.cz),
                "ERROR: coordinates do not match!");
  }

  if (opt.verbose) { cout << "open output file " << outfile << endl; }
  std::ios_base::openmode oopenmode
    =datrw::oanystream::openmode(opt.outputformat);
  std::ofstream ofs(outfile.c_str(), oopenmode);
  datrw::oanystream os(ofs, opt.outputformat, opt.debug);
  sff::FREE outfilefree;
  outfilefree.append(GATHERDIFF_VERSION);
  outfilefree.append("input file #1: "+infile1);
  outfilefree.append("input file #2: "+infile2);
  outfilefree.append("input FREE #1:");
  outfilefree.append(filefree1);
  outfilefree.append("input FREE #2:");
  outfilefree.append(filefree2);
  if (os.handlessrce()) { os << source1; }
  if (os.handlesfilefree()) { os << outfilefree; }

  int itrace=0;
  while (is1.good() && is2.good())
  {
    if (opt.verbose) { cout << "process trace #" << ++itrace << endl; }

    sff::WID2 wid2in1, wid2in2, wid2out;
    sff::INFO infoin1, infoin2;
    sff::FREE tracefreein1, tracefreein2, outtracefree;
    Tseries series1, series2;
    is1 >> series1;
    is1 >> wid2in1;
    is1 >> infoin1;
    is1 >> tracefreein1;
    is2 >> series2;
    is2 >> wid2in2;
    is2 >> infoin2;
    is2 >> tracefreein2;
    outtracefree.append("input FREE #1:");
    outtracefree.append(tracefreein1);
    outtracefree.append("input FREE #2:");
    outtracefree.append(tracefreein2);
    TFXX_assert((wid2in1.date-source1.date) == (wid2in2.date-source2.date),
                "ERROR: times do not match!");
    double dtreldev=1-(wid2in1.dt/wid2in2.dt);
    if (dtreldev<0) { dtreldev *= -1.; }
    TFXX_debug(opt.debug, "gatherdiff",
                "dt infile 1: "<< wid2in1.dt
            << " dt infile 2: "<< wid2in2.dt
            << " relative deviation: "<< dtreldev);
    TFXX_assert(dtreldev <= opt.srtol,
                "ERROR: sampling interval does not match!");
    TFXX_assert(wid2in1.nsamples == wid2in2.nsamples,
                "ERROR: numer of samples does not match!");
    if (!opt.ignorecoo)
    {
      TFXX_assert((infoin1.cs == infoin2.cs) &&
                  (infoin1.cx == infoin2.cx) &&
                  (infoin1.cy == infoin2.cy) &&
                  (infoin1.cz == infoin2.cz),
                  "ERROR: coordinates do not match!");
    }
    wid2out=wid2in1;
    wid2out.channel="dif";
    if (opt.normalize)
    {
      outtracefree.append("input series are normalized to their absolute maximum of 1");
      double max1=aff::func::absmax(series1);
      double max2=aff::func::absmax(series2);
      if (opt.verbose)
      {
        cout << "normalize input time series to absolute maximum of 1" <<
          endl;
        cout << "absolute maximum of first input series: " << max1 << endl;
        cout << "absolute maximum of second input series: " << max2 << endl;
      }
      series1 /=  max1;
      series2 /=  max2;
    }
    Tseries outseries=series1-series2;
    os << wid2out;
    if (os.handlesinfo()) { os << infoin1; }
    if (os.handlesfilefree()) { os << outtracefree; }
    os << outseries;
  }
}

/* ----- END OF gatherdiff.cc ----- */
