/*! \file sigval.cc
 * \brief extract values from input signals
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 01/12/2016
 * 
 * extract values from input signals
 * 
 * Copyright (c) 2009, 2016 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * SIGVAL is free software; you can redistribute it and/or modify
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
 *  - 06/05/2009   V1.0   Thomas Forbriger (thof)
 *  - 12.12.2010   V1.1   return signal duration
 *  - 10/02/2011   V1.2   added option to calculate overall rms
 *  - 10/02/2011   V1.3   write trace coordinates
 *  - 04/03/2011   V1.4   report trace offset
 *  - 08/09/2011   V1.5   support format modifiers
 *  - 07/11/2011   V1.6   provide source coordinates too
 *  - 15/04/2014   V1.7   new patterns %HT and %SI
 *  - 01/12/2016   V1.8   fix: do not calculate offset if either SRCE or INFO
 *                        is missing in input file
 * 
 * ============================================================================
 */
#define SIGVAL_VERSION \
  "SIGVAL   V1.8   extract values from input signals"

#include <iostream>
#include <tfxx/commandline.h>
#include <aff/series.h>
#include <aff/iterator.h>
#include <aff/dump.h>
#include <aff/seriesoperators.h>
#include <aff/functions/avg.h>
#include <aff/functions/rms.h>
#include <aff/functions/min.h>
#include <aff/functions/max.h>
#include <aff/functions/sqrsum.h>
#include <aff/subarray.h>
#include <tsxx/tsxx.h>
#include <tsxx/tapers.h>
#include <tsxx/filter.h>
#include <tsxx/wid2timeseries.h>
#include <tsioxx/inputoperators.h>
#include <fstream>
#include <tfxx/error.h>
#include <tfxx/rangestring.h>
#include <tfxx/xcmdline.h>
#include <tfxx/misc.h>
#include <tfxx/handle.h>
#include <datrwxx/readany.h>
#include <sstream>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  std::string inputformat;
  std::string outputformat;
  bool reporttotalrms;
}; // struct Options

// values type to be used for samples
typedef double Tvalue;

// time series
typedef aff::Series<Tvalue> Tseries;

// full featured time series file
typedef ts::sff::File<Tseries> Tfile;

typedef ts::TDsfftimeseries Ttimeseries;
typedef Ttimeseries::Tseries Tseries;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SIGVAL_VERSION "\n"
    "usage: sigval [-format s] [-type f] file [t:l] [file [t:l] ...]" "\n"
    "              [-rms]\n"
    "   or: sigval --help|-h" "\n"
    "   or: sigval --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-format s   define a format string for output lines" "\n"
    "-type f     define file format of input files" "\n"
    "-rms        Calculate total rms value over all traces assuming that\n"
    "            all traces have the same physical sample units.\n"
    "\n"
    "Patterns that may be used in format descriptions are:" "\n"
    "%F     file name" "\n"
    "%NT    number of trace in file" "\n"
    "%COO   trace coordinates" "\n"
    "%SCO   source coordinates" "\n"
    "%OFF   trace offset" "\n"
    "%D     date of first sample" "\n"
    "%T     time of first sample" "\n"
    "%UT    time of first sample including microsecond" "\n"
    "%HT    hierarchical time string (can be used as control parameter\n"
    "       for programs selecting a time window - like resaseda)\n"
    "%L     time duration of signal" "\n"
    "%SI    sampling interval in seconds" "\n"
    "%NS    number of samples" "\n"
    "%S     station identifier" "\n"
    "%C     channel identifier" "\n"
    "%A     auxiliary identifier" "\n"
    "%I     instrument identifier" "\n"
    "%MEAN  signal average" "\n"
    "%MIN   minimum value in signal" "\n"
    "%MAX   maximum value in signal" "\n"
    "%PPA   peak-to-peak amplitude" "\n"
    "%RMS   rms of signal" "\n"
    "%XT    tab character" "\n"
    "%%     will be replaced by a literal \'%\'" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: output format
    {"format",arg_yes,"%F(t#%NT) %S %C"},
    // 2: input file format
    {"type",arg_yes,"sff"},
    // 3: extended help for formats
    {"xhelp",arg_no,"-"},
    // 4: extended help for formats
    {"rms",arg_no,"-"},
    {NULL}
  };

  static const char tracekey[]="t";

  // define commandline argument modifier keys
  static const char* cmdlinekeys[]={tracekey, 0};

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
    exit(0);
  }

  if (cmdline.optset(3))
  {
    cerr << usage_text << endl;
    datrw::online_help(cerr);
    exit(0);
  }

  Options opt;
  opt.outputformat=cmdline.string_arg(1);
  opt.inputformat=cmdline.string_arg(2);
  opt.reporttotalrms=cmdline.optset(4);

  // extract commandline arguments
  TFXX_assert(cmdline.extra(), "missing input file");
  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);

  /*======================================================================*/
  // start processing
    
  // two variables needed to calculate the overall rms value
  double overallrms=0., totaltime=0.;

  // cycle through all input files
  // -----------------------------
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile != arguments.end())
  {
    // open input file
    std::ifstream ifs(infile->name.c_str());
    datrw::ianystream is(ifs, opt.inputformat);
    sff::SRCE srce;
    is >> srce;

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
        Tseries series;
        is >> series;
        sff::WID2 wid2;
        is >> wid2;
        sff::INFO info;
        is >> info;

        std::string retval=opt.outputformat;

        std::ostringstream oss;
        
        // pattern to be replaced by file name
        retval=tfxx::string::patsubst(retval, "%F",
                                      infile->name);

        // pattern to be replaced by trace coordinate
        oss.str("");
        if (info.cs == sff::CS_cartesian)
        {
          oss << "cartesian coordinates:";
        }
        else if (info.cs == sff::CS_spherical)
        {
          oss << "spherical coordinates:";
        }
        else
        {
          oss << "unknown coordinate system:";
        }
        oss << " c1=" << info.cx;
        oss << " c2=" << info.cy;
        oss << " c3=" << info.cz;
        retval=tfxx::string::patsubst(retval, "%COO",
                                      tfxx::string::trimws(oss.str()));

        // pattern to be replaced by source coordinate
        oss.str("");
        if (srce.cs == sff::CS_cartesian)
        {
          oss << "cartesian source coordinates:";
        }
        else if (srce.cs == sff::CS_spherical)
        {
          oss << "spherical source coordinates:";
        }
        else
        {
          oss << "unknown source coordinate system:";
        }
        oss << " s1=" << srce.cx;
        oss << " s2=" << srce.cy;
        oss << " s3=" << srce.cz;
        retval=tfxx::string::patsubst(retval, "%SCO",
                                      tfxx::string::trimws(oss.str()));

        // pattern to be replaced by trace offset
        if (is.hassrce() && is.hasinfo())
        {
          oss.str("");
          oss << " r=" << sff::offset(srce, info) << "m";
          retval=tfxx::string::patsubst(retval, "%OFF",
                                        tfxx::string::trimws(oss.str()));
        }
        else
        {
          TFXX_assert(retval.find("%OFF") == std::string::npos,
           "offset cannot be calculated, because either SRCE or INFO\n"
           "is missing in input file");
        } // else, if (is.hassrce() && is.hasinfo())

        // pattern to be replaced by trace number
        oss.str("");
        oss << itrace;
        retval=tfxx::string::patsubst(retval, "%NT",
                                      tfxx::string::trimws(oss.str()));

        std::string timestring=wid2.date.timestring();
        // pattern to be replaced by date of first samples
        retval=tfxx::string::patsubst(retval, "%D",
                                      timestring.substr(4,10));
        // pattern to be replaced by time of first samples
        retval=tfxx::string::patsubst(retval, "%T",
                                      timestring.substr(15,8));
        // pattern to be replaced by time of first samples
        retval=tfxx::string::patsubst(retval, "%UT",
                                      timestring.substr(15,15));
        // pattern to be replaced by time of first samples
        // using an hierarchical time string
        retval=tfxx::string::patsubst(retval, "%HT",
                                      wid2.date.hierarchicalstring());
        // pattern to be replaced by time duration of signal
        libtime::TRelativeTime duration=
          libtime::double2time(wid2.dt)*(series.size()-1);
        retval=tfxx::string::patsubst(retval, "%L",
                                      duration.timestring());
        // pattern to be replaced by sampling interval in seconds
        oss.str("");
        oss << wid2.dt;
        retval=tfxx::string::patsubst(retval, "%SI",
                                      tfxx::string::trimws(oss.str()));
        // pattern to be replaced by number of samples
        oss.str("");
        oss << series.size();
        retval=tfxx::string::patsubst(retval, "%NS",
                                      tfxx::string::trimws(oss.str()));
        // pattern to be replaced by station identifier
        retval=tfxx::string::patsubst(retval, "%S",
                                      tfxx::string::trimws(wid2.station));
        // pattern to be replaced by channel identifier
        retval=tfxx::string::patsubst(retval, "%C",
                                      tfxx::string::trimws(wid2.channel));
        // pattern to be replaced by auxiliary identifier
        retval=tfxx::string::patsubst(retval, "%A",
                                      tfxx::string::trimws(wid2.auxid));
        // pattern to be replaced by instrument identifier
        retval=tfxx::string::patsubst(retval, "%I",
                                      tfxx::string::trimws(wid2.instype));

        // pattern to be replaced by instrument identifier
        retval=tfxx::string::patsubst(retval, "%XT", "\t");

        // average
        double avg=aff::func::avg(series);
        oss.str("");
        oss << avg;
        retval=tfxx::string::patsubst(retval, "%MEAN",
                                      tfxx::string::trimws(oss.str()));
        // maximum
        double max=aff::func::max(series);
        oss.str("");
        oss << max;
        retval=tfxx::string::patsubst(retval, "%MAX",
                                      tfxx::string::trimws(oss.str()));
        // minimum
        double min=aff::func::min(series);
        oss.str("");
        oss << min;
        retval=tfxx::string::patsubst(retval, "%MIN",
                                      tfxx::string::trimws(oss.str()));
        // peak-to-peak
        double ppamp=(max-min);
        if (ppamp < 0) { ppamp *= -1; }
        oss.str("");
        oss << ppamp;
        retval=tfxx::string::patsubst(retval, "%PPA",
                                      tfxx::string::trimws(oss.str()));
        // rms
        double rms=aff::func::rms(series);
        oss.str("");
        oss << rms;
        retval=tfxx::string::patsubst(retval, "%RMS",
                                      tfxx::string::trimws(oss.str()));
        
        // double-per-cent to be replaced by per-cent
        retval=tfxx::string::patsubst(retval, "%%", "%");

        cout << retval << endl;
          
        // cumulative sum of signal power
        overallrms+=aff::func::sqrsum(series);
        // cumulative sum of seismogram time
        totaltime += (wid2.dt*series.size());

  /*----------------------------------------------------------------------*/
      }
      else
      {
        is.skipseries();
      }
    }
    
    // go to next file
    ++infile;
  }

  if (opt.reporttotalrms)
  {
    // calculate overall rms from total signal power
    overallrms=std::sqrt(overallrms/totaltime);
    cout << "\n"
      << "The overall rms value is: " << overallrms << "\n"
      << "  This value is calculated by integration of the total signal\n"
      << "  power of all traces over time. This integral is divided by\n"
      << "  the total time of integration and the square root of this\n"
      << "  is presented as the overall rms values. Notice that this\n"
      << "  implies that all trace have the same physical units for\n"
      << "  their sample values. If not, the outcome is meaningless."
      << endl;
  }

} // main

/* ----- END OF sigval.cc ----- */
