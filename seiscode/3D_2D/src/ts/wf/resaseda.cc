/*! \file resaseda.cc
 * \brief resample seismic data
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 07/11/2017
 * 
 * resample seismic data
 * 
 * Copyright (c) 2005, 2017 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * RESASEDA is free software; you can redistribute it and/or modify
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
 *  - 28/07/2005   V1.1   catch exception in case of non-matching window
 *  - 15/07/2008   V1.2   - provide options compatible to anyextract
 *                        - provide -edge option
 *  - 03/08/2011   V1.3   implement delay option
 *  - 14/09/2011   V1.4   implement sample offset windows
 *  - 21/11/2012   V1.5   provide different output formats
 *  - 07/11/2017   V1.6   provide option shrink
 * 
 * ============================================================================
 */
#define RESASEDA_VERSION \
  "RESASEDA   V1.6   resample seismic data"

#include <fstream>
#include <iostream>
#include <libtime++.h>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <tfxx/rangestring.h>
#include <tfxx/xcmdline.h>
#include <tfxx/misc.h>
#include <tfxx/handle.h>
#include <sffostream.h>
#include <tsxx/ipolin.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>

using std::cout;
using std::cerr;
using std::endl;
using libtime::TAbsoluteTime;
using libtime::TRelativeTime;

struct Options {
  bool verbose, overwrite, debug, delay, shrink;
  bool timefirstset, timelastset, timespanset, dtset, nset;
  TAbsoluteTime timefirst, timelast;
  TRelativeTime timespan, edge;
  double dt, delaytime;
  int n;
  std::string inputformat, outputformat;
  bool sampleoffsetbeginset, sampleoffsetendset;
  int sampleoffsetbegin, sampleoffsetend;
}; // struct Options

typedef ts::TDsfftimeseries Ttimeseries;
typedef Ttimeseries::Tseries Tseries;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    RESASEDA_VERSION "\n"
    "usage: resaseda [-tf time] [-tl time] [-n n] [-dt dt] [-ts time]" "\n"
    "                [-v] [-o] [-sf time] [-sl time] [-edge s]" "\n"
    "                [-delay d] [-type type] [-Type type]\n"
    "                [-sof n] [-sol n] [-shrink]" "\n"
    "                outfile infile [t:T] [infile [t:T] ...]" "\n"
    "   or: resaseda --help|-h" "\n"
    "   or: resaseda --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "outfile      output filename" "\n"
    "infile       input filename" "\n"
    "             t:T select traces T, where T may be any range" "\n"
    "                 specification like \'3-4\' or \'5,6,7-12,20\'" "\n"
    "\n"
    "-v           be verbose" "\n"
    "-o           overwrite output" "\n"
    "-tf time     time of first sample" "\n"
    "             (default: time of first input sample)" "\n"
    "-tl time     time of last sample" "\n"
    "-sf time     time of first sample" "\n"
    "             (default: time of first input sample)" "\n"
    "-sl time     time of last sample" "\n"
    "-sf and -sl are alternatives to -tf and -tl" "\n"
    "-n n         number of samples" "\n"
    "-ts time     duration of output time series" "\n"
    "-dt dt       new sampling interval in seconds" "\n"
    "-edge s      reduce time window by \"s\" seconds at the edges" "\n"
    "-type type   input format is \'type\'" "\n"
    "-Type type   output format is \'type\'" "\n"
    "-delay d     apply delay of \"d\" seconds by shifting the time\n"
    "             of the first sample in the input series\n" 
    "-sof n       define first sample index by offset n\n"
    "               positive sample index: define by offset from first sample\n"
    "               negative sample index: define by offset from last sample\n"
    "-sol n       define last sample index by offset n\n"
    "               positive sample index: define by offset from last sample\n"
    "               negative sample index: define by offset from first sample\n"
    "-shrink      if defined output time window is larger than time span\n"
    "               for which input data is available, reduce output time\n"
    "               window approrpiate for each input trace while maintaining\n"
    "               the defined ѕampling raster (useful for input data\n"
    "               containing gaps)\n"
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
    // 3: time of first sample
    {"tf",arg_yes,"1/1/1"},
    // 4: time of last sample
    {"tl",arg_yes,"1/1/1"},
    // 5: time span
    {"ts",arg_yes,"1/1/1"},
    // 6: number of samples
    {"n",arg_yes,"1"},
    // 7: sampling interval
    {"dt",arg_yes,"1."},
    // 8: DEBUG mode
    {"D",arg_no,"-"},
    // 9: input format
    {"type",arg_yes,"sff"},
    // 10: time of first sample
    {"sf",arg_yes,"1/1/1"},
    // 11: time of last sample
    {"sl",arg_yes,"1/1/1"},
    // 12: reduce time window
    {"edge",arg_yes,"0."},
    // 13: apply delay
    {"delay",arg_yes,"0."},
    // 14: define first sample by offset
    {"sof",arg_yes,"0"},
    // 15: define last sample by offset
    {"sol",arg_yes,"0"},
    // 16: output format
    {"Type",arg_yes,"sff"},
    // 17: extended help
    {"xhelp",arg_no,"-"},
    // 18: use shrink option with resample function
    {"shrink",arg_no,"-"},
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
    cerr << libtime::usage_time_format_string << endl;
    exit(0);
  }

  // help on file format details requested? 
  if (cmdline.optset(17))
  {
    cerr << usage_text << endl;
    cerr << endl;
    datrw::online_help(cerr); 
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.timefirstset=cmdline.optset(3);
  opt.timefirst=TAbsoluteTime(cmdline.string_arg(3));
  opt.timelastset=cmdline.optset(4);
  opt.timelast=TAbsoluteTime(cmdline.string_arg(4));
  opt.timespanset=cmdline.optset(5);
  opt.timespan=TRelativeTime(cmdline.string_arg(5));
  opt.nset=cmdline.optset(6);
  opt.n=cmdline.int_arg(6);
  opt.dtset=cmdline.optset(7);
  opt.dt=cmdline.double_arg(7);
  opt.debug=cmdline.optset(8);
  opt.inputformat=cmdline.string_arg(9);
  if (cmdline.optset(10))
  {
    opt.timefirstset=true;
    opt.timefirst=TAbsoluteTime(cmdline.string_arg(10));
  }
  if (cmdline.optset(11))
  {
    opt.timelastset=true;
    opt.timelast=TAbsoluteTime(cmdline.string_arg(11));
  }
  opt.edge=libtime::double2time(cmdline.double_arg(12));
  opt.delay=cmdline.optset(13);
  opt.delaytime=cmdline.double_arg(13);
  opt.sampleoffsetbeginset=cmdline.optset(14);
  opt.sampleoffsetendset=cmdline.optset(15);
  opt.sampleoffsetbegin=cmdline.int_arg(14);
  opt.sampleoffsetend=cmdline.int_arg(15);
  opt.outputformat=cmdline.string_arg(16);
  opt.shrink=cmdline.optset(18);
    
  // check options consistency
  {
    int nopt=0;
    if (opt.timelastset) ++nopt;
    if (opt.timespanset) ++nopt;
    if (opt.nset) ++nopt;
    if (opt.sampleoffsetendset) ++nopt;
    TFXX_assert(nopt<=1, 
                "only specify one of the options that "
                "limit the end of the output time window");
  }
    
  // check options consistency
  {
    int nopt=0;
    if (opt.timefirstset) ++nopt;
    if (opt.sampleoffsetbeginset) ++nopt;
    TFXX_assert(nopt<=1, 
                "only specify one of the options that "
                "limit the begin of the output time window");
  }

  if (opt.verbose)
  { cout << RESASEDA_VERSION << endl; }

  // extract commandline arguments
  TFXX_assert(cmdline.extra(), "missing output file");
  std::string outfile=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing input file");
  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);
  if ((arguments.size()>1) && opt.verbose)
  {
    cout << "NOTICE: file specific information (SRCE line and file FREE)" <<
      endl
      <<    "        will be taken from first file only!" << endl;
  }

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

  std::ofstream ofs(outfile.c_str(),
                    datrw::oanystream::openmode(opt.outputformat));
  datrw::oanystream os(ofs, opt.outputformat, opt.debug);

  // prepare file FREE block
  sff::FREE filefree;
  filefree.append(RESASEDA_VERSION);
  // set flag to process header of first input file
  bool firstfile=true;
  // cycle through all input files
  // -----------------------------
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile != arguments.end())
  {
    // open input file
    if (opt.verbose) { cout << "open input file " << infile->name << endl; }
    std::ifstream ifs(infile->name.c_str());
    datrw::ianystream is(ifs, opt.inputformat);
    // handle file header
    if (firstfile)
    {
      if (is.hasfree()) 
      { 
        sff::FREE infilefree;
        is >> infilefree;
        filefree.append("block read from first input file:");
        filefree.append(infilefree);
      }
      os << filefree;
      if (is.hassrce())
      {
        sff::SRCE insrceline;
        is >> insrceline;
        os << insrceline;
      }
    }

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
        TFXX_debug(opt.debug, "main", "process trace #" << itrace );
        if (opt.verbose)
        { std::cout << "  process trace #" << itrace << std::endl; }
        Tseries series;
        is >> series;
        sff::WID2 wid2;
        is >> wid2;
        TFXX_debug(opt.debug, "main", 
                   "  series and WID2 are read");

        // calculate trace specific resampling parameters
        TAbsoluteTime firstsample=wid2.date+opt.edge;
        if (opt.timefirstset) 
        { firstsample=opt.timefirst+opt.edge; }
        else if (opt.sampleoffsetbeginset)
        {
          TAbsoluteTime timeofs=firstsample;
          if (opt.sampleoffsetbegin<0)
          {
            timeofs = sff::wid2lastsample(wid2)-
              (-opt.sampleoffsetbegin)*libtime::double2time(wid2.dt);
          }
          else
          {
            timeofs += opt.sampleoffsetbegin*libtime::double2time(wid2.dt);
          }
          if (timeofs>sff::wid2lastsample(wid2))
          { timeofs=sff::wid2lastsample(wid2); }
          if (timeofs<wid2.date)
          { timeofs=wid2.date; }
          firstsample=timeofs;
        }

        // set time of last sample
        TAbsoluteTime lastsample=sff::wid2lastsample(wid2)-opt.edge;
        if (opt.sampleoffsetendset)
        {
          TAbsoluteTime timeols=lastsample;
          if (opt.sampleoffsetend<0)
          {
            timeols = firstsample+
              (-opt.sampleoffsetend)*libtime::double2time(wid2.dt);
          }
          else
          {
            timeols -= opt.sampleoffsetend*libtime::double2time(wid2.dt);
          }
          if (timeols>lastsample)
          { timeols=lastsample; }
          if (timeols<firstsample)
          { timeols=firstsample; }
          lastsample=timeols;
        }

        // set dt
        TRelativeTime newdt=libtime::double2time(wid2.dt);
        if (opt.dtset)
        { newdt=libtime::double2time(opt.dt); }
        int nsamples=(sff::wid2lastsample(wid2)-opt.edge-firstsample)/newdt; 

        // set number of samples
        if (opt.nset)
        { nsamples=opt.n; }
        else if (opt.timelastset)
        { nsamples=1+(opt.timelast-opt.edge-firstsample)/newdt; }
        else if (opt.timespanset)
        { nsamples=1+opt.timespan/newdt; }
        else if (opt.sampleoffsetendset)
        { nsamples=1+(lastsample-opt.edge-firstsample)/newdt; }

        TFXX_assert(nsamples>0,
                    "ERROR: unreasonable time window");

        if (opt.verbose) 
        {
          cout << "    resampling parameters are:" << endl
               << "      time of first sample: " 
                 << firstsample.timestring() << endl
               << "      sampling interval:    " << newdt.timestring() << endl
               << "      number of samples:    " << nsamples << endl;
          if (newdt > libtime::double2time(wid2.dt))
          {
            cout << "    NOTICE: "
                 << "New sampling interval is larger than old one."
                 << endl
                 << "      You are responsible to apply an appropriate"
                    << " anti-alias filter" 
                 << endl
                 << "      prior to resampling!"
                 << endl;
          }
          if (opt.shrink)
          {
            cout << "    shrink output time window to available input" <<
              endl
                 << "      data while maintaining the defined sampling" <<
              endl
                 << "      raster" << endl;
          }
        }
        
        // go for interpolation
        typedef ts::ipo::Interpolator Tinterpolator;
        ::sff::WID2 inwid2=wid2;
        if (opt.delay)
        {
          if (opt.delaytime<0)
          {
            inwid2.date-=libtime::double2time(-opt.delaytime);
            if (opt.verbose)
            {
              cout << "      advance series by "
                << libtime::double2time(-opt.delaytime).timestring() << endl;
            }
          }
          else
          {
            inwid2.date+=libtime::double2time(opt.delaytime);
              cout << "      delay series by "
                << libtime::double2time(opt.delaytime).timestring() << endl;
          }
        }
        Tinterpolator::Tconst_timeseries inseries(series, inwid2);
        typedef tfxx::Handle<Tinterpolator> Thipo;
        Thipo hipo(new ts::ipo::LinearInterpolator(inseries, opt.debug));
        Ttimeseries outseries;
        bool hot=true;
        try
        {
          outseries=ts::ipo::resample(*hipo, firstsample, newdt, nsamples,
                                      opt.shrink);
        }
        catch (ts::ipo::ExceptionTimeWindowOutside)
        {
          cout << "NOTICE: skipping this trace silently..." << endl;
          hot=false;
        }
        catch (ts::ipo::ExceptionTimeWindowEmpty)
        {
          cout << "NOTICE: skipping this trace silently..." << endl;
          hot=false;
        }
        if (hot)
        {
          if (opt.shrink && opt.verbose)
          {
            cout << "  time window returned from interpolator:" << endl
              <<    "                begin: "
              << outseries.header.date.timestring() << endl
              <<    "                  end: " 
              << sff::wid2lastsample(outseries.header).timestring() << endl
              <<    "    number of samples: " 
              << outseries.header.nsamples << endl;
          }
          TFXX_debug(opt.debug, "main", 
                     "  series is resampled");
          os << outseries.header;
          TFXX_debug(opt.debug, "main", 
                     "  series and WID are written");
          if (is.hasinfo()) { sff::INFO info; is >> info; os << info; }
          if (is.hasfree() || true) 
          {
            sff::FREE tracefree;
            is >> tracefree;
            tracefree.append(RESASEDA_VERSION);
            tracefree.append("read from file " + infile->name);
            os << tracefree;
          }
          os << Tseries(outseries);
          TFXX_debug(opt.debug, "main", 
                     "trace #" << itrace << " successfully processed");
        }
      }
      else
      {
        TFXX_debug(opt.debug, "main", "skip trace #" << itrace );
        if (opt.verbose)
        { std::cout << "  skip trace #" << itrace << std::endl; }
        is.skipseries();
      }
    }
    
    // go to next file
    firstfile=false;
    ++infile;
  }

}

/* ----- END OF resaseda.cc ----- */
