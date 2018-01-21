/*! \file tijerasxx.cc
 * \brief Simply cut off timeseries.
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Daniel Armbruster
 * \date 25/08/2016
 * 
 * Purpose: Simply cut off timeseries. 
 *
 * ----
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * ----
 * 
 * Copyright (c) 2012 by Daniel Armbruster
 * 
 * REVISIONS and CHANGES 
 * 02/03/2012   V0.1    Daniel Armbruster
 * 24/05/2012   V0.2    cut time window by passing timestrings
 * 15/09/2012   V0.3    fixed bug in microsecond computation setting date of
 *                      first sample in WID2 header line
 * 13/12/2012   V1.0    thof:
 *                      reworked evaluation of new time window
 *                      empty time ranges are allowed
 * 18/12/2012   V1.1    empty intersection is error condition by default
 *                      evaluate smallest time window considering all
 *                      parameters
 * 25/08/2016   V1.2    handle input time series index range correctly, where
 *                      first index differs from zero
 * 
 * ============================================================================
 */
 
#define TIJERASXX_VERSION \
  "TIJERASXX   V1.2  simply cut off timeseries data"

#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>
#include <tfxx/rangestring.h>
#include <tfxx/rangelist.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <datrwxx/datatypes.h>
#include <libtime++.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, overwrite, debug, integer, single, skipifempty;
  int firstsamples, lastsamples, durationsamples;
  double firstseconds, lastseconds, durationseconds;
  libtime::TAbsoluteTime firstdate, lastdate;
  libtime::TRelativeTime durationdate;
  bool firstdateSet, lastdateSet, durationdateSet;
  std::string inputformat, outputformat;
}; // struct Options

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    TIJERASXX_VERSION "\n"
    "usage: tijerasxx [--verbose] [--overwrite] [--integer] [--single]" "\n"
    "                 [--iformat T] [--oformat T]" "\n"
    "                 [--samplesf N] [--samplesl N] [--samplesd N]" "\n"
    "                 [--secondsf N] [--secondsl N] [--secondsd N]" "\n"
    "                 [--datesf DAT] [--datesl DAT] [--datesd DUR]" "\n"
    "                 [--skipifempty]\n"
    "                 OUTFILE INFILE [t:T] [f:F] [INFILE [t:T] [f:F] ... ]\n"
    "   or: tijerasxx --help|-h" "\n"
    "   or: tijerasxx --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "Options may be abbreviated to a short string as long as they" "\n"
    "remain unique. \"-v\" is identical to \"--verbose\"." "\n"
    "\n"
    "--verbose    be verbose" "\n"
    "--overwrite  overwrite OUTFILE if file already exists" "\n"
    "--integer    use integer values for copying" "\n"
    "--single     use single precision floats for copying" "\n"
    "             default data type for copying is double presicion floats\n"
    "--iformat T  standard format of input file(s) (see below)" "\n"
    "--oformat T  data format of OUTFILE (see below)" "\n"
    "--skipifempty skip trace if resulting intersection is empty\n"
    "             otherwise an empty intersection will be regarded\n"
    "             as an error\n"
    "\n"
    "Data selection:" "\n"
    "--samplesf N cut off N samples from the beginning of the timeseries\n"
    "--samplesl N cut off N samples from the end of the timeseries\n"
    "--samplesd N cut off after N samples beginning from '--samplesf'," "\n"
    "             '--secondsf' or '--datesf'" "\n"
    "--secondsf N cut off N seconds from the beginning of the timeseries" "\n"
    "--secondsl N cut off N seconds from the end of the timeseries" "\n"
    "--secondsd N cut off after N seconds beginning from '--samplesf'," "\n"
    "             '--secondsf' or '--datesf'" "\n"
    "--datesf DAT start timeseries at date DAT" "\n"
    "--datesl DAT finish timeseries at date DAT" "\n"
    "--datesd DUR cut off timeseries after duration DUR beginning from" "\n"
    "             '--samplesf', '--secondsf' or '--datesf'" "\n"
    "----" "\n"
    "Note that tijerasxx always will compute the smallest subset passed by\n"
    "the arguments above." "\n"
    "\n"
    "OUTFILE      output data file name" "\n"
    "INFILE(s)    input data file name(s)" "\n"
    "\n"
    "File specific options:" "\n"
    "t:T          select specfic traces from INFILE" "\n"
    "             T can be a list of traces like \"1,4,5\" or" "\n"
    "             a range like \"6-19\" or mixed like \"5,8,12-17,20\"" "\n"
    "f:F          specify file format (overrides --iformat setting)" "\n"
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
    // 1: print extended help
    {"xhelp",arg_no,"-"},
    // 2: verbose mode
    {"verbose",arg_no,"-"},
    // 3: overwrite output
    {"overwrite",arg_no,"-"},
    // 4: overwrite output
    {"iformat",arg_yes,"sff"},
    // 5: overwrite output
    {"oformat",arg_yes,"sff"},
    // 6: overwrite output
    {"DEBUG",arg_no,"-"},
    // 7: read integer data
    {"integer",arg_no,"-"},
    // 8: read single precision data
    {"single",arg_no,"-"},
    // 9: cut off N samples from the beginning
    {"samplesf",arg_yes,"0"},
    // 10: cut off N samples from the end
    {"samplesl",arg_yes,"0"},
    // 11: cut off after N samples
    {"samplesd",arg_yes,"-1"},
    // 12: cut off N seconds from the beginning
    {"secondsf",arg_yes,"0."},
    // 13: cut off N seconds from the end
    {"secondsl",arg_yes,"0."},
    // 14: cut off after N seconds
    {"secondsd",arg_yes,"-1."},
    // 15: start time of window 
    {"datesf", arg_yes, "-"},
    // 16: end time of window 
    {"datesl", arg_yes, "-"},
    // 17: time window duration set by time
    {"datesd", arg_yes, "-"},
    // 18: skip empty traces
    {"skipifempty", arg_no, "-"},
    {NULL}
  };

  //! key to select traces
  const char* const tracekey="t";
  //! key to select file format
  const char* const formatkey="f";
  //! list of keys for filename specific parameters
  static const char* cmdlinekeys[]={
    //! select traces
    tracekey,
    //! set file format
    formatkey,
    0
  }; // const char* keys[]

  // no arguments? print usage...
  if (iargc < 2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text ...
  if (cmdline.optset(0) || cmdline.optset(1))
  {
    cout << usage_text << endl;
    cout << help_text << endl;
    datrw::supported_data_types(cout);
    if (cmdline.optset(1))
    {
      cout << endl;
      datrw::online_help(cout);
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
  opt.firstsamples=cmdline.int_arg(9);
  opt.lastsamples=cmdline.int_arg(10);
  opt.durationsamples=cmdline.int_arg(11);
  opt.firstseconds=cmdline.double_arg(12);
  opt.lastseconds=cmdline.double_arg(13);
  opt.durationseconds=cmdline.double_arg(14);
  opt.firstdateSet=cmdline.optset(15);
  opt.lastdateSet=cmdline.optset(16);
  opt.durationdateSet=cmdline.optset(17);
  opt.skipifempty=cmdline.optset(18);
  if (opt.firstdateSet)
  {
    opt.firstdate=libtime::TAbsoluteTime(cmdline.string_arg(15));
  }
  if (opt.lastdateSet)
  {
    opt.lastdate=libtime::TAbsoluteTime(cmdline.string_arg(16));
  }
  if (opt.durationdateSet)
  {
    opt.durationdate=libtime::TRelativeTime(cmdline.string_arg(17));
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
        { std::cout << "  edit trace #" << itrace << std::endl; }

        datrw::Tfseries fseries;
        datrw::Tdseries dseries;
        datrw::Tiseries iseries;

        // read time series
        if (opt.integer) 
        {
          TFXX_assert(is.providesi(),
                      "ERROR: input data is not provided as integer values");
          is >> iseries;
          TFXX_debug(opt.debug, "tijerasxx: index range at input",
                     iseries.first() << " - " << iseries.last());
        }
        else if (opt.single)
        {
          TFXX_assert(is.providesf(),
                      "ERROR: input data is not provided as "
                      "single precision floats");
          is >> fseries;
          TFXX_debug(opt.debug, "tijerasxx: index range at input",
                     fseries.first() << " - " << fseries.last());
        }
        else
        {
          TFXX_assert(is.providesd(),
                      "ERROR: input data is not provided as "
                      "double precision floats");
          is >> dseries;
          TFXX_debug(opt.debug, "tijerasxx: index range at input",
                     dseries.first() << " - " << dseries.last());
        }

        // pass WID2
        sff::WID2 wid2;
        is >> wid2;

        /*------------------------------------------------------------*/
        // calculate smallest subset to cut off the series
        // ---------------------
        // a flag indicating an empty time range - just in case
        bool ResultIsEmpty=false;
        long int newFirstSample=0;
        long int newLastSample=0;
          
        // extract coordinates of input data
        libtime::TRelativeTime dt=libtime::double2time(wid2.dt);
        libtime::TAbsoluteTime TimeOfFirst=wid2.date;
        libtime::TAbsoluteTime TimeOfLast=sff::wid2lastsample(wid2);
        TFXX_debug(opt.debug, "tijerasxx (main)",
                   TFXX_value(TimeOfFirst.timestring()) 
                   << "\n      " << 
                   TFXX_value(TimeOfLast.timestring()));

        // initialize beginning of output series with time of first sample
        // of input series
        libtime::TAbsoluteTime NewTimeOfFirst=TimeOfFirst;

        // advance time of first sample if selected by command line parameters
        if (opt.firstdateSet) { NewTimeOfFirst=opt.firstdate; }
        NewTimeOfFirst += opt.firstsamples*dt;
        NewTimeOfFirst += libtime::double2time(opt.firstseconds);

        TFXX_debug(opt.debug, "tijerasxx (main)",
                   TFXX_value(NewTimeOfFirst.timestring()));

        // reduce time of last sample if selected by command line parameters
        libtime::TAbsoluteTime NewTimeOfLast=TimeOfLast;
        if (opt.lastdateSet) { NewTimeOfLast=opt.lastdate; }
        NewTimeOfLast -= opt.lastsamples*dt;
        NewTimeOfLast -= libtime::double2time(opt.lastseconds);

        // check if any of the duration setting results in an earlier time of
        // last sample; if yes, set to new value
        if (opt.durationdateSet) 
        {
          libtime::TAbsoluteTime NewLastDueToDuration
            =NewTimeOfFirst+opt.durationdate;
          if (NewLastDueToDuration<NewTimeOfLast)
          { NewTimeOfLast=NewLastDueToDuration; }
        }
        if (opt.durationsamples>0)
        {
          libtime::TAbsoluteTime NewLastDueToDuration
            =NewTimeOfFirst+opt.durationsamples*dt;
          if (NewLastDueToDuration<NewTimeOfLast)
          { NewTimeOfLast=NewLastDueToDuration; }
        }
        if (opt.durationseconds>0)
        {
          libtime::TAbsoluteTime NewLastDueToDuration
            =NewTimeOfFirst+libtime::double2time(opt.durationseconds);
          if (NewLastDueToDuration<NewTimeOfLast)
          { NewTimeOfLast=NewLastDueToDuration; }
        }
        TFXX_debug(opt.debug, "tijerasxx (main)",
                   TFXX_value(NewTimeOfLast.timestring()));

        // check if result is empty
        // ------------------------
        if ((NewTimeOfLast<NewTimeOfFirst)
            || (NewTimeOfLast<TimeOfFirst)
            || (NewTimeOfFirst>TimeOfLast))
        {
          ResultIsEmpty=true;
          if (opt.verbose)
          {
            std::cout 
              << "time range of input series:\n  "
              << TimeOfFirst.timestring()
              << " - "
              << TimeOfLast.timestring()
              << std::endl;
            std::cout 
              << "new time range as specified:\n  "
              << NewTimeOfFirst.timestring()
              << " - "
              << NewTimeOfLast.timestring()
              << std::endl;
            std::cout 
              << "notice: the intersection is empty!\n"
              << std::endl;
          }
        }
        else // not empty
        {
          newFirstSample=sff::wid2isample(wid2, NewTimeOfFirst);
          newLastSample=sff::wid2isample(wid2, NewTimeOfLast);
          // values should be fine; just for safety:
          if (newFirstSample<0) { newFirstSample=0; }
          if (newFirstSample>=wid2.nsamples) 
          { newFirstSample=wid2.nsamples-1; }
          if (newLastSample<newFirstSample) 
          { newLastSample=newFirstSample; }
          if (newLastSample>=wid2.nsamples) 
          { newLastSample=wid2.nsamples-1; }
        }

        /*------------------------------------------------------------*/

        // output data only if not empty
        if (ResultIsEmpty)
        {
          TFXX_assert((!ResultIsEmpty) || (opt.skipifempty),
            "selection results in empty intersection with current trace");
          if (opt.verbose)
          {
            cout << "skipping trace..." << endl;
          }
        }
        else
        {
          // adjust wid2
          sff::WID2 newwid2(wid2);
          newwid2.date=sff::wid2isample(wid2, newFirstSample);
          newwid2.nsamples=newLastSample-newFirstSample+1;

          if (opt.verbose) { cout << newwid2; }

          os << newwid2;

          TFXX_debug(opt.debug, "tijerasxx: trace header data availability",
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
              TFXX_debug(opt.debug, "tijerasxx: INFO data",
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
              freeblock.append("Edited with: ");
              freeblock.append(TIJERASXX_VERSION);
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

          TFXX_debug(opt.debug, "tijerasxx: index range for output",
                     newFirstSample << " - " << newLastSample);

          // write output
          if (opt.integer)
          {
            iseries.setindexrange(newFirstSample+iseries.first(), 
                                  newLastSample +iseries.first()); 
            os << iseries;
          }
          else if (opt.single)
          {
            fseries.setindexrange(newFirstSample+fseries.first(), 
                                  newLastSample +fseries.first()); 
            os << fseries;
          }
          else
          {
            dseries.setindexrange(newFirstSample+dseries.first(), 
                                  newLastSample +dseries.first()); 
            os << dseries;
          }
        } // if (!ResultIsEmpty)
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


/* ----- END OF tijerasxx.cc  ----- */
