/*! \file tidofi.cc
 * \brief time domain filter
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 17/02/2004
 * 
 * time domain filter
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * TIDOFI is free software; you can redistribute it and/or modify
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
 *  - 17/02/2004   V1.0   Thomas Forbriger
 *  - 18/12/2007   V1.1   hunting bugs...
 *  - 21.12.2010   V1.2   implement output file type option
 *  - 27.07.2011   V1.3   implement apply trace modifier
 *  - 08.09.2011   V1.4   support format modifiers
 *  - 26.01.2012   V1.5   
 *                        - write header data only if supported by output 
 *                          format
 *                        - implemented offset variable tapers
 * 
 * ============================================================================
 */
#define TIDOFI_VERSION \
  "TIDOFI   V1.5   time domain filter"

#include <iostream>
#include <fstream>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/error.h>
#include <tfxx/stringfunc.h>
#include <tfxx/rangestring.h>
#include <tfxx/rangelist.h>
#include <tfxx/misc.h>
#include <tsxx/anyfilter.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <sffxx.h>
#include <sffostream.h>
#include <aff/dump.h>
#include <tsxx/ovtaper.h>

using std::cout;
using std::cerr;
using std::endl;

typedef ts::filter::Ttimeseries Ttimeseries;
typedef Ttimeseries::Tseries Tseries;

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, debug, debugdump;
  bool overwrite;
  bool readcommandfile;
  std::string commandfile;
  bool readstdin;
  std::string inputformat, outputformat;
}; // struct Options

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    TIDOFI_VERSION "\n"
    "usage: tidofi [-v] [-o] [-cf file] [-cs] [-type type] [-Type type]" "\n"
    "              outfile infile [t:T|a:T] [f:F] [prft:file] [poft:file]\n"
    "                [infile [t:T|a:T] [f:F] [prft:file] [poft:file] ... ]" "\n"
    "   or: tidofi --help|-h" "\n"
    "   or: tidofi --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "outfile      name of file to contain results" "\n"
    "infile       input file" "\n"
    "             t:T select traces T, where T may be any range" "\n"
    "                 specification like \'3-4\' or \'5,6,7-12,20\'" "\n"
    "             f:F specifies an input file format differing from" "\n"
    "                 the format selected by \"-type\"" "\n"
    "             a:T apply filter to traces T, where T may be any range" "\n"
    "                 specification like \'3-4\' or \'5,6,7-12,20\'" "\n"
    "                 all other traces are passed to the output unchanged\n"
    "             prft:file\n"
    "                 pre-filter taper from file to be applied before\n"
    "                 filtering traces\n"
    "             poft:file\n"
    "                 post-filter taper from file to be applied before\n"
    "                 filtering traces\n"
    "             file mofifiers t and a are mutually exclusive, where a\n"
    "             has higher precendence\n"
    "             prft and poft taper parameters are read from \"file\"\n"
    "             taper parameter file is expected in format produced\n"
    "             by refract\n"
    "\n"
    "-xhelp       print detailed information regarding file formats" "\n"
    "-v           be verbose" "\n"
    "-DEBUG       produce debug output" "\n"
    "-DUMP        produce debug dumps" "\n"
    "-o           overwrite output" "\n"
    "-cf file     read filter commands from \"file\"" "\n"
    "-cs          read filter commands from stdin" "\n"
    "-type type   choose input file format (default: sff)" "\n"
    "-Type type   choose output file format (default: sff)" "\n"
    "\n"
    "In command files comment lines may either commence with \"rem\" like" "\n"
    "in seife command files or with \'#\'. The seife command \"end\" is" "\n"
    "simply ignored (like a comment line)."
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
    // 3: command file
    {"cf",arg_yes,"-"},
    // 4: stdin mode
    {"cs",arg_no,"-"},
    // 5: input file format
    {"type",arg_yes,"sff"},
    // 6: generate debug output
    {"DEBUG",arg_no,"-"},
    // 7: generate debug dumps
    {"DUMP",arg_no,"-"},
    // 8: output file format
    {"Type",arg_yes,"sff"},
    // 9: output file format
    {"xhelp",arg_no,"-"},
    {NULL}
  };

  static const char tracekey[]="t";
  static const char applykey[]="a";
  static const char formatkey[]="f";
  static const char prefiltertaperkey[]="prft";
  static const char postfiltertaperkey[]="poft";

  // define commandline argument modifier keys
  static const char* cmdlinekeys[]
    ={tracekey, formatkey, applykey, prefiltertaperkey, postfiltertaperkey, 0};

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
    cerr << endl;
    ts::filter::print_any_help(cerr);
    exit(0);
  }

  // help on file format details requested? 
  if (cmdline.optset(9))
  {
    cerr << usage_text << endl;
    cerr << endl;
    datrw::online_help(cerr); 
    exit(0);
  }

  // extract commandline options
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.readcommandfile=cmdline.optset(3);
  opt.commandfile=cmdline.string_arg(3);
  opt.readstdin=cmdline.optset(4);
  opt.inputformat=cmdline.string_arg(5);
  opt.debug=cmdline.optset(6);
  opt.debugdump=cmdline.optset(7);
  opt.outputformat=cmdline.string_arg(8);
  // 9 is --xhelp

  if (opt.verbose)
  { cout << TIDOFI_VERSION << endl; }

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

  /*----------------------------------------------------------------------*/
  // first of all read the filter commands
  // we use an sff FREE block to store the lines - its so easy
  sff::FREE filtercommands;
  if (opt.readcommandfile)
  {
    if (opt.verbose) 
    {
      cout << "read filter commands from file \"" 
        << opt.commandfile << "\"" << endl;
    }
    std::ifstream ifs(opt.commandfile.c_str());
    while (ifs.good())
    {
      std::string line;
      getline(ifs, line);
      line=tfxx::string::trimws(line);
      if (line.size() > 0)
      {
        filtercommands.append(line);
        if (opt.verbose) 
        { cout << ">> " << line << endl; }
      }
    }
  }
  // read commands from stdin
  if (opt.readstdin)
  {
    if (opt.verbose) 
    {
      cout << "read filter commands from stdin (terminate with ctrl-D)"  
        << endl;
    }
    while (std::cin.good())
    {
      std::string line;
      getline(std::cin, line);
      line=tfxx::string::trimws(line);
      if (line.size() > 0)
      {
        filtercommands.append(line);
        if (opt.verbose) 
        { cout << ">> " << line << endl; }
      }
    }
  }

  /*----------------------------------------------------------------------*/
  // strip comments etc from filter commands
  // and create filter object
  sff::FREE commands;
  ts::filter::FilterCollection filter;
  {
    sff::FREE::Tlines::const_iterator line=filtercommands.lines.begin();
    while (line != filtercommands.lines.end())
    {
      // skip comments
      bool skip=false;
      TFXX_debug(opt.debug, "main", 
                 "skip " + *line + " if irrelevant" );
      skip |= (line->substr(0,1)=="#");
      skip |= (line->substr(0,3)=="rem");
      skip |= (line->substr(0,3)=="end");
      if (!skip) { 
        std::string command=(*line);
        // replace comma by space
        std::string::size_type i=command.find(",",i);
        while (i!=std::string::npos)
        {
          command.replace(i,1," ");
          i=command.find(",",i+1);
        }
        commands.append(command); 
        filter.push_back(ts::filter::make_any_filter(command, opt.debug));
      }
      else
      {
        TFXX_debug(opt.debug, "main", 
                   "skip " + *line + "!" );
      }
      ++line;
    }
  }
  // TFXX_debug(opt.debug, "main", "STOP" ); exit(0);

  /*----------------------------------------------------------------------*/
  // prepare file FREE block
  sff::FREE filefree;
  {
    filefree.append(TIDOFI_VERSION);
    std::string line;
    if (opt.readcommandfile || opt.readstdin)
    {
      line="commands are read from";
      if (opt.readstdin)
      {
        line += " stdin";
        if (opt.readcommandfile)
        {
          line += " (first) and";
        }
        else
        {
          line += ":";
        }
      }
      filefree.append(line);
      line.clear();
      if (opt.readcommandfile) 
      {
        line += "file \"" + opt.commandfile + "\":";
        filefree.append(line);
      }
      if (filtercommands.lines.size() > 0)
      {
        filefree.append(filtercommands);
      }
      else
      {
        filefree.append("  command list is empty!");
      }
    }
    else
    {
      filefree.append("no filtercommands were read");
    }
    filefree.append("output file name:");
    filefree.append("  " + outfile);
    filefree.append("input file selection:");
    tfxx::cmdline::Tparsed::const_iterator file=arguments.begin();
    while (file != arguments.end())
    {
      filefree.append("  " + file->name);
      line="  ";
      tfxx::cmdline::Toptionmap::const_iterator option=file->options.begin();
      while (option != file->options.end())
      {
        line += "  " + option->first + ":" + option->second;
        ++option;
      }
      if (line.size()>2) { filefree.append(line); }
      ++file;
    }
    if (arguments.size()>1)
    {
      filefree.append("In cases where more than one input file is read,");
      filefree.append("the SRCE line is taken from the first file only (if");
      filefree.append("present there).");
    }
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
  std::ios_base::openmode oopenmode
    =datrw::oanystream::openmode(opt.outputformat);
  std::ofstream ofs(outfile.c_str(), oopenmode);
  datrw::oanystream os(ofs, opt.outputformat, opt.debug);

  // set flag to process header of first input file
  bool firstfile=true;
  // cycle through all input files
  // -----------------------------
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile != arguments.end())
  {
    // open input file
    if (opt.verbose) { cout << "open input file " << infile->name << endl; }
    std::string inputformat=opt.inputformat;
    if (infile->haskey(formatkey)) 
    {
      inputformat=infile->value(formatkey);
    }
    std::ios_base::openmode iopenmode
      =datrw::ianystream::openmode(inputformat);
    std::ifstream ifs(infile->name.c_str(), iopenmode);
    datrw::ianystream is(ifs, inputformat);

    // read tapers
    // -----------
    bool applyprefiltertaper=infile->haskey(prefiltertaperkey);
    bool applypostfiltertaper=infile->haskey(postfiltertaperkey);
    
    // create taper objects here
    ts::tapers::OffsetVariableTaper prefiltertaper;
    ts::tapers::OffsetVariableTaper postfiltertaper;

    // read taper parameters here
    if (applyprefiltertaper)
    {
      std::string taperfilename
        =infile->value(prefiltertaperkey);
      prefiltertaper.read(taperfilename);
    }
    if (applypostfiltertaper)
    {
      std::string taperfilename
        =infile->value(postfiltertaperkey);
      postfiltertaper.read(taperfilename);
    }
      
    // handle file header
    // ------------------
    sff::SRCE insrceline;
    bool srceavailable=false;
    if (firstfile)
    {
      if (is.hasfree()) 
      { 
        sff::FREE infilefree;
        is >> infilefree;
        filefree.append("block read from first input file:");
        filefree.append(infilefree);
      }
      if (os.handlesfilefree()) { os << filefree; }
      if (is.hassrce())
      {
        is >> insrceline;
        srceavailable=true;
        if (os.handlessrce()) { os << insrceline; }
      }
    }

    // cycle through traces of input file
    // ----------------------------------
    // setup trace selection
    typedef tfxx::RangeList<int> Trangelist;
    bool doselect=infile->haskey(tracekey);
    bool doapply=infile->haskey(applykey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(tracekey));
    // apply has precedence
    if (doapply)
    {
      if (opt.verbose && doselect)
      {
        std::cout << "  trace \"apply\" modifier has precedence "
          << "over trace \"selection\" modifier."
          << std::endl;
      }
      doselect=false;
      traceranges
        =tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(applykey));
    } 
    int itrace=0;
    while (is.good())
    {
      ++itrace;
      if ((!doselect) || traceranges.contains(itrace))
      {
        TFXX_debug(opt.debug, "main", "process trace #" << itrace );
        if (opt.verbose)
        { std::cout << "  process trace #" << itrace << ":"; }
        Tseries series;
        is >> series;
        if (opt.debugdump)
        {
          TFXX_debug(opt.debugdump, "main", 
                     "  input series:");
          DUMP(series);
        }
        sff::WID2 wid2;
        is >> wid2;
        TFXX_debug(opt.debug, "main", 
                   "  series and WID2 are read");
        sff::INFO info; 
        if (is.hasinfo()) 
        { 
          is >> info; 
        }
        // apply filter only if requested
        // ------------------------------
        if ((!doapply) || traceranges.contains(itrace))
        {
          double offset, t0, T;
          if (applyprefiltertaper || applypostfiltertaper)
          {
            TFXX_assert(is.hasinfo() && srceavailable,
                        "Offset variable taper requires source and receiver "
                        "coordinates");
            offset=::sff::offset(insrceline, info);
            t0=libtime::time2double(insrceline.date-wid2.date);
            T=wid2.dt*wid2.nsamples;
          }
          if (applyprefiltertaper)
          {
            // apply pre-filter taper
            ts::tapers::FourPoint taper
              =prefiltertaper.taper(offset, t0, T);
            taper.apply(series);
          }
          filter(Ttimeseries(series, wid2.dt), opt.debug);
          if (applypostfiltertaper)
          {
            // apply post-filter taper
            ts::tapers::FourPoint taper
              =postfiltertaper.taper(offset, t0, T);
            taper.apply(series);
          }
          TFXX_debug(opt.debug, "main", 
                     "  series is filtered");
          if (opt.debugdump)
          {
            TFXX_debug(opt.debugdump, "main", 
                       "  output series:");
            DUMP(series);
          }
          if (opt.verbose) { std::cout << " filtered" << std::endl; }
        // ------------------------------
        }
        else
        {
          if (opt.verbose) { std::cout << " passed unchanged" << std::endl; }
        }
        os << wid2;
        TFXX_debug(opt.debug, "main", 
                   "  series and WID are written");
        if (is.hasinfo()) 
        { 
          if (os.handlesinfo()) { os << info; }
        }
        if (is.hasfree() || true) 
        {
          sff::FREE tracefree;
          is >> tracefree;
          tracefree.append(TIDOFI_VERSION);
          tracefree.append("read from file " + infile->name);
          if ((!doapply) || traceranges.contains(itrace))
          {
            tracefree.append(commands);
          }
          else
          {
            tracefree.append("passed unchanged");
          }
          if (os.handlestracefree()) { os << tracefree; } 
        }
        TFXX_debug(opt.debug, "main", 
                   "trace #" << itrace << " successfully processed");
        os << series;
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
    firstfile=false;
    ++infile;
  }

}

/* ----- END OF tidofi.cc ----- */
