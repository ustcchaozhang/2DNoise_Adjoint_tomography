/*! \file anyextract.cc
 * \brief extract data files, using index file
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/09/2004
 * 
 * extract data files, using index file
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
 *  - 06/09/2004   V1.0   Thomas Forbriger
 *  - 17/09/2004   V1.1   should work now, but still is terribly verbose
 *  - 24/09/2004   V1.2   first accepted version
 *  - 23/12/2004   V1.3   gap finder
 *  - 27/07/2005   V1.4   
 *                        - going to introduce some tolerance 
 *                        - and redundant sample checks
 *                        - range check prior to copying sample is now 
 *                          offset-correct
 *                        - distinguish brief gap reports from extended gap
 *                          reports
 *                        - introduced option -dump
 *  - 28/07/2005   V1.5   introduced option -sd
 *  - 27/04/2006   V1.6   introduce options -GSE and -int
 *  - 04/05/2006   V1.7   report channel on GAP condition
 *  - 23/11/2006   V1.8   starting to introduce the DropContainer concept
 *  - 08/03/2007   V1.9   provide specification of calib and calper
 *  - 09/03/2007   V1.10  explain calib and calper
 *  - 25/09/2007   V1.11  make help less verbose
 *  - 05/12/2007   V1.12  use new interface to regular expressions
 *  - 08/07/2008   V1.13  make use of sequentialtracereader
 *  - 13/04/2010   V1.14  report GAP times to microsecond level
 *  - 28/11/2011   V1.15  implemented new features:
 *                        - skip writing for FREE blocks on request
 *                        - provide any output format
 *                        - provide internal float type copying
 *  - 29/11/2011   V1.16p started to implement gaps statistics
 *  - 20/12/2011   V1.16l gap report to stdout is implemented
 *  - 30/01/2012   V1.16m implemented completeness time series
 *  - 13/02/2012   V1.17  gap and completeness series is implemented and
 *                        tested
 *  - 14/02/2012   V1.18  provide gap summary
 *  - 14/02/2012   V1.19  provide gnuplot gap plot
 *  - 02/03/2012   V1.20  include auxid when distinguishing streams
 *  - 26/03/2012   V1.21  ilist -> iinlist (used the same name twice)
 *  - 24/04/2012   V1.22  removed misleading help line
 *  - 25/04/2012   V1.23  correctly set latesttime
 *  - 08/01/2013   V1.24  bug fix: handle earliest and latest times properly
 *                        in situations where a completeness analysis is done
 *                        on data on staggered sampling rasters
 *  - 07/11/2014   V1.25  properly report meaning of gap messages
 *
 *  TODO:
 *    * properly handle cases in which the output format can take only single
 *      traces (consider implementing sequentialtracewriter in libdatrwxx).
 *    * properly check the variable type for samples which can be handled
 *      properly by the output stream; currently the program relies on
 *      on-the-fly conversion (round-off is not checked)
 * 
 * ============================================================================
 */

#define ANYEXTRACT_VERSION \
  "ANYEXTRACT   V1.25   extract data files, using index file"

#include <fstream>
#include <iostream>
#include <list>
#include <vector>
#include <string>
#include <sstream>
#include <aff/seriesoperators.h>
#include <tfxx/misc.h>
#include <tfxx/commandline.h>
#include <tfxx/stringfunc.h>
#include <tfxx/error.h>
#include <datrwxx/tracereader.h>
#include <libtime++.h>
#include <tfxx/regexx.h>
#include <datrwxx/formats.h>
#include <datrwxx/error.h>
#include <datrwxx/writeany.h>
#include "sub/structgapanalysis.h"
#include "sub/gapfunctions.h"
#include "sub/completenessbins.h"
#include "sub/structgapseries.h"

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, debug, overwrite, rangecheck;
  bool GSEoutput, integerdata, nofreeblock, floatdata;
  libtime::TAbsoluteTime first;
  libtime::TAbsoluteTime last;
  bool firstset, lastset;
  std::string selstation;
  std::string selchannel;
  std::string selinstrument;
  std::string selauxid;
  std::string outputformat;
  bool findgapsonly, dumpintermediateresults;
  bool allowtolerance, allowduplicatesamples;
  bool breakonduplicatesamples;
  double relativetolerance;
  bool setcalib;
  bool setcalper;
  double newcalib, newcalper;
  bool writegapseries;
  std::string gapseriesfile;
  bool writecompleteness;
  std::string completenessseriesfile;
  bool printgaps;
  std::string gapoutputfile;
  std::string binsize;
  unsigned int summarizelevel;
  bool providegnuplotplot;
  std::string gnuplotfile;
}; // struct Options

/*----------------------------------------------------------------------*/

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

typedef std::vector<std::string> Tvecofstrings;

/*----------------------------------------------------------------------*/

typedef datrw::Tdseries Tseries;
typedef datrw::Tfseries Tfseries;
typedef datrw::Tiseries Tiseries;

/*----------------------------------------------------------------------*/

//! class to handle two different flavours of data
class Series {
  public:
    typedef Tseries Tds;
    typedef Tiseries Tis;
    typedef Tfseries Tfs;
    typedef aff::Tsubscript Tindex;
    typedef aff::Tsize Tsize;
    typedef Tis::Tshape Tshape;
    Series(): 
      Mselecti(false), Mselectd(false), Mselectf(false) { }
    bool selected() const { return(Mselecti || Mselectd || Mselectf); }
    Tds& ds() 
    { 
      TFXX_assert(!(Mselecti || Mselectf),
                  "ERROR (Series): other type is already slected!\n"
                  "internal inconsistency to be considered a bug");
      Mselectd=true;
      return(Mds);
    }
    Tfs& fs() 
    { 
      TFXX_assert(!(Mselecti || Mselectd),
                  "ERROR (Series): other type is already slected!\n"
                  "internal inconsistency to be considered a bug");
      Mselectf=true;
      return(Mfs);
    }
    Tis& is() 
    { 
      TFXX_assert(!(Mselectd || Mselectf),
                  "ERROR (Series): other type is already slected!\n"
                  "internal inconsistency to be considered a bug");
      Mselecti=true;
      return(Mis);
    }
    Tshape shape() const
    {
      Tshape retval;
      TFXX_assert(this->selected(), "ERROR (Series): no type is selected!\n"
                  "internal inconsistency to be considered a bug");
      if (Mselectd)
      { retval=Mds.shape(); }
      else if (Mselecti)
      { retval=Mis.shape(); }
      else 
      { retval=Mfs.shape(); }
      return(retval);
    }
    Tindex first() const 
    { return this->shape().first(); }
    Tindex last() const 
    { return this->shape().last(); }
  private:
    // integer is selected
    bool Mselecti;
    // double is selected
    bool Mselectd;
    // float is selected
    bool Mselectf;
    // double type
    Tds Mds;
    // integer type
    Tis Mis;
    // float type
    Tfs Mfs;
}; // class Series

/*======================================================================*/
// data structures and type used to handle collections of index entries 
// use to collect and sort index entries required to assemble the requested
// data set

/*! \brief struct to hold an index file entry.
 *
 * This is used to collect data from the index files
 */
struct IndexEntry {
  // name of data file (with path)
  std::string filename;
  // index of trace in data file
  int itrace;
  // format of data file
  std::string dataformat;
  // WID2 line for trace
  sff::WID2 wid2;
}; // struct IndexEntry

//! a vector of index entries
typedef std::vector<IndexEntry> Tvecofindexentries;

/*----------------------------------------------------------------------*/

/*! \brief struct to hold regular expressions.
 *
 * This is used to support channel selection.
 */
struct SelectionRegexx {
  tfxx::string::regexx station;
  tfxx::string::regexx channel;
  tfxx::string::regexx instrument;
  tfxx::string::regexx auxid;
}; // struct SelectionRegexx

/*----------------------------------------------------------------------*/

/*! \brief list of links to collection entries, building a chain for one
 * channel.
 *
 * The list contains index values to the a collection of indexentries stored
 * in a Tvecofindexentries.
 * Each list is used for one single data channel (called data stream below).
 */
typedef std::list<int> Tintlist;

/*! \brief vector of lists list of collections.
 *
 * This vector collects lists of index values for several channels.
 */
typedef std::vector<Tintlist> Tvecofintlist;

/*----------------------------------------------------------------------*/

/*! \brief A comparison function class for the collection.
 *
 * This class is used to sort index entries in a temporal order.
 */
class CompareCollection {
  public:
    CompareCollection(const Tvecofindexentries& collection):
      Mcollection(collection) { }
    bool operator()(const int& i1, const int& i2)
    { return(Mcollection[i1].wid2.date<Mcollection[i2].wid2.date); }
  private:
    const Tvecofindexentries& Mcollection;
}; // class CompareCollection

/*----------------------------------------------------------------------*/

//! output operator to report an index entry
std::ostream& operator<<(std::ostream& os, const IndexEntry& entry)
{
  std::string timestring=entry.wid2.date.timestring();
  os << timestring.substr(4,23) << " ";
  os << entry.wid2.station << " ";
  os << entry.wid2.channel << " ";
  os << entry.wid2.auxid << " ";
  os << entry.wid2.instype << " ";
  os << entry.filename << " ";
  os << entry.itrace << " ";
  os << entry.dataformat << " ";
  return(os);
} // report an Index Entry

/*======================================================================*/
// data structure used to assemble actual data sets (not yet samples but
// all information necessary to actually read and collect samples)

/*! \brief struct to hold index values defining a data block within a trace.
 *
 * A block to hold the index to the input data index entry required to read
 * samples for a fraction of the output data trace together with sample index
 * values inidcating the portion of the actual input trace to be used when
 * assembling the output data.
 */
struct DataBlock {
  // link to index entry
  int ientry;
  // first sample in input data trace
  int ifirst;
  // first sample in output data trace
  int ofirst;
  // number of samples to copy from input to output
  int nsamples;
}; // struct DataBlock

/*! \brief vector of blocks.
 *
 * A vector of blocks which are required to assemble one output trace.
 */
typedef std::vector<DataBlock> Tvecofblocks;

/*! \brief vector of vector of blocks.
 *
 * A vector of vectors of blocks to collect all information required to
 * assemble all output traces.
 * Contains a Tvecofblocks for each output trace to be assembled.
 */
typedef std::vector<Tvecofblocks> Tvecoftraces;

/*======================================================================*/
// main program
// ============

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    ANYEXTRACT_VERSION "\n"
    "usage: anyextract [common options] [extract options]\n"
    "                  index [ index [...] ] outfile\n"
    "   or: anyextract -GAPFIND [common options] [gap options]\n"
    "                  index [ index [...] ]\n"
    "   or: anyextract --help|-h|-xhelp" "\n"
    "\n"
    "common options are:\n"
    "                  [-verbose] [-DEBUG] [-dump]\n"
    "                  [-type f] [-overwrite]\n"
    "                  [-sfirst date] [-slast date] [-sduration dur]\n"
    "                  [-sstation regex] [-schannel regex]\n"
    "                  [-sinstrument regex] [-sauxid regex]\n"
    "                  [-dtr f] [-tr] [-br]\n"
    "extract options are:\n"
    "                  [-GSE] [-int] [-float] [-nofree]" "\n"
    "                  [-calib f] [-calper p]" "\n"
    "                  [-RANGECHECK]\n"
    "gap options are:\n"
    "                  [-GAPFIND] [-Ggs f] [-Gcs f] [-Gprint[=file]]" "\n"
    "                  [-Gbin s] [-Gsummarize l] [-Ggpt f]" "\n"
  };

  // define help text
  char help_text[]=
  {
    "read index file(s) and extract defined parts of the dataset" "\n"
    "\n"
    "You have to use \"anyindex\" first to create one or more index" "\n"
    "files referencing the data files. \"anyextract\" will scan these" "\n"
    "index files and decide from which data files the requested" "\n"
    "dataset can be compiled. It will arrange this information" "\n"
    "and build chains of input datasets for each unique stream" "\n"
    "(defined by station, channel, instrument and auxid) and will" "\n"
    "then read and compile the time series for the output file." "\n"
    "\n"
    "The input file type including file type modifiers is defined in the\n"
    "index file. The output file type as well as the data type (float,\n"
    "double, or int) used for copying samples can be selcted on the\n"
    "command line.\n"
    "\n"
    "The program can be executed in two differen modes:\n"
    "1. data extraction: reads the index and assembles data\n"
    "2. gap analysis: reads the index and evaluates gaps in the data\n"
    "\n"
    "Command line arguments and options:\n"
    "  (command line options may be abbreviated, i.e. \"-v\", \"-verb\",\n"
    "  and \"-verbose\" are all equivalent)\n"
    "\n"
    "-help        print online help\n"
    "-xhelp       provides extended online help (explains data formats etc.)\n" 
    "\n"
    "index ...    index file(s)" "\n"
    "\n"
    "Options common to both modes:\n"
    "  -verbose     be verbose" "\n"
    "  -DEBUG       debugging output (be very verbose)" "\n"
    "  -dump        dump intermediate results" "\n"
    "\n"
    "  -overwrite   overwrite existing output files" "\n"
    "\n"
    "  Data selection:\n"
    "  -sfirst date       date and time of first sample to extract\n"
    "  -slast date        date and time of last sample to extract\n"
    "  -sduration dur     define end of time window by duration \"dur\"\n"
    "  -sstation regex    select station by regular expression \"regex\"\n"
    "  -schannel regex    select channel by regular expression \"regex\"\n"
    "  -sinstrument regex select instrument by regular expression \"regex\"\n"
    "  -sauxid regex      select auxid by regular expression \"regex\"\n"
    "\n"
    "  Options to control the handling of inconsitencies:\n"
    "  -dtr f       allow tolerance when matching times of consecutive\n"
    "               data blocks; ''f'' defines the tolerance as a fraction\n"
    "               of the sampling interval, that is tolerated for the\n"
    "               residual of sample times\n"
    "               Notice: the tolerance is tested against the residual\n"
    "               of consecutive input blocks, not against the residual\n"
    "               of all extracted samples and the next input block\n"
    "  -br          redundant input samples are a break condition;\n"
    "               program does not abort\n"
    "  -tr          tolerate redundant samples; i.e. do not abort if input\n"
    "               blocks are overlapping, which means that the same\n" 
    "               exists twice in different input blocks\n"
    "               this clears the -br option if set\n"
    "\n"
    "  Option to select file type for time series output:\n"
    "  -type f      write output file of format \"f\"\n"
    "               in GAPFIND mode this is used together with options\n"
    "               \"-Ggs\" and \"-Gcs\"\n"
    "\n"
    "Options controlling data extraction:\n"
    "  outfile      output file (do not specify this in gap finding" "\n"
    "               mode, i.e. together with option -GAPFIND)" "\n"
    "\n"
    "  Options to select file type for time series output:\n"
    "  -type f      write output file of format \"f\"\n"
    "  -GSE         write GSE compatible data" "\n"
    "               setting this option implies option -int" "\n"
    "               and option \"-type gse\"" "\n"
    "  -int         use integer data type for copying of samples" "\n"
    "  -float       use float (single precision) data type for copying of\n"
    "               samples" "\n"
    "\n"
    "  Options to adjust content of headers in output files:\n"
    "  -calib f     set calib entry in output file to f" "\n"
    "               see notes below" "\n"
    "  -calper p    set calper entry in output file to p" "\n"
    "  -nofree      do not write FREE blocks, even if output format supports\n"
    "               FREE blocks (just speeds up data I/O of subsequent\n"
    "               operations)\n"
    "\n"
    "  Option for debugging purposes:\n"
    "  -RANGECHECK  perform array index range check (debug option)" "\n"
    "\n"
    "Options controlling gap analysis:\n"
    "  -GAPFIND     find only gaps - do not extract data\n"
    "               do not specify an output file in this case\n"
    "               the -verbose option increases the verbosity of gap\n"
    "               reports\n"
    "  -Gbin s      set bin size for gap and completeness series\n"
    "  -Ggs f       gap distribution will be written to file \"f\"\n"
    "               the output file is a time series file in format\n"
    "               specified by option \"-type f\"\n"
    "               it specifies the number of missing samples in each bin\n"
    "               as specified by option \"-Gbin s\"\n"
    "  -Gcs f       completeness distribution will be written to file \"f\"\n"
    "               the output file is a time series file in format\n"
    "               specified by option \"-type f\"\n"
    "               it specifies the completeness of the data in each bin\n"
    "               as specified by option \"-Gbin s\"in per cent, where\n"
    "               100% means that no sample is missing\n"
    "  -Gprint[=f]  print a list of gaps to stdout\n"
    "               if \"f\" is defined, write this list to file \"f\"\n"
    "  -Gsummarize l set summerize level for -Gprint to \"l\"\n"
    "               l=1: summarize results for each stream\n"
    "               l=2: summarize total results only\n"
    "               l=3: just output total number of gaps\n"
    "               l=4: just output total number of missing samples\n"
    "  -Ggpt f      create gnuplot plot file with basename \"f\"\n"
    "               the filename will be complemented by the extension\n"
    "               \".gpt\"; by running gnuplot on this file, a graphical\n"
    "               display of missing data organized in bins of the size\n"
    "               specified by option \"-Gbin s\" is created in a\n"
    "               Postscript file with basename \"f\" and extension \".ps\"\n"
  };

  // define extended help text
  char extended_help_text[]=
  {
    "calib and calper" "\n"
    "----------------" "\n"
    "  The definition of the parameters calib and calper in the GSE2.0" "\n"
    "  format specification is ambigious. The format caontains no field" "\n"
    "  that is able to specify whether the sensor has the response of" "\n"
    "  an electrodynamic seismometer or an accelerometer, but the" "\n"
    "  conversion factor calib is given in units of displacement." "\n"
    "  It is not specified, whether the factor has to be applied" "\n"
    "  with deconvolution (which would be reasonable, since units" "\n"
    "  are given relative to displacement) or without deconvolution" "\n"
    "  as is common practice. For this reason there exists only a kind" "\n"
    "  of convention how this fields are used. Since these fields at least" "\n"
    "  require the knowledge of the instrument's response, the program" "\n"
    "  usually sets both parameters to the default value -1. in the" "\n"
    "  output data. But beware: Some programs like SeismicHandler" "\n"
    "  rely on these fields, when reading GSE data. They will reject" "\n"
    "  the default -1. and replace the value by 1., which results in" "\n"
    "  a scaling of amplitudes with a factor 2*pi which is usually" "\n"
    "  not desired. In these cases it is save to choose" "\n"
    "  -calib 1. -calper 6.2831853" "\n"
    "  or" "\n"
    "  -calib 0.15915494 -calper 1." "\n"
    "  With these settings, the input data will not be scaled when" "\n"
    "  read into SeismicHandler." "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"verbose",arg_no,"-"},
    // 2: overwrite mode
    {"overwrite",arg_no,"-"},
    // 3: first date
    {"sfirst",arg_yes,"-"},
    // 4: last date
    {"slast",arg_yes,"-"},
    // 5: select station
    {"sstation",arg_yes,".*"},
    // 6: select channel
    {"schannel",arg_yes,".*"},
    // 7: select instrument
    {"sinstrument",arg_yes,".*"},
    // 8: select auxid
    {"sauxid",arg_yes,".*"},
    // 9: debug mode
    {"DEBUG",arg_no,"-"},
    // 10: range check debug mode
    {"RANGECHECK",arg_no,"-"},
    // 11: find gaps only
    {"GAPFIND",arg_no,"-"},
    // 12: tolerate input time residuals
    {"dtr",arg_yes,"0."},
    // 13: tolerate redundant input samples
    {"tr",arg_no,"-"},
    // 14: break chain on redundant input samples
    {"br",arg_no,"-"},
    // 15: dump intermediate results
    {"dump",arg_no,"-"},
    // 16: define window by duration
    {"sduration",arg_yes,"-"},
    // 17: extract integer data
    {"int",arg_no,"-"},
    // 18: write GSE compatible SFF
    {"GSE",arg_no,"-"},
    // 19: set calib 
    {"calib",arg_yes,"-1."},
    // 20: set calper
    {"calper",arg_yes,"-1."},
    // 21: detailed help text
    {"xhelp",arg_no,"-"},
    // 22: do not write FREE blocks
    {"nofree",arg_no,"-"},
    // 23: select output file format
    {"type",arg_yes,"sff"},
    // 24: use float type series for copying
    {"float",arg_no,"-"},
    // 25: set bin size for completeness analysis
    {"Gbin",arg_yes,"0/1"},
    // 26: print a list of gaps 
    {"Gprint",arg_opt,"-"},
    // 27: write gap time series to file
    {"Ggs",arg_yes,"-"},
    // 28: write completeness time series to file
    {"Gcs",arg_yes,"-"},
    // 29: set summarize level
    {"Gsummarize",arg_yes,"0"},
    // 30: gnuplot file
    {"Ggpt",arg_yes,"-"},
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
  if (cmdline.optset(0) || cmdline.optset(21))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    datrw::supported_data_types(cerr);
    if (cmdline.optset(21))
    {
      cerr << endl << extended_help_text << endl;
      datrw::online_help(cerr);
      cerr << endl;
      cerr << libtime::usage_time_format_string << endl;
    }
    exit(0);
  }

  Options opt;

  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.firstset=cmdline.optset(3);
  opt.lastset=cmdline.optset(4);
  if (opt.firstset) { opt.first=libtime::TAbsoluteTime(cmdline.string_arg(3)); }
  if (opt.lastset) { opt.last=libtime::TAbsoluteTime(cmdline.string_arg(4)); }
  opt.selstation=cmdline.string_arg(5);
  opt.selchannel=cmdline.string_arg(6);
  opt.selinstrument=cmdline.string_arg(7);
  opt.selauxid=cmdline.string_arg(8);
  opt.debug=cmdline.optset(9);
  if (opt.debug) { opt.verbose=true; }
  opt.rangecheck=cmdline.optset(10);
  opt.findgapsonly=cmdline.optset(11);
  opt.allowtolerance=cmdline.optset(12);
  opt.relativetolerance=cmdline.double_arg(12);
  opt.allowduplicatesamples=cmdline.optset(13);
  opt.breakonduplicatesamples=cmdline.optset(14);
  opt.dumpintermediateresults=cmdline.optset(15);
  // handle setting time window by duration
  if (cmdline.optset(16))
  {
    TFXX_assert(opt.firstset,
                "ERROR: you must specify the beginning of the time window too");
    TFXX_assert(!opt.lastset,
                "ERROR: conflicting definitions for time window");
    opt.lastset=true;
    opt.last=opt.first+libtime::TRelativeTime(cmdline.string_arg(16));
  }
  opt.integerdata=(cmdline.optset(17)||cmdline.optset(18));
  opt.GSEoutput=cmdline.optset(18);
  opt.setcalib=cmdline.optset(19);
  opt.newcalib=cmdline.double_arg(19);
  opt.setcalper=cmdline.optset(20);
  opt.newcalper=cmdline.double_arg(20);
  opt.nofreeblock=cmdline.optset(22);
  opt.outputformat=cmdline.string_arg(23);
  opt.floatdata=cmdline.optset(24);
  opt.binsize=cmdline.string_arg(25);
  opt.printgaps=cmdline.optset(26);
  opt.gapoutputfile=cmdline.string_arg(26);
  opt.writegapseries=cmdline.optset(27);
  opt.gapseriesfile=cmdline.string_arg(27);
  opt.writecompleteness=cmdline.optset(28);
  opt.completenessseriesfile=cmdline.string_arg(28);
  opt.summarizelevel=static_cast<unsigned int>(cmdline.int_arg(29));
  opt.providegnuplotplot=cmdline.optset(30);
  opt.gnuplotfile=cmdline.string_arg(30);

  // is any of the GAP analysis modes activated
  bool doGAPanalysis=opt.findgapsonly
    || opt.printgaps || opt.writegapseries || opt.writecompleteness
    || opt.providegnuplotplot;

  // hierarchical definition of duplicate sample conditions
  if (opt.allowduplicatesamples) opt.breakonduplicatesamples=false;

  if (opt.verbose)
  {
    cout << ANYEXTRACT_VERSION << endl;
  }

  // check special file format settings
  if (opt.GSEoutput)
  {
    cout << "GSE output selected: ";
    opt.outputformat="gse";
    opt.integerdata=true;
    cout << "set output format to \"" << opt.outputformat << "\" ";
    cout << "set internal data type to integer" << endl;
  }

  // setup vector of input filenames and read command line
  Tvecofstrings infiles;
  std::string outfile;
  TFXX_assert(cmdline.extra(), "ERROR: missing index file!");
  outfile=cmdline.next();
  // collect input files
  while (cmdline.extra()) 
  { 
    // cout << outfile << " ";
    infiles.push_back(outfile);
    outfile=cmdline.next();
    // cout << outfile << " # ";
  }
  if (doGAPanalysis)
  {
    infiles.push_back(outfile);

    if (opt.verbose || (opt.summarizelevel < 3))
    {
      cout << "SPECIAL MODE: find gaps in input data - do not extract data"
        << endl;
    }
  }
  else
  {
    TFXX_assert((infiles.size()>0), "ERROR: missing output file!");
  }

  /*----------------------------------------------------------------------*/
  // stage 0 dump:
  if (opt.dumpintermediateresults)
  {
    cout << endl;
    cout << "DUMP stage 0 results: names of index files to process" << endl;
    Tvecofstrings::const_iterator iv=infiles.begin();
    while (iv!=infiles.end())
    {
      cout << "  " << *iv << endl;
      ++iv;
    }
    cout << "  output will be written to:" << endl
      << "  " << outfile << endl;
    cout << endl;
  }


  /*
   * strategy:
   *
   * 1. read all index file entries and check whether they match the selection
   *    fill them into a vector if they do
   * 2. collect matching data streams in order of increasing time
   * 3. find gaps and setup traces for output file
   * 4. read data and write it to the output file
   */

  /*======================================================================*/
  /* 1. scan index files and collect matching entries
   *
   * In this section all index files will be read. Each entry will be checked
   * whether ist matches the station, channel, instrument, and auxid selection
   * and whether its time window lies within the selected window or at least
   * overlaps with the selection. All matching entries will be collected
   * in a global variable
   *
   *   Tvecofindexentries collection;
   *
   */
  if (opt.verbose || opt.findgapsonly)
  {
    cout << endl;
    cout << "step 1: collect matching index entries" << endl;
    cout << "data selection:" << endl;
    if (opt.firstset) 
    { cout << "  first sample:      " << opt.first.timestring() << endl; }
    else
    { cout << "  first sample:      no time specified" << endl; }
    if (opt.lastset) 
    { cout << "  last sample:       " << opt.last.timestring() << endl; }
    else
    { cout << "  last sample:       no time specified" << endl; }
    if (opt.allowduplicatesamples)
      cout << "  duplicate input samples are tolerated" << endl;
    if (opt.breakonduplicatesamples)
      cout << "  duplicate input samples are a break condition" << endl;
    if (opt.allowtolerance)
      cout << "  sampling raster tolerance is " 
        << opt.relativetolerance
        << " of sampling interval" << endl;
  }

  // prepare regular expressions
  SelectionRegexx rgxx;
  rgxx.station.expression(opt.selstation);
  rgxx.channel.expression(opt.selchannel);
  rgxx.instrument.expression(opt.selinstrument);
  rgxx.auxid.expression(opt.selauxid);

  if (opt.verbose || opt.findgapsonly)
  {
    cout << "  station:           " << rgxx.station.expression() << endl;
    cout << "  channel:           " << rgxx.channel.expression() << endl;
    cout << "  instrument:        " << rgxx.instrument.expression() << endl;
    cout << "  auxid:             " << rgxx.auxid.expression() << endl;
  }

  // vector of matching entries (my collection)
  Tvecofindexentries collection;

  // scan
  Tvecofstrings::const_iterator infile=infiles.begin(); 
  while( infile!=infiles.end())
  {
    if (opt.verbose || opt.findgapsonly)
    { cout << "** open next index file: " << *infile << endl; }
    std::ifstream is(infile->c_str());

    // number of entries scanned
    int nscanned=0;
    // start reading here to trigger is flags
    IndexEntry indexentry;
    is >> indexentry.filename;
    while(is.good())
    {
      if (opt.debug)
      { cout << "DEBUG: read next entry:" << endl; }
      // first, just read
      is >> indexentry.itrace;
      std::string formatID;
      is >> formatID;
      indexentry.dataformat=formatID;
      // just skip EOL
      getline(is, formatID);
      indexentry.wid2.read(is);
      if (opt.debug)
      {
        cout << "  filename:        " << indexentry.filename << endl;
        cout << "  trace #:         " << indexentry.itrace << endl;
        cout << "  data format:     " << 
          indexentry.dataformat << endl;
        cout << "  " << indexentry.wid2.line();
      }

      // then, check selection parameters
      bool match=true;
      if (opt.firstset)
      { match=(opt.first<=sff::wid2lastsample(indexentry.wid2)); }
      if (opt.lastset && match)
      { match=(opt.last>=indexentry.wid2.date); }
      if (match) { match=rgxx.station.match(indexentry.wid2.station); }
      if (match) { match=rgxx.channel.match(indexentry.wid2.channel); }
      if (match) { match=rgxx.instrument.match(indexentry.wid2.instype); }
      if (match) { match=rgxx.auxid.match(indexentry.wid2.auxid); }

      if (match)
      {
        collection.push_back(indexentry);
        if (opt.verbose)
        {
          cout << "  match: " << indexentry << endl;
        }
      }
      
      // fill this field before reentering the loop
      is >> indexentry.filename;
      ++nscanned;
    }
    if (opt.verbose)
    {
      cout << "  scanned " << nscanned << " entries in index file" << endl;
    }
    ++infile;
  }
  if (opt.verbose)
  {
    cout << "found " << collection.size() << " matching entries" << endl;
  }

  /*----------------------------------------------------------------------*/
  // stage 1 dump:
  if (opt.dumpintermediateresults)
  {
    cout << endl;
    cout << "DUMP stage 1 results: all matching entries" << endl;
    Tvecofindexentries::const_iterator iv=collection.begin();
    while(iv != collection.end())
    {
      const IndexEntry& entry= (*iv);
      const sff::WID2& wid2=entry.wid2;
      cout << "  " << wid2.station
        << " " << wid2.channel
        << " " << wid2.auxid
        << " " << wid2.instype
        << " dt=" << wid2.dt << "s"
        << " " << wid2.date.timestring().substr(4,23) 
        << " n=" << wid2.nsamples
        << " " << entry.filename
        << " #" << entry.itrace
        << " " << entry.dataformat << endl;
      ++iv;
    }
    cout << endl;
  }

  /*======================================================================*/
  /* 2. sort collection
   *
   * In this section the program scans the collection of index entries in the
   * global variable
   *
   *   Tvecofindexentries collection;
   *
   * The program compiles a set of links into the collection. The links are
   * organized as follows:
   *
   * For each unique stream (defined by station, channel, sampling interval and
   * auxid) a list of type Tintlist is created. The entries of this list
   * refer to entries in the global collection in the order of increasing date
   * of first sample. All these lists are collected in the global variable
   *
   *   Tvecofintlist chain;
   *
   */
  if (opt.verbose || opt.findgapsonly)
  {
    cout << endl;
    cout << "step 2: sort collected index entries" << endl;
  }

  // compare WID2
  sff::WID2compare 
    equalstreams(sff::Fstation|sff::Fchannel|sff::Fdt|sff::Fauxid);

  // build vector of chains
  if (opt.verbose) { cout << "  fill raw chains" << endl; }
  Tvecofintlist chain;
  for (unsigned int ientry=0; ientry<collection.size(); ++ientry)
  {
    // if vector is empty, it's easy
    if (chain.size()==0)
    {
      Tintlist newlist;
      newlist.push_back(ientry);
      chain.push_back(newlist);
    }
    else
    {
      // find matching
      int imatch=-1;
      for (unsigned int ilist=0; ilist<chain.size(); ++ilist)
      {
        if (equalstreams(collection[ientry].wid2,
                         collection[chain[ilist].front()].wid2))
        { imatch=ilist; }
      }
      if (imatch<0)
      {
        // found no matching chain
        Tintlist newlist;
        newlist.push_back(ientry);
        chain.push_back(newlist);
      }
      else
      {
        chain[imatch].push_back(ientry);
      }
    }
  }

  // sort vector of chains
  if (opt.verbose) { cout << "  sort chain(s) for " 
    << chain.size() << " unique stream(s)" << endl; }
  CompareCollection comparer(collection);
  for (unsigned int ilist=0; ilist<chain.size(); ++ilist)
  { chain[ilist].sort(comparer); }

  // tell result
  if (opt.verbose)
  {
    for (unsigned int ilist=0; ilist<chain.size(); ++ilist)
    {
      const Tintlist& thelist=chain[ilist];
      cout << "  next stream:" << endl;
      const IndexEntry& entry=collection[thelist.front()];
      cout << "    station:   " << entry.wid2.station;
      cout << " channel:   " << entry.wid2.channel;
      cout << " instrument:   " << entry.wid2.instype;
      cout << " auxid:     " << entry.wid2.auxid << endl;
      Tintlist::const_iterator iinlist=thelist.begin(); 
      while( iinlist!=thelist.end())
      {
        cout << "    " << collection[*iinlist] << endl;
        sff::WID2 thewid2=collection[*iinlist].wid2;
        if (opt.debug)
        {
          cout << "DEBUG: "
            << collection[*iinlist].filename
            << " " << collection[*iinlist].itrace 
            << " " << collection[*iinlist].dataformat << endl;
          cout << "DEBUG: " << thewid2.line();
        }
        cout << "      " << thewid2.date.timestring()
          << " - " << sff::wid2lastsample(thewid2).timestring()
          << endl;
        ++iinlist;
      }
    }
  }

  /*----------------------------------------------------------------------*/
  // stage 2 dump:
  if (opt.dumpintermediateresults)
  {
    cout << endl;
    cout << "DUMP stage 2 results: chains for each channel" << endl;
    Tvecofintlist::const_iterator iv=chain.begin();
    while (iv != chain.end())
    {
      Tintlist::const_iterator il=iv->begin();
      const IndexEntry& head = collection[*il];
      const sff::WID2& hw2=head.wid2;
      cout << hw2.station 
        << " " << hw2.channel 
        << " " << hw2.auxid 
        << " " << hw2.instype 
        << " (dt=" << hw2.dt << "s):" << endl;
      while (il != iv->end())
      {
        const IndexEntry& entry = collection[*il];
        libtime::TAbsoluteTime lastdate=sff::wid2lastsample(entry.wid2);
        cout << "  "
          << " #";
        cout.width(2);
        cout.fill('0');
        cout << *il << ": "
          << entry.wid2.date.timestring().substr(4,23) << " - "
          << lastdate.timestring().substr(4,23) 
          << " (" << entry.filename 
          << " #" << entry.itrace 
          << " " << entry.dataformat
          << ")" << endl;
        ++il;
      }
      ++iv;
    }
  }

  /*======================================================================*/
  /* 3. setup traces
   *
   * In this section the program compiles continuous sets of data that
   * appear in the Tintlist elements in the global variable
   *
   *   Tvecofintlist chain;
   *
   * Each continuous set of data may contain samples from one or more files.
   * These continuous sets are defined by variables of type Tvecofblocks which
   * are vectors of elements of type DataBlock. There is one entry of type
   * DataBlock for each file that contributes samples to the continuous set of
   * data. Within the DataBlock a copy operation is defined. Each copy
   * operation is defined by the source file (referenced by ientry, which is
   * the element index to the global collection), the first sample to read
   * (ifirst) from the input file and the sample this first value has to be
   * written to in the continuous set (ofirst), and the number of samples to
   * be copied (nsamples).
   *
   * These continuous sets of data are collected in the global variable
   *
   *   Tvecoftraces trace;
   *
   */
  if (opt.verbose || opt.findgapsonly)
  {
    cout << endl;
    cout << "step 3: define copy operations" << endl;
  }

  // define a time span of zero length, for comparison
  // (will be modified later, if requested)
  libtime::TRelativeTime tolerancewindow(0);
  // set of traces to define
  Tvecoftraces trace;

  // cycle through vector of unique data streams
  // -------------------------------------------
  for (unsigned int ichain=0; ichain<chain.size(); ++ichain)
  {
    // new trace = vector of blocks
    Tvecofblocks block;
    // get reference to list that must be scanned next
    // each list contains links to all data from a specific stream (defined by
    // station, channel, instrument and auxid) within the requested time
    // interval
    const Tintlist& thelist=chain[ichain];
    if (opt.verbose)
    {
      IndexEntry &theentry(collection[thelist.front()]);
      cout << "scan chain for stream" << endl;
      cout << " * station: " << theentry.wid2.station;
      cout << " * channel: " << theentry.wid2.channel;
      cout << " * instrument: " << theentry.wid2.instype;
      cout << " * auxid: " << theentry.wid2.auxid;
      cout << " * dt=" << theentry.wid2.dt << "s" << endl;
    }
    // time of next sample
    libtime::TAbsoluteTime nextdate;
    // next output trace sample to continue with
    int nextosample=0;
    // keep track of streams
    bool newstream=true;

    // scan chain for this unique data stream 
    // --------------------------------------
    Tintlist::const_iterator ilist=thelist.begin(); 
    while (ilist!=thelist.end())
    {
      // get reference to entry to work on
      const IndexEntry& entry=collection[*ilist];

      // report
      if (opt.verbose) 
      {
        if (block.empty())
        {
          cout << "  * begin a new output trace" << endl;
        }
        cout << "   * process collection entry #" << *ilist << ": "
          << entry.filename << " "
          << entry.itrace << " "
          << entry.dataformat << endl;
        cout << "      "
          << entry.wid2.date.timestring()
          << " - "
          << sff::wid2lastsample(entry.wid2).timestring() << endl;
      }

      // next block:
      // this must define a copy operation from a trace in a source file
      // to a destination trace
      DataBlock theblock;
      // link to entry in global collection 
      // which is the links to the data source trace
      theblock.ientry= *ilist;
      // flag to indicate, that last sample selected (by time of last sample)
      // is already contained in data block
      bool complete=false;

      // in case of tolerance, adjust tolerancewindow
      if (opt.allowtolerance)
      {
        tolerancewindow
          =libtime::double2time(opt.relativetolerance*entry.wid2.dt);
      }

      // adjust date of next sample to extract if we start a new stream
      // or a new block (the latter only in case we allow duplicate samples)
      if (newstream || (block.empty() && opt.allowduplicatesamples))
      {
        // set nextdate to first sample requested, if appropriate
        if (opt.firstset && (opt.first > entry.wid2.date))
        {
          nextdate = opt.first;
        }
        // set to beginning of current entry otherwise
        else
        {
          nextdate = entry.wid2.date;
        }
      }

      /* the conditions for duplicate samples are (in relaxing order)
       *
       * 1. duplicate input samples are fatal (default)
       * 2. break chain on duplicate samples
       * 3. duplicate samples are allowed
       */

      // prepare partial GAP contitions
      bool gc_not_empty = (!block.empty());
      if (!gc_not_empty) { nextdate=entry.wid2.date; }
      bool gc_no_duplicates_allowed = (!opt.allowduplicatesamples);
      bool gc_raster_residual_exceeds_tolerance
        = (sff::wid2isamplerest(entry.wid2,nextdate) > tolerancewindow);
      long int gc_sample_index 
        = sff::wid2isample(entry.wid2,nextdate);
      bool gc_sample_overlap = (gc_sample_index > 0);
      bool gc_sample_match = (gc_sample_index == 0);
      bool gc_sample_gap = (gc_sample_index < 0);
      bool gc_no_new_data = ((nextdate - tolerancewindow)
                             > sff::wid2lastsample(entry.wid2));
      bool gc_break_on_duplicate = opt.breakonduplicatesamples;

      // report partial GAP conditions
      if (opt.debug)
      {
        cout << "DEBUG GAP conditions:" << endl;
        if (gc_not_empty)
        cout << "-1--- block is not empty" <<endl;
        if (gc_raster_residual_exceeds_tolerance)
        cout << "-2--- time raster residual exceeds tolerance" <<endl;
        if (gc_no_duplicates_allowed)
        cout << "-3--- duplicate samples are not allowed" << endl;
        if (gc_sample_overlap)
        cout << "-4--- sample windows overlap" << endl;
        if (gc_sample_match)
        cout << "-5--- sample windows match" << endl;
        if (gc_sample_gap)
        cout << "-6--- sample windows gap" << endl;
        cout << "----- sample index: " << gc_sample_index << endl;
      }
        
      // enter GAP conditional
      // ---------------------
      // this is a large block of conditionals, that distinguishes the
      // different cases of GAP and non-GAP conditions, we might meet here

      // handle GAP condition first
      if (gc_not_empty 
          && (gc_sample_gap 
              || gc_raster_residual_exceeds_tolerance
              || (gc_no_duplicates_allowed
                  && gc_sample_overlap)))
      {
        // check fatal condition
        if (gc_sample_overlap 
            && gc_no_duplicates_allowed
            && (!gc_break_on_duplicate))
        {
          cout << "duplicates: " << endl;
          cout << "  next sample requested: "
            << nextdate.timestring() << endl;
          cout << "  is later than first sample in input: "
            << entry.wid2.date.timestring() << endl;
          cout << "  when processing entry" << endl;
          cout << entry << endl;
          TFXX_abort("ERROR: duplicate input samples are fatal!");
        }
        if (opt.verbose || opt.findgapsonly)
        {
          cout << "! GAP ("
            << entry.wid2.station << ","
            << entry.wid2.channel << ","
            << entry.wid2.auxid << "): ";
          if (nextdate < entry.wid2.date)
          {
            cout << nextdate.timestring() << " (expected)" << " < "
              << entry.wid2.date.timestring() << " (actual)" << endl;
          }
          else if ((entry.wid2.date <= nextdate) &&
                   (sff::wid2isamplerest(entry.wid2,nextdate) 
                    > tolerancewindow))
          {
            cout << nextdate.timestring() << " (expected)" << " >= "
              << entry.wid2.date.timestring() << " (actual)" << endl
              << " && " << "           "
              << sff::wid2isamplerest(entry.wid2,nextdate).timestring()
              << " (residual to sampling raster)"
              << " > " << tolerancewindow.timestring() 
              << " (tolerance)" << endl;
            /*
            libtime::TRelativeTime diff=entry.wid2.date-nextdate;
            cout << "NOTICE: diff=" << diff.timestring()<< endl;
            libtime::TRelativeTime dt=libtime::double2time(entry.wid2.dt);
            cout << "NOTICE: dt=" << dt.timestring()<< endl;
            long int i=diff/dt;
            cout << "NOTICE: i=" << i<< endl;
            libtime::TRelativeTime ti=diff%dt;
            cout << "NOTICE: ti=" << ti.timestring()<< endl;
            */
          }
          else if (gc_sample_overlap && gc_no_duplicates_allowed)
          {
            cout << "duplicates: ";
            cout << nextdate.timestring() << " (expected)" << " > "
              << entry.wid2.date.timestring() << " (actual)" << endl;
          }
          else
          {
            TFXX_abort("ERROR: undefined condition for GAP!");
          }
        } // if (opt.verbose || opt.findgapsonly)

        // gap found: start new trace
        trace.push_back(block); 
        // begin with new vector of blocks (new trace)
        block.clear();
        // begin with new trace
        nextosample=0;
      }
      // skip if file provides no new data
      else if (gc_no_new_data)
      {
        if (opt.verbose)
        {
          cout << "     skip this (next requested: "
            << nextdate.timestring() << ")" << endl;
        }
        ++ilist;
      }
      // define copy operation for samples
      else
      {
        if (opt.verbose)
        {
          cout << "     define copy (next requested: "
            << nextdate.timestring() << ")" << endl;
        }
        // remember value of next destination sample to use
        theblock.ofirst= nextosample;
        // default: first sample of source
        theblock.ifirst=0;
        // if first in block, find first to extract
        if (!block.empty())
        {
          TFXX_assert((entry.wid2.date <= (nextdate+tolerancewindow)),
                      "ERROR: unexpected GAP(1): check program!");
          TFXX_assert((sff::wid2isamplerest(entry.wid2,nextdate)
                       <= tolerancewindow),
                      "ERROR: unexpected GAP(2): check program!");
          theblock.ifirst=sff::wid2isample(entry.wid2,nextdate); 
        }
        else if (opt.firstset) 
        {
          if (entry.wid2.date <= opt.first) 
          { theblock.ifirst=sff::wid2isample(entry.wid2,opt.first); }
        }
        // calculate number of samples to read from input
        // if the last sample is within this block
        theblock.nsamples=entry.wid2.nsamples-theblock.ifirst;
        if (opt.lastset)
        {
          if (sff::wid2lastsample(entry.wid2)>opt.last)
          {
            theblock.nsamples=sff::wid2isample(entry.wid2,opt.last)-
              theblock.ifirst+1;
            complete=true;
          }
        }
        // set values to continue with next block
        nextosample += theblock.nsamples;
        nextdate=sff::wid2isample(entry.wid2,
                                  theblock.nsamples+
                                  theblock.ifirst);
        if (opt.verbose)
        {
          cout << "     copy input samples "
            << theblock.ifirst << "-" << theblock.ifirst+theblock.nsamples-1 
            << " to output samples "
            << theblock.ofirst << "-" << nextosample-1 
            << "(" << theblock.ofirst+theblock.nsamples-1 << ")" << endl
            << "      "
            << sff::wid2isample(entry.wid2,theblock.ifirst).timestring()
            << " - "
            << sff::wid2isample(entry.wid2,theblock.ifirst+
                                theblock.nsamples-1).timestring()
            << endl;
        }
        // add this block to trace
        block.push_back(theblock);
        // if complete: add this trace to vector of traces
        if (complete) 
        { 
          if (opt.verbose)
          {
            cout << "   * time window is complete" << endl;
          }
          trace.push_back(block); 
          // begin with new vector of blocks (new trace)
          block.clear();
          // begin with new trace
          nextosample=0;
        } // if (complete)
        // next entry
        ++ilist;
      } // if: GAP or skip condition
      // the large contitional is finished
      // ---------------------------------

      // we finished the first entry in the chain for this data stream, 
      // at least
      newstream=false;

    } // while (ilist!=thelist.end())
      // at this point all chain entries for this unique data stream are
      // scanned; go to next data stream
      
    // is there still a block to be finished?
    if (!block.empty()) 
    {
      if (opt.verbose)
      { cout << "!  * time window can not be completed!" << endl; }
      trace.push_back(block); 
    } // if (!block.empty())

  } // for (unsigned int ichain=0; ichain<chain.size(); ++ichain)
    // at this point all entries in the chains for all unique data streams
    // are in some way placed in the output copy definitions

  /*----------------------------------------------------------------------*/
  // stage 3 dump:
  if (opt.dumpintermediateresults)
  {
    cout << endl;
    cout << "DUMP stage 3 results: trace copy operations" << endl;
    Tvecoftraces::const_iterator iv=trace.begin();
    int itrace=0;
    while (iv != trace.end())
    {
      const Tvecofblocks& block= (*iv);
      Tvecofblocks::const_iterator ivb=block.begin();
      const IndexEntry& headentry = collection[ivb->ientry];
      const sff::WID2& wid2=headentry.wid2;
      cout << "  #";
      cout.width(2);
      cout.fill('0');
      cout << ++itrace;
      cout << "  " << wid2.station
        << " " << wid2.channel
        << " " << wid2.auxid
        << " " << wid2.instype
        << " dt=" << wid2.dt << "s"
        << endl;
      while (ivb != block.end())
      {
        const DataBlock& theblock = (*ivb);
        const IndexEntry& entry = collection[ivb->ientry];
        const sff::WID2& ew2=entry.wid2;
        long int lastinputsample=theblock.ifirst+theblock.nsamples-1;
        cout << "  ";
        cout.width(5);
        cout << theblock.ifirst << "-";
        cout.width(5);
        cout << lastinputsample << " -> ";
        cout.width(6);
        cout << theblock.ofirst << "-";
        cout.width(6);
        cout << theblock.ofirst+theblock.nsamples-1;
        cout << " " << ew2.date.timestring().substr(4,23);
        cout << " - ";
        cout << sff::wid2isample(ew2,lastinputsample).timestring().substr(4,23);
        cout << endl;
        ++ivb;
      }
      ++iv;
    }
    cout << endl;
  }

  /*======================================================================*/
  /* stage 4
   *
   * distinguishes between data extraction mode (default) and gap finding
   *
   * First a file FREE block is set up reporting the parameters of data
   * selection. This block is also used to report to the user (not only to the
   * output data file).
   *
   * The code for stage is then divided into two large section by the very
   * first conditional. If doGAPanalysis is true several tasks for analyzing
   * the completeness of the input data as represented by the index entries
   * can be executed. The program finished then immediately without extracting
   * data. If not, data samples are actually extracted.
   */
  
  // prepare file free block and write
  sff::FREE filefree;
  filefree.append(ANYEXTRACT_VERSION);
  filefree.append("selection:");
  if (opt.firstset)
  {
    filefree.append(std::string("* first sample: ")+opt.first.timestring());
  }
  else
  {
    filefree.append(std::string("* first sample: any"));
  }
  if (opt.lastset)
  {
    filefree.append(std::string("* last sample:  ")+opt.last.timestring());
  }
  else
  {
    filefree.append(std::string("* last sample:  any"));
  }
  filefree.append(std::string("* station:      ")+opt.selstation);
  filefree.append(std::string("* channel:      ")+opt.selchannel);
  filefree.append(std::string("* instrument:   ")+opt.selinstrument);
  filefree.append(std::string("* auxid:        ")+opt.selauxid);
  if (opt.allowduplicatesamples)
    filefree.append("* duplicate input samples are tolerated");
  if (opt.breakonduplicatesamples)
    filefree.append("* duplicate input samples are a break condition");
  if (opt.allowtolerance)
  {
    std::ostringstream oss;
    oss << "* sampling raster tolerance is " 
      << opt.relativetolerance
      << " of sampling interval";
    filefree.append(oss.str());
  }

  /* Conditional
   * -----------
   * distinguish between gap analysis and data extraction
   */
  if (doGAPanalysis)
  {
    /*======================================================================*/
    /* 4. gap analysis
     *
     * This block contains several tasks to analyse the completeness of the
     * data as being represented by the index entries previously read.
     *
     * a) Cycle through all traces defined for data output. These traces
     *    actually reflect all chunks of contiguous data being present in the
     *    input.
     *    The result of this analysis is stored in contiguouslist. Input data
     *    is reported immediately in verbose mode.
     * b) If analyzecompleteness is true, a vector of gaps (vecofgaps) is
     *    prepared by function gaps as the base for testing data completeness.
     *    The vector is reported immediately if requested (opt.printgaps).
     *
     * After that:
     * - for presenting a completeness or gaps series, a series of gaps is
     *   produced from the vecofgaps
     * - a completeness series is just a transformation of the gap series
     * - is a gnuplot control file is required this is prepared first
     */
    if (opt.verbose || opt.findgapsonly)
    {
      cout << endl;
      cout << "step 4: report contiguous data" << endl;
    }

    // report selection in verbose mode
    // --------------------------------
    if (opt.verbose)
    {
      cout << filefree;
    } // if (opt.verbose)

    /*----------------------------------------------------------------------*/

    /* a) prepare collection of statistics
     * -----------------------------------
     */
    libtime::TAbsoluteTime earliesttime;
    libtime::TAbsoluteTime latesttime;
    TContiguouslist contiguouslist;

    if (opt.verbose || (opt.summarizelevel < 3))
    {
      cout << "chunks of contiguous data:" << std::endl;
    }

    bool analyzecompleteness=opt.printgaps
      || opt.writegapseries || opt.writecompleteness
      || opt.providegnuplotplot;

    // cycle through traces
    for (unsigned int itrace=0; itrace<trace.size(); ++itrace)
    {
      if (opt.verbose) { cout << "compile trace #" << itrace+1 << ":" << endl; }
      // prepare dataspace
      Tvecofblocks &thetrace(trace[itrace]);
      int totalsamples=0;
      for (unsigned int iblock=0; iblock<thetrace.size(); ++iblock)
      {
        totalsamples+=thetrace[iblock].nsamples;
      }
      sff::FREE tracefree;
      sff::WID2 tracewid2;

      // prepare WID2 line
      tracewid2=collection[thetrace[0].ientry].wid2;
      tracewid2.nsamples=totalsamples;
      tracewid2.date=sff::wid2isample(tracewid2,thetrace[0].ifirst);

      tracefree.append("data from:");
      for (unsigned int iblock=0; iblock<thetrace.size(); ++iblock)
      {
        DataBlock &theblock(thetrace[iblock]);
        IndexEntry &theentry(collection[theblock.ientry]);
        std::ostringstream freeline;
        freeline << theentry.filename << 
          " trace #" << theentry.itrace <<
          " format: " << theentry.dataformat;
        tracefree.append(freeline.str());
      }
      // report block
      libtime::TAbsoluteTime lastdate=sff::wid2lastsample(tracewid2);
      if (opt.verbose || (opt.summarizelevel < 3))
      {
        cout << tracewid2.station << " " << tracewid2.channel << " "
          << tracewid2.auxid << " "
          << tracewid2.date.timestring().substr(4,26) << " - "
          << lastdate.timestring().substr(4,26) << std::endl;
      }
      // finished reading, write now
      if (opt.verbose) 
      {
        cout << tracewid2;
        cout << tracefree;
      }
        
      /*----------------------------------------------------------------------*/
      // record statistics

      if (analyzecompleteness)
      {
        // initalize times or record time span and statistics
          
        /* adjust values times for earliest and latest sample being expected
         * input data: either they are explicitely defined by command line
         * parameters or take the earliest and latest sample being present in
         * any of the input streams
         */
        if (itrace==0)
        {
          if (opt.firstset) 
          {
            earliesttime=opt.first;
          }
          else
          {
            earliesttime=tracewid2.date; 
          }
          if (opt.lastset) 
          { 
            latesttime=opt.last; 
          }
          else
          { 
            latesttime=lastdate; 
          }
        }
        else
        {
          if ((!opt.firstset) && (tracewid2.date < earliesttime))
          {
            earliesttime=tracewid2.date; 
          }
          if ((!opt.lastset) && (lastdate > latesttime))
          {
            latesttime=lastdate; 
          }
        }

        // record properties of this chunk of contiguous data
        Contiguous conti;
        conti.first=tracewid2.date;
        conti.last=lastdate;
        conti.dt=libtime::double2time(tracewid2.dt);
        conti.station=tracewid2.station;
        conti.channel=tracewid2.channel;
        conti.auxid=tracewid2.auxid;
        contiguouslist.push_back(conti);
      } // if (opt.writestatistics)

    } // for (unsigned int itrace=0; itrace<trace.size(); ++itrace)

    // A collection of all chunks of contiguous data is now present in
    // contiguouslist. This is the base of all further analyses.

    /*----------------------------------------------------------------------*/
    // any further gap analysis?

    if (analyzecompleteness)
    {
      TFXX_debug(opt.debug, "main",
                 "analyze contiguouslist and create vector of gaps");

      /* b) create vector of gaps
       */
      Tvecofgaps vecofgaps=gaps(earliesttime, latesttime, 
                                contiguouslist, opt.debug); 

      if (opt.printgaps)
      {
        if (opt.gapoutputfile!="-")
        {
          if (!opt.overwrite)
          {
            datrw::abort_if_exists(opt.gapoutputfile);
          }
          std::ofstream os(opt.gapoutputfile.c_str());
          printgaps(os, vecofgaps, earliesttime, latesttime, 
                    opt.summarizelevel);
        }
        else
        {
          std::cout << std::endl;
          printgaps(std::cout, vecofgaps, earliesttime, latesttime, 
                    opt.summarizelevel);
        }
      } // if (opt.printgaps)

      /*----------------------------------------------------------------------*/
      // produce gnuplot plot file
      if (opt.providegnuplotplot)
      {
        std::string gnuplotfile=opt.gnuplotfile+".gpt";
        if (!opt.overwrite)
        {
          datrw::abort_if_exists(gnuplotfile);
        }
        std::ofstream ofs(gnuplotfile.c_str());
        libtime::TRelativeTime binsize(opt.binsize);
        CompletenessBins cs(earliesttime,latesttime,binsize);
        gnuplotplot(ofs, opt.gnuplotfile+".ps", cs, vecofgaps);
      } // if (opt.providegnuplotplot)

      /*----------------------------------------------------------------------*/
      // write time series with gap information
      if (opt.writegapseries || opt.writecompleteness)
      {
        TFXX_debug(opt.debug, "main",
                   "write gap and/or completeness series");
        std::ofstream* Pgofs;
        std::ofstream* Pcofs;
        datrw::oanystream* Pgos;
        datrw::oanystream* Pcos;
        if (opt.writegapseries)
        {
          if (!opt.overwrite)
          {
            datrw::abort_if_exists(opt.gapseriesfile);
          }
          Pgofs=new std::ofstream(opt.gapseriesfile.c_str(),
                               datrw::oanystream::openmode(opt.outputformat));
          Pgos=new datrw::oanystream(*Pgofs, opt.outputformat, opt.debug);
        }
        if (opt.writecompleteness)
        {
          if (!opt.overwrite)
          {
            datrw::abort_if_exists(opt.completenessseriesfile);
          }
          Pcofs=new std::ofstream(opt.completenessseriesfile.c_str(),
                               datrw::oanystream::openmode(opt.outputformat));
          Pcos=new datrw::oanystream(*Pcofs, opt.outputformat, opt.debug);
        }
        libtime::TRelativeTime binsize(opt.binsize);
        TFXX_debug(opt.debug, "main",
                   "earliesttime: " << earliesttime.timestring()
                   << " latesttime: " << latesttime.timestring());
        CompletenessBins completenessbins(earliesttime,latesttime,binsize);
        for (Tvecofgaps::const_iterator S=vecofgaps.begin();
             S!=vecofgaps.end(); ++S)
        {
          GapSeries gapseries=seriesofmissingsamples(*S, completenessbins,
                                                     opt.debug);
          if (opt.writegapseries)
          {
            *Pgos << gapseries.header.wid2();
            *Pgos << gapseries.gapseries;
          }
          if (opt.writecompleteness)
          {
            CompletenessSeries cseries=completeness(gapseries, opt.debug);
            *Pcos << cseries.header.wid2();
            *Pcos << cseries.completeness;
            TFXX_debug(opt.debug, "main",
                       "just wrote completeness time series with "
                       << cseries.completeness.size()
                       << " samples");
          }
        }
        if (opt.writegapseries)
        {
          delete Pgos;
          delete Pgofs;
        }
        if (opt.writecompleteness)
        {
          TFXX_debug(opt.debug, "main",
                     "close completeness datstream");
          delete Pcos;
          TFXX_debug(opt.debug, "main",
                     "close completeness file stream");
          delete Pcofs;
        }
      } // if (opt.writegapseries || opt.writecompleteness)
    } // if (analyzecompleteness)

  } // if (doGAPanalysis)
  else
  {
    /*======================================================================*/
    /* 4. copy data
     *
     * In this last section the program cycles through all sets of continuous
     * data that are defined in the global variable
     *
     *   Tvecoftraces trace;
     *
     * Each element in the global variable trace defines a trace in the output
     * file of the program. For each of these traces a time series is compiled
     * from the input files as defined by the copy operations specified by the
     * DataBlock elements for each trace.
     *
     * Data files are only read and written within this last section of the
     * program.
     */
    if (opt.verbose)
    {
      cout << endl;
      cout << "step 4: copy data" << endl;
      if (opt.integerdata)
      { cout << "extract integer samples" << endl; }
      if (opt.GSEoutput)
      { cout << "write SFF with GSE compatible scaling (ampfac)" << endl; }
    }

    // open output file
    if (opt.verbose) { cout << "open output file " << outfile << endl; }
    // check if output file exists and open
    if (!opt.overwrite) { datrw::abort_if_exists(outfile); }
    std::ofstream ofs(outfile.c_str(),
                      datrw::oanystream::openmode(opt.outputformat));
    datrw::oanystream os(ofs, opt.outputformat, opt.debug);

    filefree.append("index file(s):");
    infile=infiles.begin(); 
    while( infile!=infiles.end()) { filefree.append(*infile); ++infile; }
    if ((!opt.nofreeblock) && os.handlesfilefree()) { os << filefree; } 
    if (opt.debug) { cout << "DEBUG: file FREE block:" << endl << filefree; }

    // creater the sequential trace reader
    datrw::sequentialtracereader is(opt.debug);
    // cycle through traces
    for (unsigned int itrace=0; itrace<trace.size(); ++itrace)
    {
      if (opt.verbose) { cout << "compile trace #" << itrace+1 << ":" << endl; }
      // prepare dataspace
      Tvecofblocks &thetrace(trace[itrace]);
      int totalsamples=0;
      for (unsigned int iblock=0; iblock<thetrace.size(); ++iblock)
      {
        totalsamples+=thetrace[iblock].nsamples;
      }
      if (opt.debug) 
      { cout << "  extract " << totalsamples << " samples" << endl; }
      sff::FREE tracefree;
      sff::INFO traceinfo;
      bool hasinfo=false;
      sff::WID2 tracewid2;
      // prepare two flavours of time series containers
      Series tracedata;
      // set the right one to the appropriate size
      if (opt.integerdata)
      { 
        TFXX_debug(opt.debug, "anyextract",
                   "allocate integer data series");
        tracedata.is()=Series::Tis(totalsamples); 
        tracefree.append("copy integer data");
      }
      else if (opt.floatdata)
      { 
        TFXX_debug(opt.debug, "anyextract",
                   "allocate float data series");
        tracedata.fs()=Series::Tfs(totalsamples); 
        tracefree.append("copy float (single precision) data");
      }
      else
      {
        TFXX_debug(opt.debug, "anyextract",
                   "allocate double data series");
        tracedata.ds()=Series::Tds(totalsamples);
        tracefree.append("copy double precision data");
      }
      TFXX_debug(opt.debug, "anyextract",
                 "output series container is allocated");

      // prepare WID2 line
      tracewid2=collection[thetrace[0].ientry].wid2;
      tracewid2.nsamples=totalsamples;
      tracewid2.date=sff::wid2isample(tracewid2,thetrace[0].ifirst);

      tracefree.append("read data from:");
      for (unsigned int iblock=0; iblock<thetrace.size(); ++iblock)
      {
        DataBlock &theblock(thetrace[iblock]);
        IndexEntry &theentry(collection[theblock.ientry]);
        std::ostringstream freeline;
        freeline << theentry.filename << 
          " trace #" << theentry.itrace <<
          " format: " << theentry.dataformat;
        tracefree.append(freeline.str());

        // open data file and read samples
        if (opt.verbose) 
        { 
          cout << "  open input file " << theentry.filename 
            << " (mode: " << theentry.dataformat << ", "
            << "trace: " << theentry.itrace << ")" 
            << endl; 
        }
        is.select(theentry.filename, theentry.itrace,
                           theentry.dataformat);
        Series inseries;
        TFXX_assert(is.good(), "ERROR: input is not good!");
        if (opt.integerdata)
        {
          TFXX_assert(is.providesi(),
                      "ERROR: integer data requested\n"
                      "but not provided by input stream");
          is >> inseries.is(); 
        }
        else if (opt.floatdata)
        { 
          TFXX_assert(is.providesf(),
                      "ERROR: float data requested\n"
                      "but not provided by input stream");
          is >> inseries.fs(); 
        }
        else
        {
          TFXX_assert(is.providesd(),
                      "ERROR: double precision data requested\n"
                      "but not provided by input stream");
          is >> inseries.ds(); 
        }
        TFXX_assert(((theblock.ifirst+theblock.nsamples-1)
                     <= int(inseries.last())),
                    "ERROR: too few samples in input data!");
        TFXX_assert((theblock.ifirst >= int(inseries.first())),
                    "ERROR: range error in input data!");
        TFXX_assert(((theblock.ofirst+theblock.nsamples-1)
                     <= int(tracedata.last())),
                    "ERROR: too few samples reserved in output data!");
        TFXX_assert((theblock.ofirst >= int(tracedata.first())),
                    "ERROR: range error in output data!");
        for (int isample=0; isample<theblock.nsamples; ++isample)
        {
          int ioutsample=isample+theblock.ofirst;
          int iinsample=isample+theblock.ifirst;
          if (opt.rangecheck)
          {
            TFXX_assert(((ioutsample>=tracedata.first())
                         &&(ioutsample<=tracedata.last())
                         &&(iinsample>=inseries.first())
                         &&(iinsample<=inseries.last())),
                        "ERROR: series index out of range!");
          }
          if (opt.integerdata)
          { tracedata.is()(ioutsample)=inseries.is()(iinsample); }
          else if (opt.floatdata)
          { tracedata.fs()(ioutsample)=inseries.fs()(iinsample); }
          else
          { tracedata.ds()(ioutsample)=inseries.ds()(iinsample); }
        }
        // extract INFO line if first file has INFO
        if ((iblock == 0) && (is.hasinfo()))
        { is >> traceinfo; hasinfo=true; }
      }
      // adjust calib and calper if requested
      if (opt.setcalib) { tracewid2.calib=opt.newcalib; }
      if (opt.setcalper) { tracewid2.calper=opt.newcalper; }
      // finished reading, write now
      if (opt.verbose) { cout << "  write output trace" << endl; }
      os << tracewid2; 
      if ((!opt.nofreeblock) && os.handlestracefree()) { os << tracefree; }
      if (hasinfo && os.handlesinfo()) { os << traceinfo; }
      if (opt.integerdata)
      { 
        os << tracedata.is(); 
      }
      else if (opt.floatdata)
      { 
        os << tracedata.fs(); 
      }
      else
      { 
        os << tracedata.ds(); 
      }
    }
  } // if not (doGAPanalysis)
}

/* ----- END OF anyextract.cc ----- */
