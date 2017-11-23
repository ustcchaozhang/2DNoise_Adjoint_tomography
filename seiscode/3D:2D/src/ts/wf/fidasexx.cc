/*! \file fidasexx.cc
 * \brief FIt DAtaSEts - a C++ successor of fidase
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 11/03/2011
 * 
 * FIt DAtaSEts - a C++ successor of fidase
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * FIDASEXX is free software; you can redistribute it and/or modify
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
 *  - 11/03/2011   V1.0   Thomas Forbriger
 *  - 16/03/2011   V1.1   first operational version
 *  - 08/09/2011   V1.2   support format modifiers
 * 
 * ============================================================================
 */
#define FIDASEXX_VERSION \
  "FIDASEXX   V1.2   FIt DAtaSEts - a C++ successor of fidase"

// use the input/output facilities from the standard library
#include <iostream>
#include <fstream>

// use output string streams to create strings
#include <sstream>

// use the vector container from the STL (standard template library) to store
// collections of traces and files
#include <vector>

// use command line argument reader from libtfxx
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>

// use the error handling and debugging facilities from libtfxx
#include <tfxx/error.h>
#include <tfxx/misc.h>

// use the facility in libtfxx to handle lists of trace number ranges
#include <tfxx/rangestring.h>
#include <tfxx/rangelist.h>

// use series container from libaff
#include <aff/series.h>

// use data file reading and writing facility from libdatrwxx
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>

// use solver for system of linear equations provided through liblinearxx
#include <linearxx/matrix.h>
#include <linearxx/lapackxx.h>
#include <linearxx/operators.h>

// use value extraction provided by libaff
#include <aff/functions/rms.h>
#include <aff/functions/max.h>
#include <aff/functions/min.h>
// use operators defined for series
#include <aff/seriesoperators.h>

// use plotting facility
#include <pgplotxx/affpgplot.h>

/*======================================================================*/

// using directives; allows us to use cout, cerr and endl without addressing
// them in their namespace
using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

// a struct to store values of command line arguments
struct Options {
  // be verbose
  bool verbose;
  // produce debugging output
  bool debug;
  // file format to be used
  std::string fileformat;
  // PGPLOT device
  std::string plotdevice;
  // create graphical display
  bool doplot;
  // overwrite output files in case they already exist
  bool overwrite;
  // write scaled files
  bool writescaled;
  // prefix for scaled file names
  std::string fileprefix;
  // offset range to be used
  pgplot::Trange rrange;
}; // struct Options

/*----------------------------------------------------------------------*/

// further arguments may be appended to each file name on the command line
// here we prepare the definition of their keys

// key to indicate the selection of traces
static const char tracekey[]="t";

// key to indicate the selection of a specific input format
static const char formatkey[]="f";

// define commandline argument modifier keys
static const char* cmdlinekeys[]={tracekey, formatkey, 0};

/*----------------------------------------------------------------------*/

// type of actual time series samples
typedef double Tvalue;

// type of time series
typedef aff::Series<Tvalue> Tseries;

/*----------------------------------------------------------------------*/

// a struct to store the contents of one data trace
struct Trace {
  // the time series
  Tseries series;
  // number of trace in file
  int tracenumber;
  // is this trace selected to be used
  bool selected;
  // the waveform header (sampling rate, etc)
  sff::WID2 wid2;
  // coordinates of receiver
  sff::INFO info;
  // trace specific comments
  sff::FREE free;
}; // struct Trace

// several traces (e.g. the traces of one file) are collected in a vector
// of the STL (standard template library)
typedef std::vector<Trace> Tvecoftraces;

/*----------------------------------------------------------------------*/

// a struct to store the contents of a complete data file
struct File {
  // the name of the file
  std::string filename;
  // the source location
  sff::SRCE srce;
  // file specific comments
  sff::FREE free;
  // a vector of traces
  Tvecoftraces traces;
  // keep rms values of scale traces (for plotting)
  pgplot::pgaff::Tseries rms;
  // keep offset values (for plotting)
  pgplot::pgaff::Tseries offset;
}; // struct File

// several files are collected in a vector of the STL (standard template
// library)
typedef std::vector<File> Tvecoffiles;

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    FIDASEXX_VERSION "\n"
    "usage: fidasexx [-v] [-DEBUG] [-type f] [-plot[=dev]]" "\n"
    "                [-overwrite] [-write[=prefix]] [-rmin r] [-rmax r]" "\n"
    "                file [t:l] [f:t] file [t:l] [f:t] [file [t:l] [f:t]...]" "\n"
    "   or: fidasexx --help|-h" "\n"
    "   or: fidasexx --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v           be verbose\n"
    "-DEBUG       produce debug output\n"
    "-type f      read and write files of file type \"f\"\n"
    "-plot[=dev]  produce graphical display on PGPLOT device \"dev\"\n"
    "             the specification of \"dev\" is optional\n"
    "-overwrite   overwrite output files in case they already exist\n"
    "-write[=prefix]\n"
    "             write scaled files; prepend \"prefix\" to file names\n"
    "             to distinguish scaled files and original files\n"
    "-rmin r      use only traces with offset equal or larger than \"r\"\n"
    "-rmax r      use only traces with offset equal or less than \"r\"\n"
    "\n"
    "The program expects at least two input files containing seismic data.\n"
    "Each of the input files is understood to contain data from one single\n"
    "and unique shot. File specific parameters:\n"
    "\n"
    "t:l          use only traces from list \"l\" for evaluation\n"
    "             \"l\" may be somtehing like \"2-4,5,7,11\"\n"
    "             all traces are read, tapered and written to the\n"
    "             output; but only traces in range indicated by\n"
    "             \"l\" are used to infer the scaling factor\n"
    "f:t          file has data file type \"t\" instead of the\n"
    "             global format specified by the argument to\n"
    "             \"-type\"\n"
    "\n"
    "The program is used to scale data from independent shots to make the\n"
    "apparent shot amplitude comparable. fidasexx uses a different strategy.\n"
    "While fidase tries to find optimal scaling factors such that the amplitude\n"
    "decay with offset is smooth as measured by second differences of rms\n"
    "values, fidasexx fits the rms values to an explicit amplitude decay with\n"
    "offset. The underlying model is that rms-values depend on offset r\n"
    "like\n"
    "      rms(r) = A0 * r**(K),\n"
    "where r**(K) means \"r to the power of K\".\n"
    "\n"
    "If d_(j,k) is the rms value of trace k in file j and r_(j,k) are the\n"
    "offset of trace k in file j, we search for factors f_j and K such that\n"
    "the residuals r_(j,k) = log d_(j,k) - (log f_j + K * log r_(j,k))\n"
    "are minimized in a least squares sense.\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: print detailed online help
    {"xhelp",arg_no,"-"},
    // 2: verbose mode
    {"v",arg_no,"-"},
    // 3: debug mode
    {"DEBUG",arg_no,"-"},
    // 4: default file format for input and output
    {"type",arg_yes,"sff"},
    // 5: create graphical display
    {"plot",arg_opt,"/xserve"},
    // 6: overwrite output
    {"overwrite",arg_no,"-"},
    // 7: write scale files
    {"write",arg_opt,"scaled_"},
    // 8: write scale files
    {"rmin",arg_yes,"0."},
    // 9: write scale files
    {"rmax",arg_yes,"1.e15"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  // by creating the instance cmdline of type Commandline (which is a member
  // of namespace tfxx::cmdline) the command line is parsed; all command line
  // options defined in the C-array options are extracted and can later be
  // accessed through the member functions of cmdline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    datrw::supported_data_types(cerr);
    pgplot::basic_device::ldev();
    exit(0);
  }

  // help on file format details requested? 
  if (cmdline.optset(1))
  {
    cerr << usage_text << endl;
    cerr << endl;
    datrw::online_help(cerr);
    exit(0);
  }

  // extract commandline options
  Options opt;
  opt.verbose=cmdline.optset(2);
  opt.debug=cmdline.optset(3);
  opt.fileformat=cmdline.string_arg(4);
  opt.doplot=cmdline.optset(5);
  opt.plotdevice=cmdline.string_arg(5);
  opt.overwrite=cmdline.optset(6);
  opt.writescaled=cmdline.optset(7);
  opt.fileprefix=cmdline.string_arg(7);
  opt.rrange.min=cmdline.float_arg(8);
  opt.rrange.max=cmdline.float_arg(9);

  // report program version if in verbose mode
  if (opt.verbose)
  { cout << FIDASEXX_VERSION << endl; }

  // extract commandline arguments
  // here the rest of the command line is parsed; i.e. the names of input
  // files together with file specific options
  TFXX_assert(cmdline.extra(), "missing input file");
  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);

  /*----------------------------------------------------------------------*/

  // here we read the input files
  // ----------------------------
  
  // create a collection of files
  Tvecoffiles files;

  // now cycle through the list of input file arguments
  tfxx::cmdline::Tparsed::const_iterator infilearg=arguments.begin();
  while (infilearg != arguments.end())
  {
    // create a place to store contents of file
    File infile;
    // remember name of input file
    infile.filename=infilearg->name;

    // open input file
    // ---------------
    if (opt.verbose) { cout << "open input file " << infile.filename << endl; }
    // select file format
    std::string inputformat=opt.fileformat;
    // change file format if a specific format was given on the command line
    // for this file
    if (infilearg->haskey(formatkey)) 
    {
      inputformat=infilearg->value(formatkey);
    }
    // open the input file with appropriate input mode
    std::ios_base::openmode iopenmode
      =datrw::ianystream::openmode(inputformat);
    // open file stream
    std::ifstream ifs(infile.filename.c_str(), iopenmode);
    // open seismic time series stream
    datrw::ianystream is(ifs, inputformat);

    // read file specific header information
    is >> infile.srce;
    is >> infile.free;

    /*----------------------------------------------------------------------*/

    // read all traces from this file
    // ------------------------------
    
    // check whether there is a selection of traces defined
    bool doselect=infilearg->haskey(tracekey);

    // setup list of trace number ranges
    typedef tfxx::RangeList<int> Trangelist;
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infilearg->value(tracekey));

    // count traces
    int itrace=0;

    // read next trace, while input stream is good
    while (is.good())
    {
      // count traces
      ++itrace;

      // create a place to store trace data
      Trace trace;

      // remember number of trace
      trace.tracenumber=itrace;

      // read trace data
      is >> trace.series;
      is >> trace.wid2;
      is >> trace.info;
      is >> trace.free;

      // we read all traces, but only selected traces are used for fitting
      trace.selected=(((!doselect) || traceranges.contains(itrace))
                      && opt.rrange.contains(sff::offset(infile.srce,
                                                         trace.info)));

      // trace data is read; add to collection
      infile.traces.push_back(trace);

    } // while (is.good())

    /*----------------------------------------------------------------------*/

    // file data is read; append this file to the collection of files
    files.push_back(infile);

    // next input file
    ++infilearg;

  } // while (infilearg != arguments.end())
  // all input time series files are read

  /*----------------------------------------------------------------------*/

  if (opt.verbose)
  {
    cout << "input files are read..." << endl;
    cout << "next: count traces" << endl;
  }

  /*
   * All input files are read. Now we have to know the number of traces to be
   * used in the fitting procedure as well as the number of files for which
   * scaling factors have to be inferred. In two nested loops we cycle through
   * all files and traces and count them.
   */

  // number of files
  int nfiles=0;
  // total number of traces
  int ntraces=0;

  Tvecoffiles::const_iterator F=files.begin();
  while (F!=files.end())
  {
    // count file
    ++nfiles;

    // count number of selected trace in this file
    int ninthisfile=0;

    Tvecoftraces::const_iterator T=F->traces.begin();
    while (T != F->traces.end())
    {
      // count trace if selected
      if (T->selected) { ++ntraces; ++ninthisfile; }
      // next trace
      ++T;
    } // while (T != F->traces.end())

    // at least one trace per file has to be used
    TFXX_assert(ninthisfile>0,
                "ERROR: at least one trace per file has to be selected");

    // next file
    ++F;
  } // while (F!=files.end())

  // check whether we have a reasonable number of files
  TFXX_assert(nfiles>1, "ERROR: we require at least two files!");

  /*----------------------------------------------------------------------*/

  if (opt.verbose)
  {
    cout << "number of files: " << nfiles << endl;
    cout << "total number of traces to be used: " << ntraces << endl;
    cout << "next: set up system of linear equations" << endl;
  }

  // at this point a taper should be applied to the traces
  // this is not yet implemented

  /*----------------------------------------------------------------------*/

  /*
   * Now the dimensions of the least-squares problem are defined. In the next
   * step we set up the system of linear equations which have to be solved for
   * the solution of the least-squares problem.
   *
   * Data are the logarithms of the rms values of traces. We have as many data
   * as selected traces.
   *
   * Model parameters are the scaling factors for each file (offset to the
   * logarithmic rms values) and one commen decay exponent (factor to the
   * logarithmic offset). We have one more model parameter than files.
   *
   * If d_(j,k) is the rms value of trace k in file j and r_(j,k) are the
   * offset of trace k in file j, we search for factors f_j and K such that
   * the residuals r_(j,k) = log d_(j,k) - (log f_j + K * log r_(j,k))
   * are minimized in a least squares sense.
   */

  // values to be prepared
  // ---------------------

  // vector of model parameters
  linear::TDmatrix m(nfiles+1);
  m=0.;

  // vector of data values (logarithm of rms of time series)
  linear::TDmatrix d(ntraces);
  d=0.;

  // vector of offset values 
  linear::TDmatrix r(ntraces);
  r=0.;

  // remember file index for given trace
  typedef std::vector<int> Tvecofint;
  Tvecofint fileindex(ntraces);

  // matrix of partial derivatives
  linear::TDmatrix D(ntraces, nfiles+1);
  D=0.;

  // count traces separately
  // initialize with first index value for vector d
  int itrace=d.first(0);

  // fill d, r, fileindex, and D with appropriate values
  // ---------------------------------------------------
  for (unsigned int i=0; i<files.size(); ++i)
  {
    // reference to current file (just for convenience)
    const File& currentfile=files[i];
    // cycle through all traces in this file
    for (unsigned int j=0; j<currentfile.traces.size(); ++j)
    {
      // reference to current trace (just for convenience)
      const Trace& currenttrace=currentfile.traces[j];
      // operate on trace, if trace is selected
      if (currenttrace.selected)
      {
        // just check the index to make program bullet proof
        TFXX_assert(itrace<=d.last(0),
                    "ERROR: illegal vector index - inconsistency in program");

        d(itrace)=std::log(aff::func::rms(currenttrace.series));
        r(itrace)=sff::offset(currentfile.srce,currenttrace.info);
        // check that coordinates are reasonable
        TFXX_assert(r(itrace)>1.e-6,
                    "ERROR: offset is unreasonably small (possibly zero)");
        D(itrace,i+D.first(1))=1.;
        D(itrace,D.last(1))=std::log(r(itrace));
        fileindex[itrace-d.first(0)]=i;

        // we filled this value; proceeed to next one
        ++itrace;
      } // if (currenttrace.selected)
    } // for (unsigned int j=0; j<currentfile.traces.size(); ++j)
  } // for (int i=0; i<files.size(); ++i)

  if (opt.verbose)
  {
    cout << "system of linear equations is ready" << endl;
  }

  // setup system of linear equations
  // --------------------------------
  linear::TDmatrix M=linear::op::dotNxM(linear::op::transposeNxM(D),D);
  linear::TDmatrix R=linear::op::dotNxM(linear::op::transposeNxM(D),d);

  // solve system of linear equations
  // --------------------------------
  m=linear::lapack::dposv(M,R);

  // calculate scaling factors
  Tseries factor(0,nfiles-1);
  Tvalue firstamp=std::exp(m(m.first(0)));
  for (int i=0; i<=factor.last(); ++i)
  {
    factor(i)=firstamp/std::exp(m(m.first(0)+i));
  }
  Tvalue attenuation=m(m.last(0));

  if (opt.verbose)
  {
    cout << "system of linear equations is solved" << endl;
    cout << "power-law attentuation exponent is " << attenuation << endl;
  }

  /*----------------------------------------------------------------------*/

  // extract ranges for plot
  pgplot::Trange offsetrange(0.,0.), rmsrange(0.,0.);

  // scale files
  // -----------
  for (unsigned int i=0; i<files.size(); ++i)
  {
    File& currentfile=files[i];
    currentfile.rms=pgplot::pgaff::Tseries(0,currentfile.traces.size()-1);
    currentfile.offset=pgplot::pgaff::Tseries(0,currentfile.traces.size()-1);
    if (opt.verbose)
    {
      cout << "scale file #" <<i+1 << " "
        << currentfile.filename << " by " << factor(i) << endl;
    }
    for (unsigned int j=0; j<currentfile.traces.size(); ++j)
    {
      currentfile.traces[j].series *= factor(i);
      currentfile.offset(j)=std::log10(sff::offset(currentfile.srce,
                                        currentfile.traces[j].info));
      currentfile.rms(j)=std::log10(aff::func::rms(currentfile.traces[j].series));
    } // for (int j=0; j<=traces.size(); ++j)

    // check ranges for subsequent plot
    if (i==0)
    {
      offsetrange=pgplot::pgaff::series_value_range(currentfile.offset);
      rmsrange=pgplot::pgaff::series_value_range(currentfile.rms);
    }
    else
    {
      offsetrange.extend(pgplot::pgaff::series_value_range(currentfile.offset));
      rmsrange.extend(pgplot::pgaff::series_value_range(currentfile.rms));
    }
  } // for (int i=0; i<=files.size(); ++i)

  /*----------------------------------------------------------------------*/

  // plot
  // ----

  if (opt.doplot)
  {
    pgplot::device dev(opt.plotdevice.c_str());  
    dev.env(pgplot::Trect(offsetrange,rmsrange), 0, 30);
    dev.lab("offset / m","rms value","rms amplitude of scaled traces");
    for (unsigned int i=0; i<files.size(); ++i)
    {
      File& currentfile=files[i];
      pgplot::pgaff::pt(dev, currentfile.offset, currentfile.rms, 2+i);
    } // for (int i=0; i<=files.size(); ++i)
    const int npts=500;
    pgplot::pgaff::Tseries x(0,npts-1);
    pgplot::pgaff::Tseries y(0,npts-1);
    for (int i=0; i<npts; ++i)
    {
      x(i)=offsetrange.min+i*offsetrange.fullrange()/(npts-1);
      y(i)=std::log10(firstamp)+attenuation*x(i);
    }
    pgplot::pgaff::line(dev, x, y);
  } // if (opt.doplot)

  /*----------------------------------------------------------------------*/

  // write scaled files
  // ------------------

  if (opt.writescaled)
  {
    if (opt.verbose) { cout << "write scaled files" << endl; }

    for (unsigned int i=0; i<files.size(); ++i)
    {
      File& currentfile=files[i];
      std::string ofilename=opt.fileprefix+currentfile.filename;
      if (opt.verbose) 
      {
        cout << "write file " << ofilename << endl;
      }

      // create string to comment scaling
      std::ostringstream oss;
      oss << "scaled by factor " << factor(i)
        << "; decay constant is " << attenuation;

      // add information to file free block
      currentfile.free.append(std::string(FIDASEXX_VERSION));
      currentfile.free.append(oss.str());

      // create output stream of desired file format
      std::ios_base::openmode oopenmode
        =datrw::oanystream::openmode(opt.fileformat);
      std::ofstream ofs(ofilename.c_str(), oopenmode);
      datrw::oanystream os(ofs, opt.fileformat, opt.debug);

      // write srce data and file free block if supported
      if (os.handlessrce()) { os << currentfile.srce; }
      if (os.handlesfilefree()) { os << currentfile.free; }

      // write trace data
      for (unsigned int j=0; j<currentfile.traces.size(); ++j)
      {
        if (opt.verbose)
        {
          cout << "  write trace #" << j+1 << endl;
        }
        Trace& currenttrace=currentfile.traces[j];

        // add information to file free block
        currenttrace.free.append(oss.str());

        os << currenttrace.wid2;
        if (os.handlesinfo()) { os << currenttrace.info; }
        if (os.handlesfilefree()) { os << currenttrace.free; }
        os << currenttrace.series;
      } // for (unsigned int j=0; j<currentfile.traces.size(); ++j)
    } // for (int i=0; i<=files.size(); ++i)
  } // if (opt.writescaled)

} // main()

/* ----- END OF fidasexx.cc ----- */
