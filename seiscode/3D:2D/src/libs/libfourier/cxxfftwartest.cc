/*! \file cxxfftwartest.cc
 * \brief test fftw3 array engine
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 16/05/2011
 * 
 * test fftw3 array engine
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * libfourier is free software; you can redistribute it and/or modify
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
 * 
 * REVISIONS and CHANGES 
 *  - 16/05/2011   V1.0   Thomas Forbriger
 *  - 27/05/2011   V1.1   test copy constructor and default constructor
 *  - 08/09/2011   V1.2   avoid deprecated ?anystream constructors
 * 
 * ============================================================================
 */
#define CXXFFTWARTEST_VERSION \
  "CXXFFTWARTEST   V1.2   test fftw3 array engine"

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
  
// use operators defined for series
#include <aff/seriesoperators.h>
// use operators defined for array
#include <aff/arrayoperators.h>

// use fft modules
#include <fourier/fftwaff.h>
#include <fourier/fftwaffar.h>

/*----------------------------------------------------------------------*/

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

// a struct to store values of command line arguments
struct Options {
  // be verbose
  bool verbose, verbsize, exchangeengines;
  // file format to be used
  std::string fileformat;
  // overwrite output files in case they already exist
  bool overwrite;
}; // struct Options

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
  // the waveform header (sampling rate, etc)
  sff::WID2 wid2;
  // coordinates of receiver
  sff::INFO info;
}; // struct Trace

// several traces (e.g. the traces of one file) are collected in a vector
// of the STL (standard template library)
typedef std::vector<Trace> Tvecoftraces;

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    CXXFFTWARTEST_VERSION "\n"
    "usage: cxxfftwartest input output [-s] [-v] [-o] [-x] [-type f]" "\n"
    "   or: cxxfftwartest --help|-h" "\n"
    "   or: cxxfftwartest --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "input        input file name\n"
    "output       output file name\n"
    "\n"
    "-v           verbose mode\n"
    "-s           report container sizes\n"
    "-o           overwrite output file\n"
    "-x           reverse order of DRFFTWAFF and DRFFTWAFFArrayEngine\n"
    "             (see below)\n"
    "-type f      file format\n"
    "\n"
    "Fourier coefficients for input time series are obtained from\n"
    "DRFFTWAFFArrayEngine. Then time series are constructed from these\n"
    "coefficients by application of DRFFTWAFF. If the -x option is\n"
    "selected, both transformation tools are exchanged in the\n"
    "application order.\n"
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
    // 3: overwrite mode
    {"o",arg_no,"-"},
    // 4: file type
    {"type",arg_no,"sff"},
    // 5: report container sizes
    {"s",arg_no,"-"},
    // 6: exchange engines
    {"x",arg_no,"-"},
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
    datrw::supported_data_types(cerr);
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

  Options opt;

  opt.verbose=cmdline.optset(2);
  opt.overwrite=cmdline.optset(3);
  opt.fileformat=cmdline.string_arg(4);
  opt.verbsize=cmdline.optset(5);
  opt.exchangeengines=cmdline.optset(6);

  // report program version if in verbose mode
  if (opt.verbose)
  { cout << CXXFFTWARTEST_VERSION << endl; }

  // extract commandline arguments
  // here the rest of the command line is parsed; i.e. the names of input
  // files together with file specific options
  TFXX_assert(cmdline.extra(), "missing input file");
  std::string infilename=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing output file");
  std::string outfilename=cmdline.next();

  /*----------------------------------------------------------------------*/

  // here we read the input file
  // ---------------------------

  // open input file
  // ---------------
  if (opt.verbose) { cout << "open input file " << infilename << endl; }
  // open the input file with appropriate input mode
  std::ios_base::openmode iopenmode
    =datrw::ianystream::openmode(opt.fileformat);
  // open file stream
  std::ifstream ifs(infilename.c_str(), iopenmode);
  // open seismic time series stream
  datrw::ianystream is(ifs, opt.fileformat);

  // read file specific header information
  sff::SRCE srce;
  is >> srce;

  /*----------------------------------------------------------------------*/

  // read all traces from this file
  // ------------------------------

  Tvecoftraces vecoftraces;
  // count traces
  unsigned int itrace=0;

  unsigned int nsamples=0;

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

    nsamples=nsamples > trace.series.size() ? nsamples : trace.series.size();

    // trace data is read; add to collection
    vecoftraces.push_back(trace);

  } // while (is.good())

  /*----------------------------------------------------------------------*/

  unsigned int ntraces=vecoftraces.size();

  /*----------------------------------------------------------------------*/

  if (opt.verbose)
  {
    cout << "input files are read..." << endl;
  }

  if (opt.verbose)
  {
    cout << "number of traces: " << ntraces << endl;
    cout << "number of samples in largest trace: " << nsamples << endl;
  }

  /*----------------------------------------------------------------------*/

  if (opt.exchangeengines)
  {
    // prepare array engine
    if (opt.verbose) { cout << "create FFT array engine" << endl; }
    fourier::fft::DRFFTWAFFArrayEngine primaryengine;
    primaryengine=fourier::fft::DRFFTWAFFArrayEngine(ntraces, nsamples);
    fourier::fft::DRFFTWAFFArrayEngine engine(primaryengine);

    if (opt.verbose) 
    {
      cout << "fill FFT array engine with Fourier coefficients" << endl; 
    }
    engine.spectrum()=0;

    fourier::fft::DRFFTWAFF fft;
    fft.size(nsamples);

    for (int i=0; i<engine.nseries(); ++i)
    {
      fourier::fft::DRFFTWAFF::Tspectrum
        spectrum=fft(vecoftraces[i].series,vecoftraces[i].wid2.dt);
      engine.spectrum(i).copyin(spectrum);
    }

    if (opt.verbose) { cout << "execute FFT array engine" << endl; }
    engine.c2r();

    if (opt.verbose) 
    {
      cout << "read time series samples from FFT array engine" << endl; 
    }
    for (int i=0; i<engine.nseries(); ++i)
    {
      Trace& trace=vecoftraces[i];
      Tseries& series=trace.series;
      series.copyin(engine.series(i)*engine.scale_series(trace.wid2.dt));
    }
  }
  else // if (opt.exchangeengines)
  {
    // prepare array engine
    if (opt.verbose) { cout << "create FFT array engine" << endl; }
    fourier::fft::DRFFTWAFFArrayEngine engine(ntraces, nsamples);

    if (opt.verbose) { cout << "fill FFT array engine" << endl; }
    engine.series()=0;
    // fill sample array
    for (int i=0; i<engine.nseries(); ++i)
    {
      engine.series(i).copyin(vecoftraces[i].series);
      if (opt.verbsize)
      {
        cout << "engine series #" << i << " has " << engine.series(i).size()
          << " samples" << endl;
      }
    }

    if (opt.verbose) { cout << "execute FFT array engine" << endl; }
    engine.r2c();

    if (opt.verbose) 
    {
      cout << "read coefficients and transform to time domain" << endl; 
    }
    fourier::fft::DRFFTWAFF fft;
    fft.size(nsamples);
    for (int i=0; i<engine.nseries(); ++i)
    {
      Trace& trace=vecoftraces[i];
      Tseries& series=trace.series;
      fourier::fft::DRFFTWAFFArrayEngine::TAspectrum coeff
        =engine.spectrum(i)*engine.scale_spectrum(trace.wid2.dt);
      fourier::fft::DRFFTWAFF::Tspectrum spectrum(coeff.size());
      spectrum.copyin(coeff);
      series=fft(spectrum,trace.wid2.dt);
      if (opt.verbsize)
      {
        cout << "after return from engine for trace #" << i << endl;
        cout << "  size of engine spectrum: " << engine.spectrum(i).size() 
          << endl;
        cout << "  size of copy of spectrum: " << spectrum.size()
          << endl;
        cout << "  returned series size: " << series.size() << endl;
      }
    }
  } // if (opt.exchangeengines)

  /*----------------------------------------------------------------------*/

  // write back transformed traces
  // -----------------------------

  if (opt.verbose) { cout << "write transformed traces" << endl; }
  if (opt.verbose) 
  {
    cout << "write file " << outfilename << endl;
  }
    
  // check if output file exists and open
  if (!opt.overwrite)
  {
    std::ifstream file(outfilename.c_str(),std::ios_base::in);
    TFXX_assert((!file.good()),"ERROR: output file exists!");
  }

  // create output stream of desired file format
  std::ios_base::openmode oopenmode
    =datrw::oanystream::openmode(opt.fileformat);
  std::ofstream ofs(outfilename.c_str(), oopenmode);
  datrw::oanystream os(ofs, opt.fileformat);

  // write srce data and file free block if supported
  if (os.handlessrce()) { os << srce; }

  // write trace data
  for (unsigned int j=0; j<vecoftraces.size(); ++j)
  {
    if (opt.verbose)
    {
      cout << "  write trace #" << j+1 << endl;
    }
    Trace& currenttrace=vecoftraces[j];

    os << currenttrace.wid2;
    if (os.handlesinfo()) { os << currenttrace.info; }
    os << currenttrace.series;
  } // for (unsigned int j=0; j<currentfile.traces.size(); ++j)
}

/* ----- END OF cxxfftwartest.cc ----- */
