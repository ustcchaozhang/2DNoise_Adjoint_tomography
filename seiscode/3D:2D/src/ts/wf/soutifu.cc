/*! \file soutifu.cc
 * \brief inversion for source wavelet correction filter
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 06/05/2011
 * 
 * inversion for source wavelet correction filter
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * SOUTIFU is free software; you can redistribute it and/or modify
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
 *  - 06/05/2011   V1.0   Thomas Forbriger
 *  - 08/09/2011   V1.1   support format modifiers
 *  - 30/09/2011   V1.2   support additional time series pairs
 *  - 19/10/2015   V1.3   provide full usage information from libstfinv
 *  - 22/11/2016   V1.4   make proper use of output operators
 * 
 * ============================================================================
 */
#define SOUTIFU_VERSION \
  "SOUTIFU   V1.4   inversion for source wavelet correction filter"

#include <iostream>
#include <fstream>
#include <tfxx/error.h>
#include <tfxx/commandline.h>
#include <tsioxx/inputoperators.h>
#include <tsioxx/outputoperators.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <stfinv/stfinvany.h>

/*----------------------------------------------------------------------*/

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/
// debug helper
#define PAROUT( par ) cout << #par << "=" << par << " ";

/*----------------------------------------------------------------------*/
// function to compare double
// is true if relative residual between a and b is smaller than eps
bool sameineps(const double &a, const double& b, const double& eps=1.e-8)
{
  double reldif=std::abs(a-b);
  return(reldif<=(std::abs(b*eps)));
}

/*----------------------------------------------------------------------*/

struct Options {
  bool verbose, debug, overwrite;
  bool writestf, writeconvolved;
  bool additionalpairs;
  std::string stffilename, convolvedfilename, debuglevel;
  std::string datafileformat, outputfileformat;
  std::string additionalseries, additionaloutput;
}; // struct Options

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SOUTIFU_VERSION "\n"
    "usage: soutifu [-v] [-o] [-wc f] [-ws f] [--type f] [--Type f]\n"
    "               [-add f] [-wa f]\n"
    "               [-DEBUG=level]\n"
    "               parameters data synthetics" "\n"
    "   or: soutifu --help|-h" "\n"
    "   or: soutifu --xhelp | --iohelp | --phelp p" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "Calculate optimized source wavelet correction filter to\n"
    "minimize misfit between recorded data and synthetic waveforms.\n"
    "\n"
    "data         name of file containing recorded data\n"
    "synthetics   name of file containing synthetic data\n"
    "parameters   parameterstring to select STF engine\n"
    "             The string consists of an ID selecting one of the\n"
    "             available engines and a set of options an parameters\n"
    "             to be passed to this engine.\n"
    "\n"
    "--xhelp      print summary of usage instructions for available procedures\n"
    "--iohelp     print detailed usage instructions for file input/output\n"
    "--phelp p    print detailed usage instructions for procedure \"p\"\n"
    "\n"
    "-v           be verbose\n"
    "-DEBUG=level produce debug output at level \"level\"\n"
    "-o           overwrite existing output files\n"
    "--type f     use file format type \"f\" for file input\n"
    "--Type f     use file format type \"f\" for file output\n"
    "             if --Type is not used input and output format will\n"
    "             be the same\n"
    "-wc f        write convolved synthetics to file \"f\"\n"
    "-ws f        write source wavelet correction filter \n"
    "             impulse response to file \"f\"\n"
    "-add f       read additional time series to be convolved from file \"f\"\n"
    "-wa f        write additional time series after convolved to file \"f\"\n"
    "\n"
    "Upon input the consistency of recorded and synthetic data are\n"
    "checked. For coordinates only receiver positions relative to\n"
    "source coordinates are checked, not absolute locations.\n"
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
    // 3: file format
    {"type",arg_yes,"sff"},
    // 4: name of convolved synthetics
    {"wc",arg_yes,"-"},
    // 5: name of source correction filter wavelet file
    {"ws",arg_yes,"-"},
    // 6: present full details regarding engines
    {"xhelp",arg_no,"-"},
    // 7: present full details
    {"DEBUG",arg_yes,"0"},
    // 8: file format
    {"Type",arg_yes,"-"},
    // 9: additional time series to be convolved
    {"add",arg_yes,"-"},
    // 10: output file for additional convolved time series
    {"wa",arg_yes,"-"},
    // 11: provide full details regarding file i/o
    {"iohelp",arg_no,"-"},
    // 12: provide full details regarding file i/o
    {"phelp",arg_yes,"-"},
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
  if (cmdline.optset(0) || cmdline.optset(6) || cmdline.optset(11)
      || cmdline.optset(12))
  {
    cerr << usage_text << endl;
    cerr << endl;
    cerr << help_text << endl;
    datrw::supported_data_types(cerr);
    if (cmdline.optset(11)) 
    { 
      cerr << endl;
      datrw::online_help(cerr); 
    }
    if (cmdline.optset(6)) 
    { 
      cerr << endl;
      stfinv::help(cerr); 
    }
    else
    {
      cerr << help_text << endl;
      stfinv::engines(cerr);
    }
    if (cmdline.optset(12)) 
    {
      cerr << endl;
      stfinv::usage(cmdline.string_arg(12), cerr); 
    }
    exit(0);
  }

  /*----------------------------------------------------------------------*/
  // read options and parameters
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.datafileformat=cmdline.string_arg(3);
  opt.writeconvolved=cmdline.optset(4);
  opt.convolvedfilename=cmdline.string_arg(4);
  opt.writestf=cmdline.optset(5);
  opt.stffilename=cmdline.string_arg(5);
  opt.debug=cmdline.optset(7);
  opt.debuglevel=cmdline.string_arg(7);
  opt.outputfileformat=opt.datafileformat;
  if (cmdline.optset(8)) { opt.outputfileformat=cmdline.string_arg(8); }
  opt.additionalpairs=cmdline.optset(9);
  opt.additionalseries=cmdline.string_arg(9);
  if (cmdline.optset(10))
  {
    TFXX_assert(opt.additionalpairs,
                "ERROR: provide input for additional series");
    opt.additionaloutput=cmdline.string_arg(10);
  }

  TFXX_assert(cmdline.extra(), "missing parameter string");
  std::string parameters=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing recorded data file name");
  std::string datafilename=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing synthetics data file name");
  std::string syntheticsfilename=cmdline.next();

  /*----------------------------------------------------------------------*/
  // go
  // --
 
  // read input data
  // ---------------
  typedef aff::Series<float> Tseries;
  typedef ts::sff::File<Tseries> Tfile;
  Tfile recordeddata;
  Tfile syntheticdata;
  // input for additional series
  Tfile seriesdata;

  std::ios_base::openmode iopenmode
    =datrw::ianystream::openmode(opt.datafileformat);
  if (opt.verbose) { cout << "open input file " << datafilename << endl; }
  {
    std::ifstream ifs(datafilename.c_str(), iopenmode);
    datrw::ianystream is(ifs, opt.datafileformat);
    recordeddata.read(is.idatstream(), opt.verbose);
  }

  if (opt.verbose) { cout << "open input file " << syntheticsfilename << endl; }
  {
    std::ifstream ifs(syntheticsfilename.c_str(), iopenmode);
    datrw::ianystream is(ifs, opt.datafileformat);
    syntheticdata.read(is.idatstream(), opt.verbose);
  }

  if (opt.additionalpairs)
  {
    if (opt.verbose) 
    { cout << "open input file " << opt.additionalseries << endl; }
    {
      std::ifstream ifs(opt.additionalseries.c_str(), iopenmode);
      datrw::ianystream is(ifs, opt.datafileformat);
      seriesdata.read(is.idatstream(), opt.verbose);
    }
  }

  // check input data consistency and prepare output files
  // -----------------------------------------------------
  if (opt.verbose) 
  { 
    cout << "check input data consistency,\n"
      << "prepare output data containers, and\n"
      << "prepare waveform data workspace" << endl; 
  }

  // number of traces per file
  TFXX_assert(recordeddata.size() == syntheticdata.size(),
              "ERROR: inconsitent number of traces");
  const unsigned int ntraces=recordeddata.size();

  // output data
  Tfile convolvedsynthetics;
  Tfile stffile;

  // waveform data workspace
  stfinv::Tvectoroftriples vectoroftriples;
  stfinv::Waveform stfwaveform;

  convolvedsynthetics.fileheader=syntheticdata.fileheader;

  // cycle through all traces
  for (unsigned int itrace=0; itrace<ntraces; ++itrace)
  {
    if (opt.verbose)
    {
      cout << "  check trace #" << itrace+1 << endl;
    }
    // create a reference to the time series with trace header for thsi
    // specific trace as read from file
    const Tfile::Ttracevector::Ttimeseries& rdtseries=recordeddata[itrace];
    const Tfile::Ttracevector::Ttimeseries& sdtseries=syntheticdata[itrace];

    // create a time series to be used in the output of convolved synthetics
    // based on the input synthetics; copy trace header
    Tfile::Ttracevector::Ttimeseries 
      cstseries(Tfile::Ttracevector::Ttimeseries::Tseries(sdtseries.shape()),
                sdtseries.header, sdtseries.traceindex());

    // place reference in a waveform triple
    stfinv::WaveformTriple tracetriple;
    tracetriple.data=rdtseries;
    tracetriple.synthetics=sdtseries;
    tracetriple.convolvedsynthetics=cstseries;
    tracetriple.header.sampling.dt=rdtseries.header.wid2().dt;
    tracetriple.header.sampling.n=rdtseries.header.wid2().nsamples;

    TFXX_assert((rdtseries.header.wid2().nsamples 
                  == sdtseries.header.wid2().nsamples),
                "ERROR: inconsistent trace size");
    TFXX_assert(sameineps(rdtseries.header.wid2().dt,
                            sdtseries.header.wid2().dt),
                "ERROR: inconsistent sampling interval");

    // read out source coordinates
    tracetriple.header.sx=recordeddata.fileheader.srce().cx;
    tracetriple.header.sy=recordeddata.fileheader.srce().cy;
    tracetriple.header.sz=recordeddata.fileheader.srce().cz;

    // read out receiver coordinates
    tracetriple.header.rx=rdtseries.header.info().cx;
    tracetriple.header.ry=rdtseries.header.info().cy;
    tracetriple.header.rz=rdtseries.header.info().cz;

    // check coordinate consistency
    double ddx=rdtseries.header.info().cx-recordeddata.fileheader.srce().cx;
    double ddy=rdtseries.header.info().cy-recordeddata.fileheader.srce().cy;
    double ddz=rdtseries.header.info().cz-recordeddata.fileheader.srce().cz;
    double sdx=sdtseries.header.info().cx-syntheticdata.fileheader.srce().cx;
    double sdy=sdtseries.header.info().cy-syntheticdata.fileheader.srce().cy;
    double sdz=sdtseries.header.info().cz-syntheticdata.fileheader.srce().cz;
    if (opt.debug)
    if (!(sameineps(ddx,sdx) && sameineps(ddy,sdy) &&
                 sameineps(ddz,sdz)))
    {
      cout << "NOTICE: ";
      PAROUT(ddx);
      PAROUT(sdx);
      PAROUT(ddy);
      PAROUT(sdy);
      PAROUT(ddz);
      PAROUT(sdz);
      PAROUT(sameineps(ddx,sdx));
      PAROUT(sameineps(ddy,sdy));
      PAROUT(sameineps(ddz,sdz));
      cout << endl;
    }
    TFXX_assert((sameineps(ddx,sdx) && sameineps(ddy,sdy) &&
                 sameineps(ddz,sdz)),
                "ERROR: inconsistent receiver positions");

    // check time consistency
    TFXX_assert((rdtseries.header.wid2().date 
                == recordeddata.fileheader.srce().date),
                "ERROR: trigger delay not supported");
    TFXX_assert((sdtseries.header.wid2().date 
                == syntheticdata.fileheader.srce().date),
                "ERROR: trigger delay not supported");

    // add this triple to collection
    vectoroftriples.push_back(tracetriple);

    // add this trace to convolved synthetics
    convolvedsynthetics.push_back(cstseries);

    // catch values for stf waveform
    if (itrace==0)
    {
      Tfile::Ttracevector::Ttimeseries stftseries;
      stftseries
        =Tfile::Ttracevector::Ttimeseries::Tseries(tracetriple.data.shape()); 

      stfwaveform.sampling.dt=tracetriple.header.sampling.dt;
      stfwaveform.sampling.n=tracetriple.header.sampling.n;
      stfwaveform.series=stftseries;

      ::sff::WID2 wid2=stftseries.header.wid2();
      wid2.nsamples=stfwaveform.sampling.n;
      wid2.dt=stfwaveform.sampling.dt;
      wid2.date=libtime::now();
      stftseries.header.wid2(wid2);
      ::sff::SRCE srce=stffile.fileheader.srce();
      srce.date=stftseries.header.wid2().date;
      stffile.fileheader.srce(srce);

      stffile.push_back(stftseries);
    } // if (itrace==0)

  } // for (unsigned int itrace=0; itrace<ntraces; ++itrace)

  /*----------------------------------------------------------------------*/

  // prepare additional pairs
  Tfile convolvedseries;

  // waveform data workspace
  stfinv::Tvectorofpairs vectorofpairs;
  vectorofpairs.clear();

  if (opt.additionalpairs)
  {

    convolvedseries.fileheader=seriesdata.fileheader;
    if (opt.verbose)
    {
      cout << "prepare additional series to be convolved" << endl;
    }

    // cycle through all traces
    for (unsigned int itrace=0; itrace<seriesdata.size(); ++itrace)
    {
      if (opt.verbose)
      {
        cout << "  check trace #" << itrace+1 << endl;
      }
      // create a reference to the time series with trace header for thsi
      // specific trace as read from file
      const Tfile::Ttracevector::Ttimeseries& sdtseries=seriesdata[itrace];

      // create a time series to be used in the output of convolved synthetics
      // based on the input synthetics; copy trace header
      Tfile::Ttracevector::Ttimeseries 
        cstseries(Tfile::Ttracevector::Ttimeseries::Tseries(sdtseries.shape()),
                 sdtseries.header, sdtseries.traceindex());

      // place reference in a waveform triple
      stfinv::WaveformPair tracepair;
      tracepair.synthetics=sdtseries;
      tracepair.convolvedsynthetics=cstseries;
      tracepair.sampling.dt=sdtseries.header.wid2().dt;
      tracepair.sampling.n=sdtseries.header.wid2().nsamples;

      // add this triple to collection
      vectorofpairs.push_back(tracepair);

      // add this trace to convolved synthetics
      convolvedseries.push_back(cstseries);

    } // for (unsigned int itrace=0; itrace<seriesdata.size(); ++itrace)

  } // if (opt.additionalpairs)
  
  /*----------------------------------------------------------------------*/

  // create STF engine
  // -----------------
  if (opt.verbose) 
  {
    parameters += ":verbose";
    cout << "create STF engine" << endl; 
  }
  if (opt.debug) { parameters += ":DEBUG="+opt.debuglevel; }
  stfinv::STFEngine engine(vectoroftriples, stfwaveform, 
                           vectorofpairs, parameters);

  // run STF engine
  // --------------
  if (opt.verbose) { cout << "run engine" << endl; }
  engine.run();

  // write results
  // -------------
  
  if (opt.writestf)
  {
    if (opt.verbose) 
    {
      cout << "write STF to file " << opt.stffilename << endl;
    }
    if (!opt.overwrite)
    {
      std::ifstream file(opt.stffilename.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    std::ios_base::openmode oopenmode
      =datrw::oanystream::openmode(opt.outputfileformat);
    std::ofstream ofs(opt.stffilename.c_str(), oopenmode);
    datrw::oanystream os(ofs, opt.outputfileformat, opt.debug);
    
    os << stffile.fileheader.srce();
    for (unsigned int itrace=0; itrace<stffile.size(); ++itrace)
    {
      os << stffile[itrace];
    }
  } // if (opt.writestf)

  if (opt.writeconvolved)
  {
    if (opt.verbose) 
    {
      cout << "write convolved synthetics to file " 
        << opt.convolvedfilename << endl;
    }
    if (!opt.overwrite)
    {
      std::ifstream file(opt.convolvedfilename.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    std::ios_base::openmode oopenmode
      =datrw::oanystream::openmode(opt.outputfileformat);
    std::ofstream ofs(opt.convolvedfilename.c_str(), oopenmode);
    datrw::oanystream os(ofs, opt.outputfileformat, opt.debug);
    
    os << convolvedsynthetics.fileheader.srce();
    for (unsigned int itrace=0; itrace<convolvedsynthetics.size(); ++itrace)
    {
      os << convolvedsynthetics[itrace];
    }
  } // if (opt.writeconvolved)

  if (opt.additionalpairs)
  {
    if (opt.verbose) 
    {
      cout << "write additional convolved series to file " 
        << opt.additionaloutput << endl;
    }
    if (!opt.overwrite)
    {
      std::ifstream file(opt.additionaloutput.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    std::ios_base::openmode oopenmode
      =datrw::oanystream::openmode(opt.outputfileformat);
    std::ofstream ofs(opt.additionaloutput.c_str(), oopenmode);
    datrw::oanystream os(ofs, opt.outputfileformat, opt.debug);
    
    os << convolvedseries.fileheader.srce();
    for (unsigned int itrace=0; itrace<convolvedseries.size(); ++itrace)
    {
      os << convolvedseries[itrace];
    }
  } // if (opt.additionalpairs)

} // main

/* ----- END OF soutifu.cc ----- */
