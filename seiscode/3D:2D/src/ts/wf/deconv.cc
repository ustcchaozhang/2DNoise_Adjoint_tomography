/*! \file deconv.cc
 * \brief calculate response function by deconvolution
 * 
 * ----------------------------------------------------------------------------
 *
 * \author Thomas Forbriger
 * \date 23/04/2009
 * 
 * calculate response function by deconvolution
 * 
 * Copyright (c) 2009 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 23/04/2009   V1.0   Thomas Forbriger
 *  - 02/08/2011   V1.1   added some columns to spectral output
 * 
 * ============================================================================
 */
#define DECONV_VERSION \
  "DECONV   V1.1   calculate response function by deconvolution"

#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/rangelist.h>
#include <tfxx/rangestring.h>
#include <tfxx/error.h>
#include <tsxx/tsxx.h>
#include <tsioxx/outputoperators.h>
#include <tsxx/tapers.h>
#include <tsxx/filter.h>
#include <tsioxx/inputoperators.h>
#include <fourier/fftwaff.h>
#include <datrwxx/readany.h>
#include <datrwxx/sff.h>

using std::cout;
using std::cerr;
using std::endl;

typedef aff::Series<double> Tseries;
typedef ts::sff::SFFTimeSeries<Tseries> Ttimeseries;
typedef fourier::fft::DRFFTWAFF Tfft;

/*----------------------------------------------------------------------*/

//! key to select traces
const char* const tracekey="t";
//! list of keys for filename specific parameters
const char* keys[]={
  //! select traces
  tracekey,
  0
}; // const char* keys[]

/*----------------------------------------------------------------------*/

// structure to hold values of command line options
struct Options {
  bool verbose, overwrite, writetransform;
  double damping, cosinefrac;
  std::string transformfile, inputformat;
  bool demean, detrend;
  int ndemean, ndetrend;
}; // struct Options

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    DECONV_VERSION "\n"
    "usage: deconv [-v] [-o] [-damping=v] [-cosine=v]" "\n"
    "              [-transform=file] [-type f]" "\n"
    "              [-demean[=n]] [-dtrend[=n]]" "\n"
    "              response input [t:l] [input [t:l]]" "\n"
    "   or: deconv --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "response     impulse response of filter" "\n"
    "input        input time series" "\n"
    "             Several filenames together with trace selectors can be" "\n"
    "             used. The first to traces read will be used for the" "\n"
    "             analysis:" "\n"
    "             trace #1: input to filter" "\n"
    "             trace #2: output from filter" "\n"
    "\n"
    "-verbose     be verbose" "\n"
    "-overwrite   overwrite existing output file" "\n"
    "-damping v   set damping to fraction v of average input energy" "\n"
    "-cosine v    use cosine taper, wherer 0<v<0.5 is the taper fraction" "\n"
    "-transform f write Fourier transform of impulse response to file \"f\"" "\n"
    "             1st column: frequency\n"
    "             2nd column: amplitude response\n"
    "             3nd column: phase response\n"
    "             4th column: amplitude response from Fourier amplitudes\n"
    "                         without damping (ragularization)\n"
    "             5th column: FFT of filter input signal\n"
    "             6th column: FFT of filter output signal\n"
    "-type f      data input files have format \"f\"" "\n"
    "-demean[=n]  remove average (determined from n samples)" "\n"
    "-detrend[=n] remove trend (determined from n samples)" "\n"
    "\n"
    "The program reads the input and output signals for a linear time" "\n"
    "invariant filter from the SFF data files \"input\" and \"output\"." "\n"
    "From these the impulse response function of the filter is" "\n"
    "calulated by a stabilized deconvolution in the Fourier domain." "\n"
    "The result will written to the SFF data file \"response\", where" "\n"
    "the source time defines the origin." "\n"
    "\n"
    "The default taper is a cosine taper of 0.5 (i.e. Hanning)." "\n"
    "Setting the cosine taper fraction to 0. will result in a" "\n"
    "boxcar taper." "\n"
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
    // 3: remove average
    {"demean",arg_opt,"0"},
    // 4: cosine taper
    {"cosine",arg_yes,"0.5"},
    // 5: output file for transform
    {"transform",arg_yes,"coeff.tab"},
    // 6: damping
    {"damping",arg_yes,"0.1"},
    // 7: damping
    {"type",arg_yes,"sff"},
    // 8: remove trend
    {"detrend",arg_opt,"0"},
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
    exit(0);
  }

  /*----------------------------------------------------------------------*/
  // extract command line options
  //
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.demean=cmdline.optset(3);
  opt.ndemean=cmdline.int_arg(3);
  opt.cosinefrac=cmdline.double_arg(4);
  opt.writetransform=cmdline.optset(5);
  opt.transformfile=cmdline.string_arg(5);
  opt.damping=cmdline.double_arg(6);
  opt.inputformat=cmdline.string_arg(7);
  opt.detrend=cmdline.optset(8);
  opt.ndetrend=cmdline.int_arg(8);

  // check option values
  TFXX_assert(((0<=opt.damping) && (opt.damping<=1.0)),
    "Damping fraction is out of meaningful range");

  if (opt.verbose)
  {
    cout << DECONV_VERSION << endl;
  }

  /*----------------------------------------------------------------------*/
  // extract file names and trace numbers from command line
  //
  TFXX_assert(cmdline.extra(), "missing output file name");
  std::string impulseresponsefilename=cmdline.next();

  TFXX_assert(cmdline.extra(), "missing input file name");
  // data file parameters from command line
  tfxx::cmdline::Tparsed 
    filenames=tfxx::cmdline::parse_cmdline(cmdline, keys);

  // extract input and ouput trace info
  std::string filterinputname, filteroutputname;
  int filterinputtrace, filteroutputtrace;
  {
    tfxx::cmdline::Tparsed::const_iterator I=filenames.begin();
    TFXX_assert(I!=filenames.end(), "Missing filter input file name");
    std::string tracelist="1";
    if (I->haskey(tracekey)) { tracelist=I->value(tracekey); }
    tfxx::RangeListStepper<int> rls(tfxx::string::rangelist<int>(tracelist));
    TFXX_assert(rls.valid(), "Illegal tracelist");
    filterinputname=I->name;
    filterinputtrace=rls.current();
    ++rls;
    if (!rls.valid())
    {
      ++I;
      TFXX_assert(I!=filenames.end(), "Missing filter output file name");
      tracelist="1";
      if (I->haskey(tracekey)) { tracelist=I->value(tracekey); }
      rls=tfxx::RangeListStepper<int>(tfxx::string::rangelist<int>(tracelist));
      TFXX_assert(rls.valid(), "Illegal tracelist");
    }
    filteroutputname=I->name;
    filteroutputtrace=rls.current();
  }
  if (opt.verbose) {
    cout << "filter input from file " << filterinputname
      << " (trace #" << filterinputtrace << ")" << endl;
    cout << "filter output from file " << filteroutputname
      << " (trace #" << filteroutputtrace << ")" << endl;
  }

  /*----------------------------------------------------------------------*/
  // FREE block ro report processing
  sff::FREE processfree, tracefree;
  std::ostringstream freeline;

  processfree.append(DECONV_VERSION);

  /*----------------------------------------------------------------------*/
  // read input time series
    
  sff::FREE finputfilefree, foutputfilefree;
  Ttimeseries finputtseries, foutputtseries;

  {
    // open filter input file
    if (opt.verbose) { cout << "open input file " << filterinputname << endl; }
    std::ifstream ifs(filterinputname.c_str());
    datrw::ianystream is(ifs, opt.inputformat);
    is >> finputfilefree;
    int itrace=1;
    while (itrace!=filterinputtrace) { is.skipseries(); ++itrace; }
    TFXX_assert(is.good(), "Illegal trace for filter input");
    is >> finputtseries;
  }
  tracefree.append("**** filter input signal");
  processfree.append("filter input signal");
  freeline << filterinputname << "(trace " << filterinputtrace << ")";
  tracefree.append(freeline.str());
  processfree.append(freeline.str());
  processfree.append(finputfilefree);
  processfree.append(finputtseries.header.free());

  {
    // open filter output file
    if (opt.verbose) { cout << "open input file " << filteroutputname << endl; }
    std::ifstream ifs(filteroutputname.c_str());
    datrw::ianystream is(ifs, opt.inputformat);
    is >> foutputfilefree;
    int itrace=1;
    while (itrace!=filteroutputtrace) { is.skipseries(); ++itrace; }
    TFXX_assert(is.good(), "Illegal trace for filter output");
    is >> foutputtseries;
  }
  tracefree.append("**** filter output signal");
  processfree.append("filter output signal");
  freeline.str("");
  freeline << filteroutputname << "(trace " << filteroutputtrace << ")";
  tracefree.append(freeline.str());
  processfree.append(freeline.str());
  processfree.append(foutputfilefree);
  processfree.append(foutputtseries.header.free());

  /*----------------------------------------------------------------------*/
  // check header consistency
  sff::WID2compare compare(sff::Fnsamples | sff::Fdt | sff::Fdate);
  if (opt.verbose) { cout << "checking consistency..." << endl; }
  if (!compare (finputtseries.header.wid2(),foutputtseries.header.wid2()))
  {
    cerr << "ERROR: header signature mismatch:" << endl;
    cerr << "filter input:" << endl;
    cerr << finputtseries.header.wid2().line();
    cerr << "filter output:" << endl;
    cerr << foutputtseries.header.wid2().line();
    TFXX_abort("baling out...");
  }

  /*----------------------------------------------------------------------*/
  // go for processing
  processfree.append("==== start processing ====");
  
  // create taper
  ts::tapers::Cosine taper(opt.cosinefrac);
  freeline.str("");
  freeline << "cosine taper fraction: " << opt.cosinefrac;
  processfree.append(freeline.str());
  if (opt.verbose) { cout << freeline.str() << endl; }
  
  const sff::WID2& iwid2=finputtseries.header.wid2();
  const sff::WID2& owid2=foutputtseries.header.wid2();

  // demean
  if (opt.demean) { 
    freeline.str("");
    freeline << "remove average estimated for ";
    if (opt.ndemean==0) { freeline << "all"; } else { freeline << opt.ndemean; }
    freeline << " samples";
    if (opt.verbose) { cout << freeline.str() << endl; }
    processfree.append(freeline.str());
    ts::filter::RemoveAverage demean(opt.ndemean);
    demean(ts::filter::Ttimeseries(finputtseries, iwid2.dt));
    demean(ts::filter::Ttimeseries(foutputtseries, owid2.dt));
  }

  // detrend
  if (opt.detrend) { 
    freeline.str("");
    freeline << "remove trend estimated for ";
    if (opt.ndetrend==0) { freeline << "all"; } 
    else { freeline << opt.ndetrend; }
    freeline << " samples";
    if (opt.verbose) { cout << freeline.str() << endl; }
    processfree.append(freeline.str());
    ts::filter::RemoveTrend detrend(opt.ndetrend);
    detrend(ts::filter::Ttimeseries(finputtseries, iwid2.dt));
    detrend(ts::filter::Ttimeseries(foutputtseries, owid2.dt));
  }

  // apply taper
  taper.apply(finputtseries);
  taper.apply(foutputtseries);

  // create FFTW processor
  Tfft fft;

  // Fourier transform
  if (opt.verbose) { cout << "Fourier transformation..." << endl; }
  Tfft::Tspectrum icoeff=fft(finputtseries, iwid2.dt);
  Tfft::Tspectrum ocoeff=fft(foutputtseries, owid2.dt);

  // calculate energy in input signal
  if (opt.verbose) { cout << "calculate energy in input signal..." << endl; }
  double energy=0;
  {
    aff::Browser<Tfft::Tspectrum> IC(icoeff);
    while (IC.valid())
    {
      energy += IC->real()*IC->real()+IC->imag()*IC->imag();
      ++IC;
    }
  }
  energy /= iwid2.nsamples;
  double damping=energy*opt.damping;
  
  // calculate response function
  // phase is calculated in terms of signal delay
  Tfft::Tspectrum resp(icoeff.size());
  Tseries amp(icoeff.size());
  Tseries ampdirect(icoeff.size());
  Tseries phase(icoeff.size());

  {
    aff::Iterator<Tfft::Tspectrum> RC(resp);
    aff::Browser<Tfft::Tspectrum> IC(icoeff);
    aff::Browser<Tfft::Tspectrum> OC(ocoeff);
    aff::Iterator<Tseries> AI(amp);
    aff::Iterator<Tseries> ADI(ampdirect);
    aff::Iterator<Tseries> PI(phase);
    const double radtodegrees=180./3.1415926535897931159979634685441;

    while(IC.valid() && OC.valid() && RC.valid())
    {
      //*RC = ( (*OC) * std::conj(*IC) ) / ( (*IC) * std::conj(*IC) + damping );
      *RC = ( (*OC) * std::conj(*IC) ) / ( std::norm(*IC) + damping );
      if (AI.valid()) { *AI = std::abs(*RC); ++AI; }
      if (PI.valid()) { *PI = -atan2(RC->imag(),RC->real())*radtodegrees; ++PI; }
      //if (ADI.valid()) { *ADI = aoutput/ainput; ++ADI; }
      if (ADI.valid()) { *ADI = (std::abs(*OC)/std::abs(*IC)); ++ADI; }
      ++OC; ++IC; ++RC;
    }
  }

  // calculate impulse response
  Tseries impresp=fft(resp, iwid2.dt);

  // write impulse response to output files
  {
    // open output file
    // ----------------
    std::string& outfile=impulseresponsefilename;
    if (opt.verbose) { cout << "open output file " << outfile << endl; }
    // check if output file exists and open
    if (!opt.overwrite)
    {
      std::ifstream file(outfile.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    std::ofstream ofs(outfile.c_str());
    datrw::osffstream os(ofs);

    os << iwid2;
    os << impresp;
    os << finputtseries;
    os << foutputtseries;
  }

  // write response spectrum
  
  if (opt.writetransform)
  {
    // length of time series
    double T=iwid2.dt*iwid2.nsamples;
    // frequency sampling interval
    double df=1/T;
    std::ofstream os(opt.transformfile.c_str());

    int fidx=0;
    aff::Browser<Tseries> AI(amp);
    aff::Browser<Tseries> ADI(ampdirect);
    aff::Browser<Tseries> PI(phase);
    aff::Browser<Tfft::Tspectrum> IC(icoeff);
    aff::Browser<Tfft::Tspectrum> OC(ocoeff);

    while(AI.valid() && PI.valid() && ADI.valid() && IC.valid() && OC.valid())
    {
      double f=fidx*df;
      os << f << " " << *AI << " " << *PI << " " << *ADI << " ";
      os << " " << abs(*IC) << " " << abs(*OC);
      os << endl;
      ++AI; ++PI; ++fidx; ++ADI; ++IC; ++OC;
    }
  }
}

/* ----- END OF deconv.cc ----- */
