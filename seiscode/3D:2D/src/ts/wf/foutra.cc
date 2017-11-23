/*! \file foutra.cc
 * \brief Fourier transforms
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 25/07/2006
 * 
 * Fourier transforms
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * FOUTRA is free software; you can redistribute it and/or modify
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
 *  - 25/07/2006   V1.0   Thomas Forbriger
 *  - 12/09/2007   V1.1   this version produces correct spectra
 *  - 13/09/2007   V1.2   The program is tested against Walter's code
 *                        now provides processing details in verbose mode and
 *                        in trace FREE block
 *  - 17/09/2007   V1.4   provides:
 *                        - demean and detrend
 *                        - scaling to average in relative bandwidth
 *                        - ASCII output
 *                        - output ASCII table on logarithmic frequency scale
 *                        - average only ASCII output if requested
 *  - 26/06/2009   V1.5   additional feature:
 *                        - adjust number of samples to make FFT more efficient
 *  - 08/10/2010   V1.6   start testing foutra scaling
 *  - 12/12/2010   V1.7   implemented amplitude scaling for harmonic signal
 *  - 14/12/2010   V1.8   implemented segment averaging
 *                        distinguish between input series, output spectrum
 *                        and segment (analysis) series
 *  - 10/01/2011   V1.8b  corrected Fourier transformation formula in
 *                        doxygen documentation
 *  - 03/12/2014   V1.9   provide output file format selector
 *  - 08/01/2015   V1.10  FIX: when using -scalerbw take bandwidth from
 *                        opt.scaledecades rather than opt.decades
 *  - 29/01/2015   V1.11  FEATURE: provide calculation of n-th derivative
 *                        of time series
 *
 *  \note 08/01/2010:
 *  Scaling for foutra power spectrum was tested against theory.
 *  The integral over the power spectral density calculated by foutra over the
 *  whole frequency band (up to Nyquist frequency) provides the total power of
 *  the signal (i.e. the variance, i.e. the square of the rms value).
 *
 *  \sa \ref page_foutra
 * 
 *  \todo
 *  The use of series containers is messy. The same container is used for
 *  different purposes, once being used as a reference, than as a copy.
 *  This should be sorted out.
 *
 * ============================================================================
 */
#define FOUTRA_VERSION \
  "FOUTRA   V1.10   Fourier transforms"

#include <iostream>
#include <tfxx/commandline.h>
#include <aff/series.h>
#include <aff/iterator.h>
#include <aff/dump.h>
#include <aff/seriesoperators.h>
#include <aff/functions/avg.h>
#include <aff/functions/rms.h>
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
#include <datrwxx/writeany.h>
#include <fourier/fftwaff.h>
#include <sstream>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, overwrite, debug, asciiout, logascii;
  std::string inputformat, asciibase, outputformat;
  bool amplitudespectrum, powerspectrum, boxcartaper;
  bool avgconstbw, avgrelbw, avgasciionly;
  bool demean, detrend, scalerbw, adjustdivisor;
  bool derivative;
  int ndemean, ndetrend, divisor;
  double decades, scaledecades, asciidecades, nderivative;
  int avgsamples;
  bool reportrms, harmonicsignal, padzeroes;
  int padfactor, nsegments;
}; // struct Options

// values type to be used for samples
typedef double Tvalue;

// time series
typedef aff::Series<Tvalue> Tseries;

// full featured time series file
typedef ts::sff::File<Tseries> Tfile;

typedef ts::TDsfftimeseries Ttimeseries;
typedef Ttimeseries::Tseries Tseries;

typedef fourier::fft::DRFFTWAFF Tfft;

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    FOUTRA_VERSION "\n"
    "usage: foutra [-v] [-o] [-type type] [-Type type] [-D] [-ASCII[=base]]\n"
    "              [-amplitude] [-power] [-logascii[=n]]" "\n"
    "              [-boxcar] [-avg[=n]] [-rbw[=n]] [-avgascii]" "\n"
    "              [-demean[=n]] [-dtrend[=n]] [-scalerbw[=n]]" "\n"
    "              [-divisor[=n]] [-rms] [-harmonic] [-pad n]" "\n"
    "              [-derivative n]" "\n"
    "              [-nsegments n]" "\n"
    "              outfile infile [t:T] [infile [t:T] ...]" "\n"
    "   or: foutra --help|-h" "\n"
    "   or: foutra --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "outfile      output filename" "\n"
    "infile       input filename" "\n"
    "             t:T select traces T, where T may be any range" "\n"
    "                 specification like \'3-4\' or \'5,6,7-12,20\'" "\n"
    "\n"
    "-help        prints this help text" "\n"
    "-xhelp       print information concerning supported data formats" "\n"
    "\n"
    "-v           be verbose" "\n"
    "-D           debug mode" "\n"
    "-o           overwrite output" "\n"
    "-boxcar      apply boxcar taper (i.e. no taper; default is Hanning)" "\n"
    "-amplitude   calculate amplitude spectrum" "\n"
    "-power       calculate power spectrum" "\n"
    "-type type   select input file type" "\n"
    "-Type type   select output file type" "\n"
    "-avg[=n]     smooth power spectrum by averaging over n samples" "\n"
    "-rbw[=n]     smooth power spectrum by averaging over n decades" "\n"
    "-demean[=n]  remove average (determined from n samples)" "\n"
    "-detrend[=n] remove trend (determined from n samples)" "\n"
    "-derivative[=n] take n-th derivative of time series" "\n"
    "-scalerbw[=n] scale to mean value in n decades" "\n"
    "-divisor[=n] FFT becomes very inefficient if the factorization" "\n"
    "             of the number of samples includes large prime numbers." "\n"
    "             This option removes the least number of samples to" "\n"
    "             the total number of samples a multiple of \"n\"" "\n"
    "-ASCII[=base] write result to two-column ASCII files "
                   "with basename \'base\'" "\n"
    "-logascii[=n] write ASCII data on logarithmic frequency axis with" "\n"
    "              one value per \'n\' decades" "\n"
    "-avgascii    only average values for output to ASCII file" "\n"
    "             this option speeds up calculation together with" "\n"
    "             -scalerbw which increases computation time" "\n"
    "             with the square of frequency" "\n"
    "-rms         report rms values of input data" "\n"
    "-harmonic    scale output appropriate fro harmonic signals" "\n"
    "             useful for two-tone-tests of linearity (see below)" "\n"
    "-pad n       pad time series with zeroes; n gives the integer factor" "\n"
    "             for the number of samples; the raw amplitude spectrum" "\n"
    "             has to be understood as the spectrum of the whole" "\n"
    "             series including the padded zeroes; PSD and harmonic" "\n"
    "             signals are scaled to represent the taper time window" "\n"
    "             only, such that padding is a means of smoothing only" "\n"
    "-nsegments n subdivide input time series in \"n\" overlapping" "\n"
    "             sequences (overlap is 50%); spectral analysis is" "\n"
    "             done for each sequence individually; the result" "\n"
    "             is the mean of all sequences; this applies to PSD" "\n"
    "             and harmonic signal analysis only; it is particularly" "\n"
    "             useful for two-tone-test where spectral smoothing" "\n"
    "             of background noise is anticpated, while maintaining" "\n"
    "             the full resolution for harmonic peaks" "\n"
    "\n"
    "If input units are K, then the output units of power spectra will" "\n"
    "be K*K/Hz. The units for amplitude spectra then are K/Hz. If scaling" "\n"
    "to the mean in a relative bandwidth is used (only applies for power" "\n"
    "spectra; switch -scalerbw) the output units are K*K." "\n"
    "\n"
    "The Fourier transformation does not exist for harmonic signals." "\n"
    "The option \"-harmonic\" supports the analysis of a time limited" "\n"
    "portion of an harmonic signal. If this option is set, the output" "\n"
    "is scaled such that the spectral values of the peaks of harmonic" "\n"
    "signals are the time domain amplitude in units of K." "\n"
    "\n"
    "The integral over the power spectral density calculated by foutra" "\n"
    "over the total bandwidth (over all frequencies from 0 Hz to Nyquist" "\n" 
    "frequency) provides the total power of the signal (i.e. the" "\n"
    "variance of the input signal, i.e. the square of the rms value)." "\n"
    "This is called the one-sided power spectral density." "\n"
    "\n"
    "The input signal can be extended by padding with zeroes. This" "\n"
    "mainly is used to obtain a smoother representation of the" "\n"
    "corresponding spectral display where the amplitude of narrow" "\n"
    "peaks might not fall on a spectral node when analysing harmonic" "\n"
    "signals. The spectral representation consequently is scaled" "\n"
    "to the length of the applied taper function (and not to the" "\n"
    "full length of the padded time series) for harmonic" "\n"
    "signal analysis and power spectral densities, since the" "\n"
    "signal under investigation is understood as being infinite in" "\n"
    "time. For amplitude spectra of transient signals (the default)" "\n"
    "no scaling to a time window takes place, since padding with" "\n"
    "zeroes is understood as not altering the signal." "\n"
    "\n"
    "Output is written in SFF. There the sampling interval provided" "\n"
    "in the WID2 line specifies the frequency sampling interval of the" "\n"
    "spectrum. The first coefficient in the file is at 0 Hz. The" "\n"
    "last is the coefficient at Nyquist frequency." "\n"
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
    // 3: input format
    {"type",arg_yes,"sff"},
    // 4: debug mode
    {"D",arg_no,"-"},
    // 5: amplitude spectrum
    {"amplitude",arg_no,"-"},
    // 6: power spectrum
    {"power",arg_no,"-"},
    // 7: apply boxcar taper
    {"boxcar",arg_no,"-"},
    // 8: average over constant number of samples
    {"avg",arg_opt,"21"},
    // 9: average over constant relative bandwidth (in decades)
    {"rbw",arg_opt,"0.167"},
    // 10: remove average
    {"demean",arg_opt,"0"},
    // 11: remove trend
    {"detrend",arg_opt,"0"},
    // 12: scale to mean in relative bandwidth
    {"scalerbw",arg_opt,"0.167"},
    // 13: write result to ASCII files
    {"ASCII",arg_opt,"spectrum"},
    // 14: write result to ASCII files
    {"logascii",arg_opt,"0.167"},
    // 15: average ASCII output only
    {"avgascii",arg_no,"-"},
    // 16: divisor for number of samples
    {"divisor",arg_opt,"100"},
    // 17: report rms value
    {"rms",arg_no,"-"},
    // 18: report rms value
    {"harmonic",arg_no,"-"},
    // 19: pad time series
    {"pad",arg_yes,"1"},
    // 20: subdivide input series
    {"nsegments",arg_yes,"1"},
    // 21: extended help
    {"xhelp",arg_no,"-"},
    // 22: input format
    {"Type",arg_yes,"sff"},
    // 23: input format
    {"derivative",arg_opt,"1"},
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
  if (cmdline.optset(0) || cmdline.optset(21))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    datrw::supported_data_types(cerr);
    if (cmdline.optset(21)) { datrw::online_help(cerr); }
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.inputformat=cmdline.string_arg(3);
  opt.debug=cmdline.optset(4);
  opt.amplitudespectrum=cmdline.optset(5);
  if (!opt.amplitudespectrum)
  { opt.powerspectrum=cmdline.optset(6); }
  opt.boxcartaper=cmdline.optset(7);
  opt.avgconstbw=cmdline.optset(8);
  opt.avgsamples=cmdline.int_arg(8);
  opt.avgrelbw=cmdline.optset(9);
  opt.decades=cmdline.double_arg(9);
  opt.demean=cmdline.optset(10);
  opt.ndemean=cmdline.int_arg(10);
  opt.detrend=cmdline.optset(11);
  opt.ndetrend=cmdline.int_arg(11);
  opt.scalerbw=cmdline.optset(12);
  opt.scaledecades=cmdline.double_arg(12);
  opt.asciiout=cmdline.optset(13);
  opt.asciibase=cmdline.string_arg(13);
  opt.logascii=cmdline.optset(14);
  opt.asciidecades=cmdline.double_arg(14);
  opt.avgasciionly=cmdline.optset(15);
  opt.adjustdivisor=cmdline.optset(16);
  opt.divisor=cmdline.int_arg(16);
  opt.reportrms=cmdline.optset(17);
  opt.harmonicsignal=cmdline.optset(18);
  opt.padzeroes=cmdline.optset(19);
  opt.padfactor=cmdline.int_arg(19);
  opt.nsegments=cmdline.int_arg(20);
  opt.outputformat=cmdline.string_arg(22);
  opt.derivative=cmdline.optset(23);
  opt.nderivative=cmdline.double_arg(23);

  TFXX_assert((opt.divisor > 0), "illegal value for argument divisor");
  TFXX_assert((opt.nsegments > 0), "illegal value for argument nsegments");
  TFXX_assert((opt.padfactor > 0), "illegal value for argument pad");

  if (opt.verbose)
  { cout << FOUTRA_VERSION << endl; }

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
  // do not apply segmentation if not PSD or harmonic signal analysis
  if (!(opt.powerspectrum || opt.harmonicsignal)) { opt.nsegments=1; }

  /*======================================================================*/
  // create processing description
  sff::FREE processfree;
  if (opt.nsegments>1)
  {
    std::ostringstream freeline;
    freeline << "Input series is subdivided into "
      << opt.nsegments << " segments with 50% overlap";
    processfree.append(freeline.str());
    processfree.append("The analysis is done individually for all segments");
    processfree.append("The result is the average over all segments");
  }
  if (opt.demean)
  {
      std::ostringstream freeline;
      freeline << "The average over ";
      if (opt.ndemean >0)
      {
        freeline << opt.ndemean;
      }
      else
      {
        freeline << "all";
      }
      freeline << " samples is removed from all samples";
      processfree.append(freeline.str());
  }
  if (opt.detrend)
  {
      std::ostringstream freeline;
      freeline << "The trend over ";
      if (opt.ndetrend >0)
      {
        freeline << opt.ndetrend;
      }
      else
      {
        freeline << "all";
      }
      freeline << " samples is removed from all samples";
      processfree.append(freeline.str());
  }
  if (opt.boxcartaper)
  {
    processfree.append("A boxcar taper (i.e no taper) is applied to the data");
  }
  else
  {
    processfree.append("A Hanning taper is applied to the data");
  }
  if (opt.padzeroes) 
  {
    std::ostringstream freeline;
    freeline << "pad with zeroes increasing the total "
      << "number of samples by a factor of " << opt.padfactor;
    processfree.append(freeline.str());
  }
  if (opt.harmonicsignal || opt.amplitudespectrum || opt.powerspectrum)
  {
    processfree.append("An appropriately scaled FFT (libdrfftw) is applied");
    if (opt.derivative)
    {
      processfree.append("Calculate values for derivative with respect to");
      processfree.append("  time by multiplication of the Fourier coefficients");
      std::ostringstream freeline;
      freeline << "  with the angular frequency to the power of " 
        << opt.nderivative;
      processfree.append(freeline.str());
    }
  }
  if (opt.amplitudespectrum)
  {
    processfree.append("The result are coefficients of the amplitude spectrum");
    processfree.append("Units are: input units / Hz");
  }
  else if (opt.powerspectrum)
  {
    processfree.append("The result are coefficients of the power spectrum");
    if (opt.scalerbw) 
    {
      std::ostringstream freeline;
      freeline << "The result is the average power in " 
        << opt.scaledecades << " decades";
      processfree.append(freeline.str());
      processfree.append("Units are: input units squared");
    }
    else
    {
      processfree.append("Units are: input units squared / Hz");
    }
    if ((opt.avgconstbw || opt.avgrelbw) && !opt.avgasciionly)
    {
      processfree.append("The spectrum is smoothed with a boxcar over");
      if (opt.avgconstbw)
      {
        std::ostringstream freeline;
        freeline << opt.avgsamples << " samples";
        processfree.append(freeline.str());
      }
      else if (opt.avgrelbw)
      {
        std::ostringstream freeline;
        freeline << opt.decades << " decades";
        processfree.append(freeline.str());
      }
    }
    else
    {
      processfree.append("The spectrum is not smoothed");
      if (opt.avgasciionly && opt.asciiout)
      {
        processfree.append("The ASCII output is smoothed with a boxcar over");
        std::ostringstream freeline;
        freeline << opt.decades << " decades";
        processfree.append(freeline.str());
      }
    }
  }
  else if (opt.harmonicsignal)
  {
    processfree.append("The input is understood to contain harmonic signals.");
    processfree.append("The result are signal amplitudes for harmonic peaks.");
    processfree.append("Units are: input units");
  }
  else
  {
    processfree.append("The output is a tapered version of the time series");
  }
  if (opt.amplitudespectrum || opt.powerspectrum || opt.harmonicsignal)
  {
    processfree.append("The sampling interval provided in the WID2 line");
    processfree.append("specifies the sampling interval of the spectrum");
    processfree.append("in Hz. The first coefficient is that at 0 Hz.");
  }

  if (opt.verbose)
  {
    cout << "processing of each trace will take place as follows:" << endl;
    for(sff::FREE::Tlines::const_iterator I=processfree.lines.begin(); 
        I != processfree.lines.end(); ++I) 
    { cout << "  " << *I << std::endl; }
  }

  /*======================================================================*/
  // start processing
  
  // create taper
  ts::tapers::Hanning taper;
  // create FFTW processor
  Tfft fft;

  // open output file
  // ----------------
  if (opt.verbose) { cout << "open output file " << outfile << endl; }
  // check if output file exists and open
  if (!opt.overwrite)
  {
    std::ifstream file(outfile.c_str(),std::ios_base::in);
    TFXX_assert((!file.good()),"ERROR: output file exists!");
  }
  std::ofstream ofs(outfile.c_str());
  datrw::oanystream os(ofs, opt.outputformat);

  // prepare file FREE block
  sff::FREE filefree;
  filefree.append(FOUTRA_VERSION);
  // set flag to process header of first input file
  bool firstfile=true;
  // count output traces
  int otrace=0;
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
        Tseries inputseries;
        is >> inputseries;
        sff::WID2 wid2;
        is >> wid2;
        TFXX_debug(opt.debug, "main", "  series and WID2 are read");

  /*----------------------------------------------------------------------*/
  // report rms value if requested
        if (opt.reportrms)
        {
          std::cout << "    rms-value of input time series: "
            << aff::func::rms(inputseries) << std::endl;
        }
  /*----------------------------------------------------------------------*/
  // process data
  //
  // inputseries: full series read from file
  // series: series for one segment to be analysed 
  //         the spectral representation of a single segment is also stored in
  //         this container
  // spectrum: resulting spectral representation
  //
  // in the previous (pre segmentation) version of foutra only "series" was
  // used; we will make use of the aff::Series reference semantics to use the
  // old code with no more modifications than necessary; such "series" will
  // be used for different things
  //
  // wid2.dt remains valid throughout the processing
  // wid2.nsamples is only correct for the inputseries
  //   use series.size() for other request for number of samples

        // prepare segmentation
        // --------------------
        int nsegsamples=inputseries.size();
        if (opt.nsegments>1)
        {
          nsegsamples=2*inputseries.size()/(opt.nsegments+1); 
        }

        // adjust length of time series
        if (opt.adjustdivisor)
        {
          if (opt.verbose)
          {
            std::cout << "    adjust divisor to " << opt.divisor << std::endl;
            std::cout << "    number of input samples:      " << wid2.nsamples
              <<std::endl;
            std::cout << "    number of segment samples:    " << nsegsamples
              <<std::endl;
          }
          int rest=nsegsamples % opt.divisor;
          nsegsamples=nsegsamples-rest;
          if (opt.verbose)
          {
            std::cout << "    number of processing samples: " << nsegsamples
              <<std::endl;
          }
        }

        // segment stride
        int segstride=inputseries.size()/(opt.nsegments+1);

        if (opt.verbose && (opt.nsegments>1))
        {
           cout << "    number of input samples:           " 
             << inputseries.size() << endl;
           cout << "    number of segments:                " 
             << opt.nsegments << endl;
           cout << "    number of samples in each segment: " 
             << nsegsamples << endl;
           cout << "    stride between segments:           "
             << segstride << endl;
           cout << "    overlap of proximate segments:     "
             << 100.*(nsegsamples-segstride)/nsegsamples << "%" << endl;
        }

        // length of time series segment
        double T=wid2.dt*nsegsamples;
        // length of taper window
        double Tw=T;
        // frequency sampling interval
        double df=1./T;

        if (opt.padzeroes)
        {
          T *= opt.padfactor;
          df /= opt.padfactor;
        }

        if (opt.verbose)
        {
          cout << "    duration T of each segment:     " 
            << T << "s" << endl;
          cout << "    duration Tw of signal window:   "
            << Tw << "s" << endl;
          cout << "    frequency sampling interval df: "
            << df << "Hz" << endl;
        }
            
        // the result will be collected in
        Tseries spectrum;

        // segment counter
        int isegment=0;

        // start actual processing
        // -----------------------
        while (isegment<opt.nsegments)
        {
          int firstindex=inputseries.first()+isegment*segstride;
          int lastindex=firstindex+nsegsamples-1;
          TFXX_debug(opt.debug, "main", "  segment index range: "
                     << firstindex << "-" << lastindex);
          TFXX_assert((firstindex >= inputseries.first()) &&
                      (firstindex <= inputseries.last()) &&
                      (lastindex >= inputseries.first()) &&
                      (lastindex <= inputseries.last()),
                      "ERROR: index out of range; program design error");
          Tseries segseries=aff::subarray(inputseries)(firstindex,lastindex);
          Tseries series=segseries;
          if (opt.nsegments>1) { series=segseries.copyout(); }
          series.shift(-series.first());

          // demean
          if (opt.demean) { 
            TFXX_debug(opt.debug, "main", "  demean");
            ts::filter::RemoveAverage demean(opt.ndemean);
            demean(ts::filter::Ttimeseries(series, wid2.dt));
          }

          // detrend
          if (opt.detrend) { 
            TFXX_debug(opt.debug, "main", "  detrend");
            ts::filter::RemoveTrend detrend(opt.ndetrend);
            detrend(ts::filter::Ttimeseries(series, wid2.dt));
          }

          // apply taper
          if (!opt.boxcartaper) { 
            TFXX_debug(opt.debug, "main", "  apply taper");
            taper.apply(series); 
          }

          // pad time series
          // Notice: padding the signals requires correct scaling of amplitudes
          // this is only implemented for harmonic signals up to now
          if (opt.padzeroes)
          {
            Tseries newseries(series.size()*opt.padfactor);
            newseries=0.;
            Tseries subseries(aff::subarray(newseries)(series.f(),series.l()));
            subseries.copyin(series);
            series=newseries;
          }

          // call FFT for amplitude or power spectrum or harmonic signal
          if (opt.amplitudespectrum || opt.powerspectrum || opt.harmonicsignal)
          {
            TFXX_debug(opt.debug, "main", "call FFT");
            Tfft::Tspectrum coeff=fft(series, wid2.dt);
            TFXX_debug(opt.debug, "main",
                       "returned from FFT; create output series");
            series=Tseries(coeff.size());
            TFXX_debug(opt.debug, "main", "create iterators");
            aff::Iterator<Tseries> S(series);
            aff::Browser<Tfft::Tspectrum> C(coeff);

            // take derivative if selected
            if (opt.derivative)
            {
              for (int i=coeff.f()+1; i<=coeff.l(); ++i)
              {
                double frequency=df*(i-coeff.f())*2*M_PI;
                double thepower=opt.nderivative;
                double factor=std::pow(frequency,thepower);
                coeff(i) *= factor;
              }
            }

            TFXX_debug(opt.debug, "main", "calculate square of modulus and copy");
            while(S.valid() && C.valid())
            {
              *S = C->real()*C->real()+C->imag()*C->imag();
              ++S; ++C;
            }
          }
            
          // take sqrt for amplitude spectrum
          if (opt.amplitudespectrum || opt.harmonicsignal)
          {
            TFXX_debug(opt.debug, "main", "calculate sqrt and scale");
            aff::Iterator<Tseries> S(series);
            while(S.valid())
            {
              *S = std::sqrt(*S);
              ++S;
            }
          }
          
          // apply scaling appropriate for harmonic signals
          if (opt.harmonicsignal)
          {
            // amplitude of Hanning taper instrument function equals 1
            // amplitude of Boxcar taper instrument function equals 2
            double fscaling;
            if (opt.boxcartaper) 
            { fscaling = 2./Tw; }
            else
            { fscaling = 4./Tw; }
            series *= fscaling;
          }

          // apply appropriate scaling and averaging for power spectrum
          if (opt.powerspectrum)
          {
            // scaling factor to adjust for taper effect
            double tapscaling=1.;
            if (opt.boxcartaper)
            {
              tapscaling=1.;
            }
            else
            {
              // scaling factor for Hanning
              // see below for derivation of this factor
              tapscaling=8./3.;
            }

            // we have an energy spectrum so far
            // adjust scaling factor to obtain signal power
            double scalingfactor=2.*tapscaling/Tw;

            // scale to relative bandwidth if requested
            if (opt.scalerbw)
            {
              TFXX_debug(opt.debug, "main", 
                         "scale to average in relative bandwidth");
              double bwfactor=(std::pow(10.,opt.scaledecades)-1.)/
                std::pow(10.,opt.scaledecades/2.);
              // cout << "bwfactor: " << bwfactor << endl;
              // cout << "index range: " 
              //   << series.f() << "-" << series.l() << endl;
              for (int k=series.f(); k<=series.l(); ++k)
              {
                // center frequency
                double fm=k*df;
                series(k) *= scalingfactor*bwfactor*fm;
              }
            } // if (opt.scalerbw)
            else
            {
              TFXX_debug(opt.debug, "main", "scale");
              series *= scalingfactor;
            } // if (opt.scalerbw) else

            // apply smoothing
            if (opt.avgconstbw && !opt.avgasciionly)
            {
              TFXX_debug(opt.debug, "main", "average over constant bandwidth");
              // create a copy
              Tseries p(series.size());
              p.copyin(series);
              int d=opt.avgsamples/2+1;
              for (int k=p.f(); k<=p.l(); ++k)
              {
                int k1=k-d;
                int k2=k+d;
                k1 = k1 < p.f() ? p.f() : k1;
                k2 = k2 > p.l() ? p.l() : k2;
                // cout << k1 << " " << k << " " << k2 << endl;
                TFXX_assert(k2>k1, "negative bandwidth for averaging")
                Tseries sub=aff::subarray(p)(k1,k2);
                series(k)=aff::func::avg(sub);
                /*
                series(k)=0;
                for (int j=sub.f(); j<=sub.l(); ++j)
                {
                  series(k) += sub(j);
                }
                series(k) /= double(sub.size());
                */
              } // for (int k=p.f(); k<=p.l(); ++k)
            } // if (opt.avgconstbw && !opt.avgasciionly)
            else if (opt.avgrelbw && !opt.avgasciionly)
            {
              TFXX_debug(opt.debug, "main", "average over relative bandwidth");
              // create a copy
              TFXX_debug(opt.debug, "main", "  create a copy");
              Tseries p(series.size());
              TFXX_debug(opt.debug, "main", "  copy in data");
              p.copyin(series);
              double bwfactor=std::sqrt(std::pow(10.,opt.decades));
              // cout << bwfactor << endl;
              TFXX_debug(opt.debug, "main", "  cycle over data");
              for (int k=p.f(); k<=p.l(); ++k)
              {
                // center frequency
                double fm=k*df;
                double f1=fm/bwfactor;
                double f2=fm*bwfactor;
                // cout << fm << " " << f1 << " " << f2 << endl;
                int k1=int(f1/df);
                int k2=int(f2/df)+1;
                k1 = k1 < p.f() ? p.f() : k1;
                k2 = k2 > p.l() ? p.l() : k2;
                TFXX_assert(k2>k1, "negative bandwidth for averaging")
                Tseries sub=aff::subarray(p)(k1,k2);
                series(k)=aff::func::avg(sub);
              } // for (int k=p.f(); k<=p.l(); ++k)
            } // else if (opt.avgrelbw && !opt.avgasciionly)
          } // if (opt.powerspectrum)

          // collect result
          if (isegment==0)
          {
            spectrum=series.copyout();
            TFXX_debug(opt.debug, "main", "copy result");
          }
          else
          {
            // spectrum += series;
            for (int i=spectrum.f(),
                 j=series.f();
                 i<=spectrum.l(); ++i, ++j)
            {
              TFXX_assert(j<=series.l(),
                          "ERROR: program design error");
              spectrum(i) += series(j);
            }
            TFXX_debug(opt.debug, "main", "added result");
          }

          ++isegment;
          TFXX_debug(opt.debug, "main",
                     "finished segment #" << isegment);
        } // while (isegment<opt.nsegments)

        if (opt.nsegments>1) { spectrum /= opt.nsegments; }
        // use reference semantics to make results available under previous
        // name
        Tseries series=spectrum;

  /*----------------------------------------------------------------------*/
  // end of analysis section
  // begin of output section
  /*----------------------------------------------------------------------*/

        // adjust sampling interval to frequency interval
        if (opt.amplitudespectrum || opt.powerspectrum || opt.harmonicsignal)
        { wid2.dt=df; }
        wid2.nsamples=series.size();

        os << wid2;
        TFXX_debug(opt.debug, "main", "  series and WID are written");
        if (is.hasinfo()) { sff::INFO info; is >> info; os << info; }
        if (is.hasfree() || true) 
        {
          sff::FREE tracefree;
          is >> tracefree;
          tracefree.append(FOUTRA_VERSION);
          tracefree.append("data read from file " + infile->name);
          tracefree.append(processfree);
          os << tracefree;
        }
        os << series;

        // write ASCII output if requested
        if (opt.asciiout)
        {
          TFXX_debug(opt.debug, "main", "write to ASCII file");
          ++otrace;
          std::ostringstream outname;
          outname << opt.asciibase << ".";
          outname.width(3);
          outname.fill('0');
          outname << otrace << ".asc";
          std::string filename(outname.str());
          std::ofstream aos(filename.c_str());

          Tseries f;
          Tseries p;
          if (opt.logascii)
          {
            TFXX_debug(opt.debug, "main", 
                       "prepare data with logarithmic scale");
            // f_k = f_min * pow(10,k*ndecades)
            // k = log10( f_k/f_min ) / ndecades
            int kmin=0;
            // f_min = df
            // f_max = df*series.l();
            int kmax=int(std::log10(double(series.l()))/opt.asciidecades);
            TFXX_debug(opt.debug, "main", 
                       "  output index range: " << kmin << "-" << kmax);
            f=Tseries(kmin,kmax);
            p=Tseries(kmin,kmax);
            double bwfactor=std::sqrt(std::pow(10.,opt.decades));
            for (int k=f.f(); k<=f.l(); ++k)
            {
              // center frequency
              double fm=df*std::pow(10.,double(k*opt.asciidecades));
              int l=int(0.5+fm/df);
              f(k)=l*df;
              if (opt.avgasciionly)
              {
                // center frequency
                double fm=f(k);
                double f1=fm/bwfactor;
                double f2=fm*bwfactor;
                int l1=int(f1/df);
                int l2=int(f2/df)+1;
                l1 = l1 < series.f() ? series.f() : l1;
                l2 = l2 > series.l() ? series.l() : l2;
                /*
                TFXX_debug(opt.debug, "main", 
                           "  center frequency: " << fm << " Hz");
                TFXX_debug(opt.debug, "main", 
                           "  average: " << f1 << " Hz - " << f2 << " Hz");
                TFXX_debug(opt.debug, "main", 
                           "  index range: " << l1 << " - " << l2);
                */
                TFXX_assert(l2>=l1, "negative bandwidth for averaging");
                Tseries sub=aff::subarray(series)(l1,l2);
                p(k)=aff::func::avg(sub);
              } // if (opt.avgasciionly)
              else
              {
                p(k)=series(l);
              } // if (opt.avgasciionly) else
            } // for (int k=f.f(); k<=f.l(); ++k)
          } // if (opt.logascii)
          else
          {
            // just copy and prepare frequency vector
            f=Tseries(series.size());
            p=Tseries(series.size());
            p.copyin(series);
            for (int k=f.f(); k<=f.l(); ++k)
            {
              f(k) = k*df;
            }
          } // if (opt.logascii) else
          for (int k=f.f(); k<=f.l(); ++k)
          {
            // center frequency
            aos.setf(std::ios_base::scientific,std::ios_base::floatfield);
            aos.precision(10);
            aos << f(k) << " ";
            aos.setf(std::ios_base::scientific,std::ios_base::floatfield);
            aos.precision(10);
            aos << p(k) << endl;
          }
        } // if (opt.asciiout)

        TFXX_debug(opt.debug, "main", 
                   "trace #" << itrace << " successfully processed");
      }
      else
      {
        TFXX_debug(opt.debug, "main", "skip trace #" << itrace );
        if (opt.verbose)
        { std::cout << "  skip trace #" << itrace << std::endl; }
        is.skipseries();
      } // if ((!doselect) || traceranges.contains(itrace))
    } // while (is.good())
    
    // go to next file
    firstfile=false;
    ++infile;
  } // while (infile != arguments.end())

} // main

/*======================================================================*/

/*! \page page_foutra Spectral analysis (foutra.cc)
 *
 * Sections in this page:
 *   - \ref sec_foutra_fourier_scaling
 *   - \ref sec_foutra_hanning_PSD_scaling
 *   - \ref sec_foutra_scaling_harmonics
 *     - \ref subsec_foutra_scaling_harmonics_harmsig
 *     - \ref subsec_foutra_scaling_harmonics_boxcar
 *     - \ref subsec_foutra_scaling_harmonics_hanning
 *     - \ref subsec_foutra_scaling_harmonics_tests
 *
 * <HR>
 * \section sec_foutra_fourier_scaling Scaling of Fourier transforms
 *
 * The Fourier transform presented in libfourier is scaled appropriately to
 * represent coefficients of the Fourier integral transform (see libfourier
 * documentation).
 *
 * The maximum of the transform of the Boxcar taper is T,
 * where T is the duration of the window. Similarly the maximum
 * of the Fourier transform of the Hanning taper is 0.5*T, where the duration of
 * the window is T.
 *
 * \date 12/2010 \author thof
 */

/*----------------------------------------------------------------------*/

/*! \page page_foutra
 * <HR>
 * \section sec_foutra_hanning_PSD_scaling Scaling factor for Hanning taper when applied in PSD calculation
 *
 * Tapering stationary noise with a Hanning taper reduces total signal energy
 * an thus the signal power obtained through an FFT. In his tutorial Agnew
 * recommends to scale each sample \f$s_k\f$ of the series by \f$w_k/W\f$,
 * where \f$w_k\f$ is the \f$k\f$-th sample of the taper and 
 *
 * \f[
 *   W^2 = \frac{1}{N} \sum\limits_0^{N-1} w_k^2
 * \f]
 *
 * is a measure for the loss in total signal power due to application of the
 * taper. This applies only for staionary noise.
 * 
 * From Walter's Matlab
 * scripts I took:
 *
 * If data is the time series vector and y is the Hanning taper of appropriate
 * length, Walter calculates
 *   
 * \code
 *   w = sqrt(sum(y.*y)/ndata);
 * \endcode
 *
 * and
 *   
 * \code
 *   y=y/w;
 * \endcode
 *
 * where ndata is the length of data and y.
 *
 * For a Hanning taper
 * \f[
 *   w_k = \sin^2(k \pi/(N-1)).
 * \f]
 *
 * Thus
 *
 * \f[
 *   W^2 = \frac{1}{N} \sum\limits_0^{N-1} \sin^4(k\pi/(N-1))
 * \f]
 *
 * From Gradshteyn and Ryzhik (eq. 1.321 3) I find
 *
 * \f[
 *   \sin^4(k\pi/(N-1))
 *
 *     = \frac{1}{8} \left( \cos(4 k\pi/(N-1))
 *                 - 4 \cos(2 k\pi/(N-1))
 *                 + 3 \right).
 * \f]
 *
 * Within the sum the contribution of both cos-terms will vanish, since both
 * are averaged over one and two periods, respectively. Thus
 *
 * \f[
 *   W^2 = \frac{1}{N} \sum\limits_0^{N-1} \frac{3}{8} = \frac{3}{8}.
 * \f]
 *
 * Since foutra is not scaling the taper but scaling the power spectrum, we
 * have to apply the factor 8/3 to the result of power spectrum calculation.
 *
 * This factor 8/3=2.66667 was tested against the value for \f$W^2\f$, when
 * explicitely derived from a Hanning taper time series by the above formula.
 *
 * In the Makefile you will find a section with test code. 
 * This offers instantaneous testing of PSD scaling in foutra.
 * \dontinclude Makefile
 * \skip # test foutra PSD scaling
 * \until #----------------------------------------------------------------------
 *
 * \date 13/9/2007 \author thof
 *
 */

/*----------------------------------------------------------------------*/

/*! \page page_foutra
 * <HR>
 * \section sec_foutra_scaling_harmonics Scaling for harmonic signals
 *
 * For harmonic signals the FFT normalized to the duration of the available
 * time window has an amplitude of the value of the maximum of the Fourier
 * transform of the window function times half the amplitude of the time domain
 * signal. To obtain a spectral representation with peaks of the amplitude of
 * the time domain signal, the FFT must be scaled accordingly.
 *
 * We understand 
 * \f[
 *   \tilde{f}(\omega)=\int\limits_{-\infty}^{+\infty} 
 *     f(t) \,e^{-i\omega t} \textrm{d}{t}.
 * \f]
 * as the Fourier transformation of the time domain signal
 * \f[
 *   f(t)=\int\limits_{-\infty}^{+\infty} 
 *     \tilde{f}(\omega) \,e^{i\omega t} \frac{\textrm{d}{\omega}}{2\pi}.
 * \f]
 * If we apply a time domain window function \f$w(t)\f$ to the function
 * \f$f(t)\f$, we obtain the tapered function
 * \f[
 *   g(t)=f(t) w(t)
 * \f]
 * and its Fourier transform
 * \f[
 *   \tilde{g}(\omega)=\int\limits_{-\infty}^{+\infty}
 *     \tilde{f}(\omega')\,\tilde{w}(\omega-\omega')\,\textrm{d}\omega.
 * \f]
 *
 * \subsection subsec_foutra_scaling_harmonics_harmsig Application to harmonic signals 
 *
 * Let
 * \f[
 *   f(t)=A\,\cos(\omega_0 t+\phi)
 * \f]
 * be the harmonic signal under investigation.
 * Then
 * \f[
 *   f(t)=A\left\{\cos(\omega_0 t)\cos(\phi)-
 *     \sin(\omega_0 t)\sin(\phi)\right\}
 * \f]
 * and
 * \f[
 *   \tilde{f}(\omega) =
 *     \frac{A}{2}\left\{
 *       \cos(\phi)
 *         \left[
 *           \delta(\omega-\omega_0) 
 *           +
 *           \delta(\omega+\omega_0) 
 *         \right]
 *       +i\sin(\phi)
 *         \left[
 *           \delta(\omega-\omega_0) 
 *           -
 *           \delta(\omega+\omega_0) 
 *         \right]
 *     \right\},
 * \f]
 * where \f$\delta(\omega)\f$ is Dirac's delta function with
 * \f[
 *   \delta(\omega)=\left\{
 *     \begin{array}{ll}
 *       \infty & \textrm{if $\omega=0$ and}\\
 *       0 & \textrm{otherwise}
 *     \end{array}
 *   \right.
 * \f]
 * and
 * \f[
 *   \int\limits_{-\infty}^{+\infty}
 *     \delta(\omega)\,\textrm{d}\omega=1
 * \f]
 * such that
 * \f[
 *   \tilde{f}(\omega)=\int\limits_{-\infty}^{+\infty}
 *     \tilde{f}(\omega')\,\delta(\omega-\omega')\,\textrm{d}\omega.
 * \f]
 * This way I obtain
 * \f[
 *   \tilde{g}(\omega)=
 *     \frac{A}{2}\left\{
 *       e^{i\phi}\tilde{w}(\omega-\omega_0)
 *       +
 *       e^{-i\phi}\tilde{w}(\omega+\omega_0)
 *     \right\}
 * \f]
 * for the Fourier transform of the tapered function.
 * If we ignore interference with side-lobes from the negative frequency
 * \f$-\omega_0\f$ and side-lobes of potential other harmonics at nearby
 * frequencies, we can approximate
 * \f[
 *   \tilde{g}(\omega_0)\approx\frac{A}{2}e^{i\phi}\tilde{w}(0)
 * \f]
 * and
 * \f[
 *   \left|\tilde{g}(\omega_0)\right|\approx
 *     \frac{A}{2}\left|\tilde{w}(0)\right|.
 * \f]
 *
 * \subsection subsec_foutra_scaling_harmonics_boxcar Boxcar taper function
 *
 * The boxcar taper is defined as
 * \f[
 *   w(t) = 
 *   \left\{
 *     \begin{array}{ll}
 *       1 & \textrm{if $|t|\leq T/2$ and}\\
 *       0 & \textrm{otherwise}
 *     \end{array}
 *   \right.
 * \f]
 * with
 * \f[
 *   \tilde{w}(\omega)
  *    = T \frac{\sin(\omega T/2)}{\omega T/2}
 * \f]
 * and
 * \f[
 *   w_{\textrm{max}}=w(0)=T.
 * \f]
 *
 * \subsection subsec_foutra_scaling_harmonics_hanning Hanning taper function
 *
 * The Hanning taper is defined as
 * \f[
 *   w(t) = 
 *   \left\{
 *     \begin{array}{ll}
 *       \cos^2(\pi t / T)) 
 *       = \frac{1}{2}\left[
 *           1 + \cos(2\pi t / T)
 *         \right]
 *       & \textrm{if $|t|\leq T/2$ and}\\
 *       0 & \textrm{otherwise}
 *     \end{array}
 *   \right.
 * \f]
 * with
 * \f[
 *   \tilde{w}(\omega)
 *    = T/2 \frac{\sin(\omega T/2)}{\omega T/2}
 *    + T/4 \frac{\sin(\omega T/2+\pi)}{\omega T/2+\pi}
 *    + T/4 \frac{\sin(\omega T/2-\pi)}{\omega T/2-\pi}
 * \f]
 * (see Blackman, R.B. and Tukey, J.W. 1958.
 *      The measurement of power spectra.
 *      Dover Publications, Inc., New York.
 *      Section B.5)
 * and
 * \f[
 *   w_{\textrm{max}}=w(0)=\frac{T}{2}.
 * \f]
 *
 * \subsection subsec_foutra_scaling_harmonics_tests Instant tests
 *
 * In the Makefile you will find a section with test code. 
 * This offers instantaneous testing of harmonic signal scaling in foutra.
 * \dontinclude Makefile
 * \skip # test foutra scaling for harmonic signals
 * \until #----------------------------------------------------------------------
 *
 * \date 10.01.2011 \author thof
 */

/* ----- END OF foutra.cc ----- */
