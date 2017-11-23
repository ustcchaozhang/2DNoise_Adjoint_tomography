/*! \file cross.cc
 * \brief cross correlation
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 22/11/2016
 * 
 * cross correlation
 * 
 * Copyright (c) 2005, 2016 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 26/01/2005   V1.0   Thomas Forbriger
 *  - 18/09/2015   V1.1   provide convolution as alternative operation
 *  - 16/11/2016   V1.2   provide new features:
 *                        - full libdatrwxx support
 *                        - trace selectors
 *  - 22/11/2016   V1.3   operate in the Fourier domain to speed up
 *                        computation for large time series
 * 
 * ============================================================================
 */
#define CROSS_VERSION \
  "CROSS   V1.3   cross correlation and convolution"

#include <fstream>
#include <iostream>
#include <list>
#include <string>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/error.h>
#include <tfxx/misc.h>
#include <tfxx/rangelist.h>
#include <tfxx/rangestring.h>
#include <tsioxx/sfftimeseries.h>
#include <tsioxx/inputoperators.h>
#include <tsioxx/outputoperators.h>
#include <tsxx/correlate.h>
#include <tsxx/convolve.h>
#include <datrwxx/writeany.h>
#include <datrwxx/readany.h>
#include <aff/seriesoperators.h>
#include <aff/subarray.h>
#include <aff/iterator.h>
#include <fourier/fftwaff.h>

using std::cout;
using std::cerr;
using std::endl;

typedef aff::Series<double> Tseries;
typedef ts::sff::SFFTimeSeries<Tseries> Tts;
typedef Tts::Tdttimeseries Tdttimeseries;

struct Options {
  bool verbose, overwrite, debug, convolve, fourier;
  std::string inputformat, outputformat;
}; // struct Options

/* ====================================================================== */

/*! Process time series with padding.
 *
 * Input time series must be appropriately zero padded. This means that the
 * series passed to the FFT processor is larger than the actual time series.
 * To avoid re-allocation of workspace, this class handles all operation in
 * one persistent workspace and supports access to Fourier coefficients for
 * different time series length.
 */
class FourierProcessor {
  public: 
    //! type of FFT processor
    typedef fourier::fft::DRFFTWAFF Tfft;
    //! type of time series data
    typedef Tfft::Tseries Tseries;
    //! type of Fourier coefficient data
    typedef Tfft::Tspectrum Tspectrum;
    //! full time series with sampling interval
    typedef ts::TDtimeseries Ttimeseries;
    //! full time series with sampling interval
    typedef Ttimeseries::Tconsttimeseries Tconsttimeseries;
    //! create object
    FourierProcessor(const bool& debug=false):
      Mspecavail(false), Minsize(0), Mdt(0), Mdebug(debug) { }
    //! create object by passing a time series
    FourierProcessor(const Tconsttimeseries& s,
                     const bool& debug=false):
      Mspecavail(false), Minsize(0), Mdt(0), Mdebug(debug)
    { this->setseries(s); }
    /*! create object by passing a time series and defining the desired length
     *  of padded series
     */
    FourierProcessor(const Tconsttimeseries& s, const unsigned int& n):
      Mspecavail(false), Minsize(0), Mdt(0) 
    { 
      this->setseries(s); 
      this->padseries(n); 
    }
    //! return Fourier coefficients for padded time series with n samples
    Tspectrum::Tcoc operator()(const unsigned int& n);
    //! return Fourier coefficients for time series s padded to n samples
    Tspectrum::Tcoc operator()(const Tconsttimeseries& s,
                               const unsigned int& n);
    //! set new input series
    void setseries(const Tconsttimeseries& s);
  private:
    //! pad input series and calculate Fourier coefficients
    void padseries(const unsigned int& n);
  private:
    //! FFT processor
    Tfft MFFT;
    //! workspace for input series must have at least desired size
    Tseries Mseries;
    //! series addressing part of workspace
    Tseries Mpadded;
    //! resulting Fourier coefficients
    Tspectrum Mspectrum;
    //! indicate available Fourier coefficients
    bool Mspecavail;
    //! size of original input series
    unsigned int Minsize;
    //! sampling interval
    double Mdt;
    //! debug flag
    bool Mdebug;
}; // class FourierProcessor

/* ---------------------------------------------------------------------- */

FourierProcessor::Tspectrum::Tcoc 
  FourierProcessor::operator()(const Tconsttimeseries& s,
                                             const unsigned int& n)
{
  TFXX_debug(this->Mdebug,
             "FourierProcessor::operator()(const Tconsttimeseries& s,\n"
             "                             const unsigned int& n)",
             TFXX_value(s.size()) << " " << TFXX_value(n));
  this->setseries(s);
  return(this->operator()(n));
} // FourierProcessor::Tspectrum::Tcoc
  //   FourierProcessor::operator()(const Tconsttimeseries& s,
  //                                              const unsigned int& n)

/* ---------------------------------------------------------------------- */

FourierProcessor::Tspectrum::Tcoc 
  FourierProcessor::operator()(const unsigned int& n)
{
  TFXX_debug(this->Mdebug,
             "FourierProcessor::operator()(const unsigned int& n)",
             TFXX_value(n));
  TFXX_assert(Minsize>0, "no input time series available");
  this->padseries(n);
  TFXX_assert(Mspecavail, "no Fourier coefficents available");
  return(Mspectrum);
} // FourierProcessor::Tspectrum::Tcoc 
  //   FourierProcessor::operator()(const unsigned int& n)

/* ---------------------------------------------------------------------- */

void FourierProcessor::setseries(const Tconsttimeseries& s)
{
  TFXX_debug(this->Mdebug,
             "FourierProcessor::setseries(const Tconsttimeseries& s)",
             TFXX_value(s.size()));
  Minsize=s.size();
  if (Minsize>Mseries.size())
  {
    Mseries=Tseries(s.size());
  }
  Mseries=0.;
  Mseries.copyin(s);
  Mdt=s.header.dt;
  TFXX_debug(this->Mdebug,
             "FourierProcessor::setseries(const Tconsttimeseries& s)",
             TFXX_value(s.header.dt));
  Mpadded=Mseries;
  Mspecavail=false;
} // void FourierProcessor::setseries(const Tconsttimeseries& s)

/* ---------------------------------------------------------------------- */

// pad series
// expects samples to be previously set through setseries
void FourierProcessor::padseries(const unsigned int& n)
{
  TFXX_debug(this->Mdebug,
             "FourierProcessor::padseries(const Tconsttimeseries& n)",
             TFXX_value(n));
  TFXX_assert(Minsize<=n, 
              "desired series size would truncate original series");
  bool refft=((Mpadded.size() != n) || (!Mspecavail));
  if (n>Mseries.size())
  {
    Mseries=Tseries(n);
    Mseries=0.;
    Mseries.copyin(Mpadded);
    refft=true;
  }
  if (refft)
  {
    Mpadded=aff::subarray(Mseries)(Mseries.f(),Mseries.f()+n-1);
    Mspectrum=MFFT(Mpadded, this->Mdt);
    Mspecavail=true;
  }
} // void FourierProcessor::padseries(const unsigned int& n)

/* ---------------------------------------------------------------------- */

FourierProcessor::Tspectrum 
  specconv(const FourierProcessor::Tspectrum::Tcoc & sref,
           const FourierProcessor::Tspectrum::Tcoc & ssig)
{
  TFXX_assert(sref.size() == ssig.size(),
              "inconsistent sets of coefficients");
  FourierProcessor::Tspectrum result(sref.size());
  result=FourierProcessor::Tspectrum::Tvalue(0.);
  aff::Browser<FourierProcessor::Tspectrum::Tcoc> BR(sref);
  aff::Browser<FourierProcessor::Tspectrum::Tcoc> BD(ssig);
  aff::Iterator<FourierProcessor::Tspectrum> IR(result);
  while (BR.valid() && BD.valid() && IR.valid())
  {
    (*IR) = ((*BR) * (*BD));
    ++IR;
    ++BR;
    ++BD;
  }
  return(result);
} // FourierProcessor::Tspectrum 
  //   specconv(const FourierProcessor::Tspectrum::Tcoc & sref,
  //            const FourierProcessor::Tspectrum::Tcoc & ssig)

/* ---------------------------------------------------------------------- */

FourierProcessor::Tspectrum 
  speccorr(const FourierProcessor::Tspectrum::Tcoc & sref,
           const FourierProcessor::Tspectrum::Tcoc & ssig)
{
  TFXX_assert(sref.size() == ssig.size(),
              "inconsistent sets of coefficients");
  // imaginary unit time pi (shift factor)
  const std::complex<double> expfac(0.,M_PI);
  FourierProcessor::Tspectrum result(sref.size());
  result=FourierProcessor::Tspectrum::Tvalue(0.);
  aff::Browser<FourierProcessor::Tspectrum::Tcoc> BR(sref);
  aff::Browser<FourierProcessor::Tspectrum::Tcoc> BD(ssig);
  aff::Iterator<FourierProcessor::Tspectrum> IR(result);
  int l=0;
  while (BR.valid() && BD.valid() && IR.valid())
  {
    // cross correlation in the Fourier domain and simultaneous time shift to
    // place origin of time scale in center of corresponding time series
    *IR = conj(*BR) * (*BD) * std::exp(double(l)*expfac);
    ++IR;
    ++BR;
    ++BD;
    ++l;
  }
  return(result);
} // FourierProcessor::Tspectrum 
  //   speccorr(const FourierProcessor::Tspectrum::Tcoc & sref,
  //            const FourierProcessor::Tspectrum::Tcoc & ssig)

/* ====================================================================== */

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    CROSS_VERSION "\n"
    "usage: cross [-v] [-o] [-D] [-convolve] [--itype t] [--otype f]\n"
    "             [-fourier]\n"
    "             file [f:F] [t:T] [file [f:F] [t:T] [...]] outfile\n"
    "   or: cross --help|-h" "\n"
    "   or: cross --xhelp" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v           be verbose" "\n"
    "-D           debug mode" "\n"
    "-o           overwrite existing output file" "\n"
    "-convolve    apply convolution rather than correlation\n"
    "-fourier     operate in the Fourier domain\n"
    "-itype t     file format for input files\n"
    "-otype t     file format for output files\n"
    "\n"
    "file ...     input file(s)" "\n"
    "             t:T select traces T, where T may be any range\n"
    "                 specification like '3-4' or '5,6,7-12,20'\n"
    "             f:F specifies an input file format differing from\n"
    "                 the format selected by \"--itype\"\n"
    "outfile      output file" "\n"
    "\n"
    "--xhelp      print details on supported I/O formats\n"
    "\n"
    "The first trace read will be taken as a reference. All other\n"
    "traces will be cross-correlated with the reference or convolved\n"
    "with the reference. Convolution and cross-correlation are understood\n"
    "as an approximation to the corresponding integral form. Consequently\n"
    "the sampling interval of time series must be identical and the\n"
    "result of the discrete operation will be scaled with the sampling\n"
    "interval." "\n"
    "\n"
    "Data time will be handled as follows:\n"
    "\n"
    "Convolution\n"
    "  The reference series is understood as the impulse response of a\n"
    "  filter which s to be applied to the other input series. If a\n"
    "  source date is given in the file of the reference trace, this will\n"
    "  be taken as the origin of time, allowing for acausal impulse\n"
    "  response. If it is missing, the first sample of the reference (filter\n"
    "  impulse response) is understood to be at lag=0s. The date of the\n"
    "  first sample of the output trace is set to account for possible\n"
    "  acausal parts of the filter response. Source definition is taken\n"
    "  only from the first of the input files and is passed unaltered to\n"
    "  the output.\n"
    "\n"
    "Cross-correlation\n"
    "  Each input trace is cross-correlated with the reference trace.\n"
    "  When calculating the lag time, absolute time of the first sample\n"
    "  of each of the traces is taken into account. Absolute time of the\n"
    "  output is set with respect to the source time. The source time is\n"
    "  taken from the file header of the reference trace. If a source time\n"
    "  is missing there, it will be set to a default value.\n"
    "\n"
    "If option -convolve is selected, all traces will be convolved with\n"
    "the reference trace.\n"
    "\n"
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
    // 3: debug mode
    {"D",arg_no,"-"},
    // 4: convolve rather than correlate
    {"convolve",arg_no,"-"},
    // 5: detailed information on I/O formats
    {"xhelp",arg_no,"-"},
    // 6: convolve rather than correlate
    {"itype",arg_yes,"sff"},
    // 7: convolve rather than correlate
    {"otype",arg_yes,"sff"},
    // 8: convolve rather than correlate
    {"fourier",arg_no,"-"},
    {NULL}
  };
    
  /*----------------------------------------------------------------------*/
  /* define file specific options
   */
    
  static const char formatkey[]="f";
  static const char tracekey[]="t";
    
  // define commandline argument modifier keys
  static const char* cmdlinekeys[]
    ={formatkey, tracekey, 0};
 
  /*----------------------------------------------------------------------*/

  // no arguments? print usage...
  if (iargc<2) 
  {
    cerr << usage_text << endl;
    exit(0);
  }

  // collect options from commandline
  Commandline cmdline(iargc, argv, options);

  // help requested? print full help text...
  if (cmdline.optset(0) || cmdline.optset(5))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    cerr << endl;
    datrw::supported_data_types(cerr);
    cerr << endl;
    if (cmdline.optset(5))
    {
      cerr << endl;
      datrw::online_help(cerr);
    }
    exit(0);
  }

  /* ---------------------------------------------------------------------- */
  // evaluate command line parameters
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.debug=cmdline.optset(3);
  opt.convolve=cmdline.optset(4);
  opt.inputformat=cmdline.string_arg(6);
  opt.outputformat=cmdline.string_arg(7);
  opt.fourier=cmdline.optset(8);

  if (opt.verbose) { cout << CROSS_VERSION << endl; }

  TFXX_assert(cmdline.extra(), "missing input file!");
  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);
  TFXX_assert(arguments.size()>1, "missing output file!");
  // extract output file name and remove from list
  std::string outfile=arguments.back().name;
  arguments.pop_back();

  if ((arguments.size()>1) && opt.verbose)
  {
    cout 
      << "NOTICE: file specific information (SRCE line and file FREE)" << endl
      << "        will be taken from first file only!" << endl;
  }

  /* ---------------------------------------------------------------------- */
  // create Fourier workspace
  FourierProcessor FFTreference(opt.debug), FFTdata(opt.debug);
  // create FFT processor for transformation to time domain
  FourierProcessor::Tfft FFT;

  /* ---------------------------------------------------------------------- */
  // open output file
  // ----------------
  if (opt.verbose) { cout << "open output file " << outfile << endl; }
  // check if output file exists and open
  if (!opt.overwrite) { datrw::abort_if_exists(outfile); }
  std::ios_base::openmode oopenmode
    =datrw::oanystream::openmode(opt.outputformat);
  std::ofstream ofs(outfile.c_str(), oopenmode);
  TFXX_debug(opt.debug, "main()", "open os");
  datrw::oanystream os(ofs, opt.outputformat, opt.debug);
  TFXX_debug(opt.debug, "main()", "os is open");

  /*
   * the first trace to be read has to be stored as the reference trace
   */
  Tts reference;
  bool referenceread=false;

  // file header information for reference trace
  ts::sff::FileHeader referencefileheader;

  // cycle through list of file names found on command line
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile!=arguments.end())
  {
    if (opt.verbose) { cout << "open " << std::string(infile->name) << endl; }
    std::string inputformat=opt.inputformat;
    if (infile->haskey(formatkey)) 
    {
      inputformat=infile->value(formatkey);
    }
    std::ios_base::openmode iopenmode
      =datrw::ianystream::openmode(inputformat);
    std::ifstream ifs(infile->name.c_str(), iopenmode);
    datrw::ianystream is(ifs, inputformat);
      
    // handle file header
    // ------------------
    ts::sff::FileHeader fileheader;
    is >> fileheader;

    // cycle through traces of input file
    // ----------------------------------
    // setup trace selection
    typedef tfxx::RangeList<int> Trangelist;
    bool doselect=infile->haskey(tracekey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(tracekey));

    int iftrace=0;
    while (is.good())
    {
      ++iftrace;
      if (doselect && (!traceranges.contains(iftrace)))
      {
        if (opt.verbose) 
        { std::cout << "     skip trace #" << iftrace << std::endl; }
        is.skipseries();
      }
      else
      {
        if (opt.verbose) { std::cout << "process trace #" << iftrace << ": "; }
        if (!referenceread)
        {
          // handle file header of reference
          // -------------------------------
          // set a SRCE definition if not present in input and mode is
          // cross-correlation
          sff::FREE freeblock(fileheader.free());
          freeblock.append(CROSS_VERSION);
          // SRCE date is a reference time for cross-correlograms
          if (!opt.convolve)
          {
            if (!fileheader.hassrce())
            {
              sff::SRCE srce;
              srce.date=libtime::TAbsoluteTime(2000);
              fileheader.srce(srce);
              freeblock.append("added SRCE definition to file header");
            }
          }
          fileheader.free(freeblock);
          os << fileheader;
          referencefileheader=fileheader;

          // read actual reference time series
          // ---------------------------------
          if (opt.verbose) 
          { std::cout << "read reference trace" << std::endl; }
          is >> reference;
          referenceread=true;
          if (opt.verbose) { cout << reference.header.wid2().line() << endl; }
          if (opt.fourier) 
          { 
            TFXX_debug(opt.debug,
                       "main()", "prepare FFTreference "
                       << TFXX_value(reference.header.wid2().dt));
            Tts::Tdttimeseries dtreference(reference);
            TFXX_debug(opt.debug,
                       "main()", TFXX_value(dtreference.header.dt));
            FFTreference.setseries(dtreference); 
          }
        }
        else
        {
          if (opt.verbose) { cout << "read trace" << endl; }
          Tts data;
          is >> data;
          if (opt.verbose) { cout << data.header.wid2().line() << endl; }
          TFXX_assert((data.header.wid2().dt == reference.header.wid2().dt),
                      "sampling intervals do not match!");
          Tts result(data);
          sff::WID2 wid2line(data.header.wid2());
          if (opt.fourier) 
          { 
            TFXX_debug(opt.debug,
                       "main()", "prepare FFTdata");
            Tts::Tdttimeseries dtdata(data);
            FFTdata.setseries(dtdata); 
          }
          if (opt.convolve)
          {
            if (opt.fourier)
            {
              TFXX_debug(opt.debug,
                         "main()", "start convolution in the Fourier domain");
              unsigned int n=data.size()+reference.size();
              TFXX_debug(opt.debug,
                         "main()", "padded size: " << TFXX_value(n));
              FourierProcessor::Tspectrum::Tcoc sdata=FFTdata(n);
              FourierProcessor::Tspectrum::Tcoc sreference=FFTreference(n);
              TFXX_debug(opt.debug,
                         "main()", "call specconv "
                         << TFXX_value(sreference.size()) << ", "
                         << TFXX_value(sdata.size()));
              FourierProcessor::Tspectrum 
                sresult=specconv(sreference, sdata);
              TFXX_debug(opt.debug,
                         "main()", "returned: " << TFXX_value(sresult.size()));
              result=FFT(sresult, reference.header.wid2().dt);
            }
            else
            {
              result=ts::convolve(reference,data);
              result *= wid2line.dt;
            } // else, if (opt.fourier)
            wid2line.auxid="conv";
            // set time of first sample
            // ------------------------
            if (referencefileheader.hasfree())
            {
              // offset of impulse response
              libtime::TRelativeTime t0(reference.header.wid2().date
                                        -referencefileheader.srce().date);
              if (reference.header.wid2().date
                  <referencefileheader.srce().date)
              {
                wid2line.date -= t0;
              }
              else
              {
                wid2line.date += t0;
              }
            }
          } else {
            if (opt.fourier)
            {
              unsigned int n=data.size()+reference.size();
              FourierProcessor::Tspectrum::Tcoc sdata=FFTdata(n);
              FourierProcessor::Tspectrum::Tcoc sreference=FFTreference(n);
              FourierProcessor::Tspectrum 
                sresult=speccorr(sreference, sdata);
              result=FFT(sresult, reference.header.wid2().dt);
            }
            else
            {
              result=ts::correlate(reference,data);
              result *= wid2line.dt;
            } // else, if (opt.fourier)
            wid2line.auxid="corr";
            /*
             * Time delay of current signal with respect to first sample of
             * reference signal to be understood as a signed value:
             *
             *   tor = signal.date-ref.date
             *
             * Duration of reference signal: Tr
             * Duration of current signal: Ts
             *
             * Duration of cross-correlogram: Tr+Ts
             *
             * lag time of first sample:
             *
             * tl1=tor-(Tr+Ts)/2
             */
            libtime::TRelativeTime
              Tr(libtime::double2time(reference.header.wid2().dt
                                      *(reference.header.wid2().nsamples-1)));
            libtime::TRelativeTime
              Ts(libtime::double2time(data.header.wid2().dt
                                      *(data.header.wid2().nsamples-1)));
            libtime::TRelativeTime tor(data.header.wid2().date
                                       -reference.header.wid2().date);
            libtime::TAbsoluteTime
              torigin=referencefileheader.srce().date-((Tr+Ts)/2);
            if (data.header.wid2().date
                >=reference.header.wid2().date)
            {
              torigin += tor;
            }
            else
            {
              torigin -= tor;
            }
            wid2line.date=torigin;
          }
          result.header.wid2(wid2line);
          /*
          result.header.wid2().date=fileheader.srce.date
             +(result.series.first()
               *libtime::double2time(result.header.wid2().dt))
             +(data.header.wid2().date-reference.header.wid2().date);
          */
          if (opt.verbose) { cout << result.header.wid2().line() << endl; }
          os << result;
        } // else; if (!referenceread)
      } // else; if (doselect && (!traceranges.contains(itrace)))
    }
    ++infile;
  }
}

/* ----- END OF cross.cc ----- */
