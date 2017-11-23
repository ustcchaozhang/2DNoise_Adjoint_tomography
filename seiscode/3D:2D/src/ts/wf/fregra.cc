/*! \file fregra.cc
 * \brief Spectrogram for unevenly sampled frequencies
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 05/07/2011
 * 
 * Spectrogram for unevenly sampled frequencies
 * 
 * Copyright (c) 2011 by Thomas Forbriger (BFO Schiltach) 
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
 * 
 * REVISIONS and CHANGES 
 *  - 05/07/2011   V1.0   Thomas Forbriger
 *  - 08/09/2011   V1.1   support format modifiers
 * 
 * ============================================================================
 */
#define FREGRA_VERSION \
  "FREGRA   V1.1   Spectrogram for unevenly sampled frequencies"

#include <iostream>
#include <complex>
#include <tfxx/commandline.h>
#include <aff/series.h>
#include <aff/seriesoperators.h>
#include <tsxx/tsxx.h>
#include <tsxx/tapers.h>
#include <fstream>
#include <tfxx/error.h>
#include <tfxx/rangestring.h>
#include <tfxx/xcmdline.h>
#include <tfxx/misc.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <sstream>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose, overwrite, debug, log, boxcar;
  std::string inputformat, outputformat;
  double fmin, fmax, olap, width;
  int nfreq;
}; // struct Options

// values type to be used for samples
typedef double Tvalue;

// time series
typedef aff::Series<Tvalue> Tseries;

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    FREGRA_VERSION "\n"
    "usage: fregra [-v] [-o] [-type type] [-Type type] [-D] [-boxcar]" "\n"
    "              [-fmin f] [-fmax f] [-n n] [-olap f]\n"
    "              [-width f] [-log]\n"
    "              outpattern infile [t:T] [infile [t:T] ...]" "\n"
    "   or: fregra --help|-h" "\n"
    "   or: fregra --xhelp" "\n"
    "\n"
    "NOTICE: fregra is not yet fully implemented!"
  };

  // define full help text
  char help_text[]=
  {
    "outpattern   name pattern for output file (may contain %N, which\n"
    "             will be replaced by sequential number).\n"
    "infile       input file (traces can be selected by the a list\n"
    "             definition like 3,5,8-12)\n"
    "\n"
    "-v           be verbose\n"
    "-o           overwrite existing output file\n"
    "-type type   select input file type\n"
    "-Type type   select output file type\n"
    "-DEBUG       produce debug output\n"
    "-boxcar      use boxcar window (default: Hanning)\n"
    "-fmin f      smallest frequency to be used\n"
    "-fmax f      largest frequency to be used\n"
    "-n n         number of frequencies to be used\n"
    "-olap f      overlap of time windows\n"
    "-width f     width of frequency resolution\n"
    "-log         use logarithmic frequency scale\n"
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
    {"DEBUG",arg_no,"-"},
    // 4: boxcar window
    {"boxcar",arg_no,"-"},
    // 5: smallest frequency
    {"fmin",arg_yes,"0.1"},
    // 6: largest frequency
    {"fmax",arg_yes,"1."},
    // 7: number of frequencies
    {"n",arg_yes,"10"},
    // 8: time window overlap
    {"olap",arg_yes,"0.5"},
    // 9: frequency width factor
    {"width",arg_yes,"2."},
    // 10: logarithmic frequency scale
    {"log",arg_no,"-"},
    // 11: input file type
    {"type",arg_yes,"sff"},
    // 12: output file type
    {"Type",arg_yes,"sff"},
    // 13: detailed help
    {"xhelp",arg_no,"-"},
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
  if (cmdline.optset(0) || cmdline.optset(13))
  {
    cerr << usage_text << endl;
    cerr << help_text << endl;
    datrw::supported_input_data_types(cerr);
    datrw::supported_output_data_types(cerr);
    if (cmdline.optset(13)) { datrw::online_help(cerr); }
    exit(0);
  }

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.overwrite=cmdline.optset(2);
  opt.debug=cmdline.optset(3);
  opt.boxcar=cmdline.optset(4);
  opt.fmin=cmdline.double_arg(5);
  opt.fmax=cmdline.double_arg(6);
  opt.nfreq=cmdline.int_arg(7);
  opt.olap=cmdline.double_arg(8);
  opt.width=cmdline.double_arg(9);
  opt.log=cmdline.optset(10);
  opt.inputformat=cmdline.optset(11);
  opt.outputformat=cmdline.optset(11);

  // check validity of command line parameters
  TFXX_assert((opt.fmin > 0), "illegal frequency value");
  TFXX_assert((opt.fmax > 0), "illegal frequency value");
  TFXX_assert((opt.olap > 0) && (opt.olap<1.), "illegal overlap value");
  TFXX_assert((opt.width > 0), "illegal width value");
  TFXX_assert((opt.nfreq > 0), "illegal number of frequencies");

  if (opt.verbose)
  { cout << FREGRA_VERSION << endl; }

  // extract commandline arguments
  TFXX_assert(cmdline.extra(), "missing output file pattern");
  std::string outpattern=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing input file");

  tfxx::cmdline::Tparsed arguments=parse_cmdline(cmdline, cmdlinekeys);
  if ((arguments.size()>1) && opt.verbose)
  {
    cout << "NOTICE: only the first trace will be processed!" <<
      endl;
  }

  /*======================================================================*/
  // create processing description
  sff::FREE processfree;

  ts::tapers::Hanning hanningtaper;

  // set up frequencies
  Tseries frequencies(0,opt.nfreq-1);
  if (opt.log)
  {
    double lfmin=std::log10(opt.fmin);
    double lfmax=std::log10(opt.fmax);
    double ldf=(lfmax-lfmin)/(static_cast<double>(opt.nfreq)-1);
    for (int i=0; i<opt.nfreq; ++i)
    {
      frequencies(i)=std::pow(10.,lfmin+i*ldf);
    }
  } // if (opt.log)
  else
  {
    double df=(opt.fmax-opt.fmin)/(static_cast<double>(opt.nfreq)-1);
    for (int i=0; i<opt.nfreq; ++i)
    {
      frequencies(i)=opt.fmin+i*df;
    }
  } // if (opt.log) else

  /*======================================================================*/
  // process one trace (currently the only one to be processed)

  unsigned int otrace=0;
  tfxx::cmdline::Tparsed::const_iterator infile=arguments.begin();
  while (infile != arguments.end())
  {
      
    // open input file
    if (opt.verbose) { cout << "open input file " << infile->name << endl; }
    std::ifstream ifs(infile->name.c_str(),
                      datrw::ianystream::openmode(opt.inputformat));
    datrw::ianystream is(ifs, opt.inputformat);

    // work on filoe header
    sff::FREE infilefree;
    if (is.hasfree()) { is >> infilefree; }
    sff::SRCE insrceline;
    if (is.hassrce()) { is >> insrceline; } 

    // cycle through traces of input file
    // ----------------------------------
    // setup trace selection
    unsigned int itrace=0;
    typedef tfxx::RangeList<int> Trangelist;
    bool doselect=infile->haskey(tracekey);
    Trangelist traceranges=
      tfxx::string::rangelist<Trangelist::Tvalue>(infile->value(tracekey));
    while (is.good())
    {
      ++itrace;
      if ((!doselect) || traceranges.contains(itrace))
      {
        ++otrace;

        TFXX_debug(opt.debug, "main", "process trace #" << itrace );

        if (opt.verbose)
        { std::cout << "  process trace #" << itrace << std::endl; }

        Tseries inputseries;
        is >> inputseries;

        sff::WID2 wid2;
        is >> wid2;

        ::sff::SRCE insrce;
        is >> insrce;

        ::sff::FREE inputtracefree;
        is >> inputtracefree;

        // a little helper
        std::ostringstream oss;

        // open output file
        // ----------------
        std::string outfile=outpattern;
        {
          oss.str("");
          oss << otrace;
          outfile=tfxx::string::patsubst(outfile, "%N", 
                                         tfxx::string::trimws(oss.str()));
        }
        if (opt.verbose) { cout << "open output file " << outfile << endl; }
        // check if output file exists and open
        if (!opt.overwrite)
        {
          std::ifstream file(outfile.c_str(),std::ios_base::in);
          TFXX_assert((!file.good()),"ERROR: output file exists!");
        }
        std::ofstream ofs(outfile.c_str(),
                          datrw::oanystream::openmode(opt.outputformat));
        datrw::oanystream os(ofs, opt.outputformat);

        // prepare and write file header
        ::sff::FREE outfilefree;
        oss.str("");
        oss << "trace #" << itrace << " from file " << infile->name;
        outfilefree.append(oss.str());
        outfilefree.append(processfree);
        os << outfilefree;
        ::sff::SRCE outsrce=insrce;
        outsrce.cx=0.;
        outsrce.cy=0.;
        outsrce.cz=0.;
        os << outsrce;
        
        // process all frequencies
        double df;
        for (int ifreq=0; ifreq<opt.nfreq; ++ifreq)
        {
 /*----------------------------------------------------------------------*/
 // actual processing for current trace
 
          const double pi=3.141592653589793115998;
          const double pi2=2.*pi;
 
          // define processing parameters
          if (ifreq<opt.nfreq-1)
          {
            df=frequencies(ifreq+1)-frequencies(ifreq);
          }
          
          double dt=wid2.dt;
          double fwidth=df*opt.width;
          double twin=1./(pi2*fwidth);
          int nwin=static_cast<int>(twin/(dt));
          TFXX_assert(nwin>0, "ERROR: invalid value!");
          int nstep=static_cast<int>(twin/(dt*opt.olap));
          if (nstep<1) { nstep=1; }

          // prepare output trace free
          ::sff::FREE tracefree;
          oss.str("");
          oss << "processing frequency #" << ifreq
            << " " << frequencies(ifreq) << " Hz";
          tracefree.append(oss.str());
          oss.str("");
          oss << "frequency resolution: " << fwidth << " Hz";
          tracefree.append(oss.str());
          oss.str("");
          oss << "Fourier transform window size: "
            << nwin << " samples, "
            << nwin*dt << " seconds";
          tracefree.append(oss.str());
          oss.str("");
          oss << "Fourier transform window shifting: "
            << nstep << " samples";
          tracefree.append(oss.str());
          tracefree.append(inputtracefree);

          // prepare output trace INFO line
          ::sff::INFO outinfo;
          outinfo.cs=sff::CS_cartesian;
          outinfo.cx=frequencies(ifreq);

          Tseries fregram(inputseries.shape());
          fregram=0.;

          typedef std::complex<double> Tcplx;
          typedef aff::Series<Tcplx> Tcoeff;
          unsigned int isize=static_cast<unsigned int>(2*nwin+1);
          Tcplx ime(0.,1.);
          Tcoeff ftfac(isize);
          Tcplx ftexpfac=ime*pi2*frequencies(ifreq)*dt;
          for (int i=ftfac.f(); i<=ftfac.l(); ++i)
          { ftfac(i)=std::exp(ftexpfac*static_cast<double>(i)); }
           
          // taper and scaling for PSD
          // scaling factor to adjust for taper effect
          double tapscaling=1.;
          if (opt.boxcar)
          {
            tapscaling=1.;
          }
          else
          {
            // scaling factor for Hanning
            // see foutra.cc for derivation of this factor
            tapscaling=8./3.;
            hanningtaper.apply(ftfac);
          }

          // we have an energy spectrum so far
          // adjust scaling factor to obtain signal power
          double scalingfactor=2.*tapscaling/(isize*dt);
          ftfac *= scalingfactor;

          // deploy factors
          Tseries depfac(0,2*nstep-1);
          for (int i=0; i<2*nstep; ++i)
          {
            depfac(i)=-0.5*(std::cos(pi2*i/(2*nstep-1))-1.);
          }

          // cycle through input series
          int isample=inputseries.first();
          while (isample <=inputseries.last())
          {

            std::complex<double> C(0.,0.);
            int imin=isample-nwin;
            int imax=isample+nwin;
            for (int i=imin; i<=imax; ++i)
            {
              if ((i>=inputseries.first()) && (i<=inputseries.last()))
              {
                // Fourier transform
                C += ftfac(i-imin-ftfac.f())*inputseries(i);
              } // if ((i>=inputseries.first()) && (i<=inputseries.last()))
            } // for (int i=isample-nwin; i<isample+nwin; ++i)
            double PSD=C.real()*C.real()+C.imag()*C.imag();

            // deploy results
            int ibase=isample-nstep;
            for (int i=0; i<2*nstep; ++i)
            {
              int j=ibase+i;
              if ((j>=fregram.f())&&(j<=fregram.l()))
              {
                fregram(j)+=depfac(i)*PSD;
              }
            }

            isample+=nstep;
          }

          os << fregram;
          os << wid2;
          os << outinfo;
          os << tracefree;

 /*----------------------------------------------------------------------*/
        } // while (Ifreq!=frequencies.end())^

      } // if ((!doselect) || traceranges.contains(itrace))
      else
      {
        if (opt.verbose)
        { std::cout << "  skipping trace #" << itrace << std::endl; }
      } // if ((!doselect) || traceranges.contains(itrace)) else
    } // while (is.good())
    ++infile;
  } // while (infile != arguments.end())

} // main

/* ----- END OF fregra.cc ----- */
