/*! \file noisymize.cc
 * \brief program reads a set of SFF traces, convolves them with white noise and stacks
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 27/06/2006
 * 
 * program reads a set of SFF traces, convolves them with white noise and stacks
 * 
 * Copyright (c) 2006 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 27/06/2006   V1.0   Thomas Forbriger
 *  - 11/07/2006   V1.1   keep result of full convolution length
 *  - 26/02/2007   V1.2   report factors
 *  - 02/03/2007   V1.3   scale with offset
 *                 V1.4   special scaling for horizontal component
 *  - 08/03/2007   V1.5   
 *                        - allow switching of offset specific scaling
 *                        - added description in help section
 *                        - improved out format of list of factors
 *  - 07/09/2007   V1.6   corrected horizontal component scaling factor
 * 
 * ============================================================================
 */
#define NOISYMIZE_VERSION \
  "NOISYMIZE   V1.6   program reads a set of SFF traces,\n" \
  "                   convolves them with white noise and stacks"

#include <fstream>
#include <iostream>
#include <string>
#include <cmath>
#include <tfxx/commandline.h>
#include <tfxx/error.h>
#include <aff/series.h>
#include <aff/seriesoperators.h>
#include <tsxx/tsxx.h>
#include <tsxx/convolve.h>
#include <tsioxx/inputoperators.h>
#include <tsxx/random.h>
#include <datrwxx/readany.h>
#include <datrwxx/sff.h>

using std::cout;
using std::cerr;
using std::endl;

// put all commandline settings into a struct
struct Options {
  bool verbose, setnoiselength, overwrite, reportfactors;
  bool noscaling;
  int noiselength;
  double offexp;
  std::string inputformat;
}; // struct Options

// put all filenames in one struct
struct Filenames {
  std::string Zin;
  std::string Rin;
  std::string Zout;
  std::string Rout;
}; // struct Filenames

// values type to be used for samples
typedef double Tvalue;

// time series
typedef aff::Series<Tvalue> Tseries;

// full featured time series file
typedef ts::sff::File<Tseries> Tfile;

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    NOISYMIZE_VERSION "\n"
    "usage: noisymize Zin Rin Zout Rout" "\n"
    "                 [-v] [-type t] [-n n] [-o] [-rf] [-s e]" "\n"
    "                 [-fixed]" "\n"
    "   or: noisymize --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "Zin          vertical component input data" "\n"
    "Rin          radial component input data" "\n"
    "Zout         vertical component output data" "\n"
    "Rout         radial component output data" "\n"
    "\n"
    "-v           be verbose" "\n"
    "-type t      input file type" "\n"
    "-n n         number of noise samples to use" "\n"
    "-o           overwrite output" "\n"
    "-rf          report factors" "\n"
    "-s e         scale seismograms with offset" "\n"
    "             the scaling factor is r**e, where r is the offset" "\n"
    "-fixed       do not scale each offset with a randomly" "\n"
    "             chosen amplitude" "\n"
    "\n"
    "This program was specifically created to test procedures for\n"
    "MHVSR (microseismic H/V spectral ratio) analysis by application\n"
    "to synthetic data. Synthetic seismograms for vertical and radial\n"
    "component of the receiver calculated for a transient source signal\n"
    "have to be convolved with the same random noise in this case\n"
    "\n"
    "Description of the procedure:" "\n"
    "- the program reads pairs of seismograms from Zin and Rin" "\n"
    "- each of the input files may contain multiple traces" "\n"
    "- for each trace in Zin there must be a corresponding trace" "\n"
    "  in Rin at the same offset" "\n"
    "- for each offset the program does the following:" "\n"
    "  1. the Z-trace and the R-trace are convolved with the same" "\n"
    "     stochastic time series (random normaly distributed white noise)" "\n"
    "     a new stochastic time series is calculated for each offset" "\n"
    "     through the libgsl random number generator" "\n"
    "  2. the Z-trace and the R-trace are scaled by a random value" "\n"
    "     (this function can be switched off through option -fixed)" "\n"
    "  3. the Z-trace and the R-trace are scaled by an offset" "\n"
    "     dependend value (selected through option -s e)" "\n"
    "  4. the R-trace is scaled by 0.707 to simulate a uniform" "\n"
    "     distribution of sources of all azimuths; as a consequence" "\n"
    "     the R result my be used as N- and E-component as well" "\n"
    "- the results for all offsets are stacked; the resulting Z-" "\n"
    "  and R-series are written to files Zout and Rout," "\n"
    "  respectively"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: input format
    {"type",arg_yes,"sff"},
    // 3: number of noise samples
    {"n",arg_yes,"1"},
    // 4: overwrite output file
    {"o",arg_no,"-"},
    // 5: report factors
    {"rf",arg_no,"-"},
    // 6: report factors
    {"s",arg_yes,"0."},
    // 7: report factors
    {"fixed",arg_no,"-"},
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
  // read command line settings

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.inputformat=cmdline.string_arg(2);
  opt.setnoiselength=cmdline.optset(3);
  opt.noiselength=cmdline.int_arg(3);
  opt.overwrite=cmdline.optset(4);
  opt.reportfactors=cmdline.optset(5);
  opt.offexp=cmdline.double_arg(6);
  opt.noscaling=cmdline.optset(7);

  Filenames filename;
  TFXX_assert(cmdline.extra(), "ERROR: missing filename Zin!");
  filename.Zin=cmdline.next();
  TFXX_assert(cmdline.extra(), "ERROR: missing filename Rin!");
  filename.Rin=cmdline.next();
  TFXX_assert(cmdline.extra(), "ERROR: missing filename Zout!");
  filename.Zout=cmdline.next();
  TFXX_assert(cmdline.extra(), "ERROR: missing filename Rout!");
  filename.Rout=cmdline.next();

  /*----------------------------------------------------------------------*/
  // read vertical component input data
  if (opt.verbose)
  { cout << "read input file " << filename.Zin << endl; }
  Tfile Zindata;
  {
    std::ifstream ifs(filename.Zin.c_str());
    datrw::ianystream is(ifs, opt.inputformat);
    Zindata.read(is.idatstream(), opt.verbose); 
  }
  // read radial component input data
  if (opt.verbose)
  { cout << "read input file " << filename.Rin << endl; }
  Tfile Rindata;
  {
    std::ifstream ifs(filename.Rin.c_str());
    datrw::ianystream is(ifs, opt.inputformat);
    Rindata.read(is.idatstream(), opt.verbose); 
  }

  /*----------------------------------------------------------------------*/
  // consistency checks
  // ------------------
  //
  if (opt.verbose) { cout << "consistency checks:" << endl; }

  // check number of traces
  TFXX_assert(Zindata.size()==Rindata.size(),
              "ERROR: inconsitent number of traces");
  if (opt.verbose) 
  { cout << "input files both have " << Zindata.size() << " traces." << endl; }

  // check trace headers
  Tfile::Tbase::const_iterator Zit=Zindata.begin();
  Tfile::Tbase::const_iterator Rit=Rindata.begin();
  sff::WID2compare compare(sff::Fnsamples | sff::Fdt);
  while((Zit!=Zindata.end()) && (Rit!=Rindata.end()))
  {
    TFXX_assert(compare(Zit->header.wid2(), Rit->header.wid2()),
                "ERROR: inconsitent trace headers");
    if (Zit->header.hasinfo())
    {
      sff::INFO Zinfo=Zit->header.info();
      sff::INFO Rinfo=Rit->header.info();
      TFXX_assert(Zinfo==Rinfo,
                  "ERROR: inconsitent coordinates");
    }
    ++Zit;
    ++Rit;
  }
  if (opt.verbose) 
  { cout << "trace headers and coordinates are consistent" << endl; }

  /*----------------------------------------------------------------------*/
  // go and noisymize data
  // ---------------------
  //
  Zit=Zindata.begin();
  Rit=Rindata.begin();
  // source coordinates
  sff::SRCE Zsrce=Zindata.fileheader.srce();
  sff::SRCE Rsrce=Rindata.fileheader.srce();
  // results
  Tseries Zseries, Rseries;
  // scaling factors
  Tseries fac=ts::rnd::dugauss(Zindata.size());
  if (opt.noscaling) { fac=1.; }
  Tseries offfac(Zindata.size());
  Tseries traceoffset(Zindata.size());
  //
  if (!opt.setnoiselength)
  {
    opt.noiselength=Zit->size();
  }
  if (opt.verbose) 
  { 
    cout << "convolve " << Zit->size() 
      << " data samples (each trace) with" << endl
      << opt.noiselength << " samples of uniform gaussian noise" 
      << " and stack." << endl; 
  }
  bool initialize_result=true;
  unsigned int itrace=0;
  while((Zit!=Zindata.end()) && (Rit!=Rindata.end()))
  {
    // calculate offset and offset dependend scaling
    sff::INFO Zinfo=Zit->header.info();
    sff::INFO Rinfo=Rit->header.info();
    double Zoffset=sff::offset(Zsrce, Zinfo);
    double Roffset=sff::offset(Rsrce, Rinfo);
    double offerr=(Zoffset/Roffset)-1.;
    if (offerr < 0.) { offerr=-offerr; }
    TFXX_assert((offerr < 1.e-4), 
                "ERROR: offset of R- and Z-component are inconsistent!");
    traceoffset(itrace)=Zoffset;
    offfac(itrace)=std::pow(Zoffset, opt.offexp);
    // scaling factor for radial component, since we see only the projection
    // of the radial component of all sources on the respective horizontal
    // component
    // 7.9.2007: scaling factor changed appropriately for signal power (see
    //           help output of gresynoise)
    // Hfactor = sqrt( (int_0^{2*pi} cos(phi)**2 d phi) / (2 pi) )
    // formerly: const double Hfactor=2./M_PI;
    const double Hfactor=0.707; // = 1/sqrt(2)
    // go
    double scalefac=fac(itrace)*offfac(itrace);
    if (opt.verbose) { cout << "*"; cout.flush(); }
    Tseries noise=ts::rnd::dugauss(opt.noiselength);
    if (opt.verbose) { cout << "+"; cout.flush(); }
    if (initialize_result)
    { Zseries = scalefac*ts::convolve(*Zit, noise); }
    else
    { Zseries += scalefac*ts::convolve(*Zit, noise); }
    if (opt.verbose) { cout << "+"; cout.flush(); }
    if (initialize_result)
    { Rseries = Hfactor*scalefac*ts::convolve(*Rit, noise); }
    else
    { Rseries += Hfactor*scalefac*ts::convolve(*Rit, noise); }
    initialize_result=false;
    ++Zit;
    ++Rit;
    ++itrace;
  }
  if (opt.verbose) { cout << endl; }
  if (opt.reportfactors)
  {
    cout << "factors used for scaling:" << endl;
    cout.width(5);
    cout << "#";
    cout.width(12);
    cout << "f1";
    cout.width(12);
    cout << "f2";
    cout.width(14);
    cout << "offset" << endl;
    for (itrace=0; itrace<Zindata.size(); itrace++)
    {
      cout.width(5); cout.precision(3);
      cout << itrace;
      cout.width(12); cout.precision(4);
      cout << fac(itrace);
      cout.width(12); cout.precision(4);
      cout << offfac(itrace);
      cout.width(12); cout.precision(4);
      cout << traceoffset(itrace)*1.e-3 << "km";
      cout << endl;
    }
    cout << "traces are scale by f1*f2" << endl;
  }

  /*----------------------------------------------------------------------*/
  // write result
  if (opt.verbose) 
  { cout << "prepare output data" << endl; }
  typedef ts::sff::SFFTimeSeries<Tseries> Tsffseries;
  Tsffseries::Theader Zheader(Zindata[0].header);
  Tsffseries::Theader Rheader(Rindata[0].header);

  sff::WID2 wid2line=Zheader.wid2();
  wid2line.nsamples=Zseries.size();
  wid2line.channel="Z";
  Zheader.wid2(wid2line);
  Tsffseries Zoutdata(Zseries, Zheader);;

  wid2line=Rheader.wid2();
  wid2line.nsamples=Rseries.size();
  wid2line.channel="R";
  Rheader.wid2(wid2line);
  Tsffseries Routdata(Rseries, Rheader);;

  {
    if (opt.verbose) { cout << "open output file " << filename.Zout << endl; }
    // check if output file exists and open
    if (!opt.overwrite)
    {
      std::ifstream file(filename.Zout.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    std::ofstream ofs(filename.Zout.c_str());
    datrw::osffstream os(ofs);

    // prepare file FREE block
    sff::FREE filefree;
    filefree.append(NOISYMIZE_VERSION);

    os << Zoutdata;
  }

  {
    if (opt.verbose) { cout << "open output file " << filename.Rout << endl; }
    // check if output file exists and open
    if (!opt.overwrite)
    {
      std::ifstream file(filename.Rout.c_str(),std::ios_base::in);
      TFXX_assert((!file.good()),"ERROR: output file exists!");
    }
    std::ofstream ofs(filename.Rout.c_str());
    datrw::osffstream os(ofs);

    // prepare file FREE block
    sff::FREE filefree;
    filefree.append(NOISYMIZE_VERSION);

    os << Routdata;
  }
}

/* ----- END OF noisymize.cc ----- */
