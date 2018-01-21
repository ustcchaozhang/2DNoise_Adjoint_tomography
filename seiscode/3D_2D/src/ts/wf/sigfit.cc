/*! \file sigfit.cc
 * \brief fit signal by trial-signals
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 28/01/2004
 * 
 * fit signal by trial-signals
 * 
 * Copyright (c) 2004 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * SIGFIT is free software; you can redistribute it and/or modify
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
 *  - 28/01/2004   V1.0   Thomas Forbriger
 *  - 24/02/2004   V1.1   introduced search range stabilization
 *                        more output values
 *                 V1.2   correction: searchfac is a factor!
 *  - 16/03/2004   V1.3   add trend and offset fit
 *  - 24/09/2004   V1.4   
 *                        - allow to skip samples if this is helpful to remove
 *                          transient filter loading response
 *                        - remove correction from synthetics and signal
 *                          and make this process transparent
 *                        - provide exponential decay
 *  - 02/08/2007   V1.5   explain results line
 *  - 11/02/2011   V1.6   add definition of traces
 *  - 20/02/2012   V1.7   read and write any file format (damb)
 *                        trace selecttion requires skip trace (thof)
 *  - 22/11/2016   V1.8   make use of new output facilities in libtsxx
 * 
 * ============================================================================
 */
#define SIGFIT_VERSION \
  "SIGFIT   V1.8   fit signal by trial-signals"

#include <fstream>
#include <iostream>
#include <list>
#include <vector>
#include <cmath>
#include <tfxx/commandline.h>
#include <tfxx/xcmdline.h>
#include <tfxx/rangelist.h>
#include <tfxx/rangestring.h>
#include <tfxx/error.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <tsxx/tsxx.h>
#include <tsxx/innerproduct.h>
#include <tsxx/wid2timeseries.h>
#include <tsxx/wid2tsio.h>
#include <aff/array.h>
#include <aff/dump.h>
#include <aff/seriesoperators.h>
#include <linearxx/lapackxx.h>

// double precision time series bundle which includes a WID2 header
typedef ts::TDsfftimeseries Tbundle;
typedef datrw::Tdseries Tseries;
typedef std::vector<Tbundle> Tbundlevec;
typedef aff::Array<Tbundle::Tseries::Tvalue> Tmatrix;

using std::cout;
using std::cerr;
using std::endl;

// structure to store commandline options
struct Options {
  bool verbose;
  double Tdate;
  bool truncate;
  std::string residualname;
  bool writeresidual;
  double searchrange;
  bool usesearchrange;
  bool equalsearch;
  bool fittrend, fitoffset, doskip, fitexp;
  double amptrend, ampoffset, skip, tcexp;
  std::string itype, otype;
}; // struct Options

/*======================================================================*/
/* functions */

// remove average
void removeavg(Tseries& s)
{
  double avg=0.;
  for (int i=s.first(); i<=s.last(); ++i) { avg += s(i); }
  avg /= s.size();
  for (int i=s.first(); i<=s.last(); ++i) { s(i) -= avg; }
}

// remove trend
void removetre(Tseries& s)
{
  TFXX_abort("removetre does not work");
  double sum=0.;
  double jser=0.;
  double jqser=0.;
  for (int i=s.first(); i<=s.last(); ++i) 
  { 
    sum += s(i); 
    jser += i;
    jqser += i*i;
  }
  //double a,b;
  sum /= s.size();
  for (int i=s.first(); i<=s.last(); ++i) { s(i) -= sum; }
}

// formatted number output
std::string formatfloat(const double &v)
{
  char seq[20];
  std::sprintf(seq, "%8.5f ", v);
  return(std::string(seq));
}

/*======================================================================*/
/* main progran */

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    SIGFIT_VERSION "\n"
    "usage: sigfit [-v] [-Tdate v] [-truncate]" "\n"
    "              [-Sramp[=v]] [-Sconst[=v]] [-Sexp[=v]]" "\n"
    "              [-residual f]" "\n"
    "              [-searchrange[=r]] [-equalsearch] [-skip n]" "\n"
    "              [-itype F] [-otype F]" "\n"
    "              signal [t:T f:F] trial [t:T f:F] [trial [t:T f:F]...]" "\n"
    "   or: sigfit --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-v           be verbose" "\n"
    "-Sramp[=v]   add a ramp to the set of trial signals with amplitude v" "\n"
    "-Sconst[=v]  add a constant (of value v) to the set of trial signals" "\n"
    "-Sexp[=v]    add a exponential decay (with time constant v relative" "\n"
    "             to the length of the time series) to the set of trial" "\n"
    "             signals" "\n"
    "-Tdate v     tolerance for comparison of date of first sample" "\n"
    "             v give the tolerance in units of the sampling interval" "\n"
    "-truncate    truncate all series to the same number of samples" "\n"
    "-residual f  write waveforms containing residual to file \"f\"" "\n"   
    "-searchrange[=r] set search range to stabilize the fit in cases" "\n"
    "             where a trial series does not (or almost not)" "\n"
    "             contribute to the signal" "\n"
    "             the search range r is given in units of" "\n"
    "             rms(signal)/rms(trial signals)" "\n"
    "-equalsearch use same search range for each trial signal" "\n"
    "-skip n      skip n seconds at beginning of each trace" "\n"
    "-itype F     set input file formats to F (default: sff)" "\n"
    "-otype F     set output (residual) file format to F (default: sff)" "\n"
    "\n"
    "signal       signal to by explained by a linear combination" "\n"
    "             of trial signals" "\n"
    "trial        any set of trial signals" "\n"
    "\n"
    "File specific options:" "\n"
    "t:T          select specific traces from input file" "\n"
    "             T can be a list of traces like '1,4,5' or" "\n"
    "             a range like '6-19' or mixed like '5,8,12-17,20'" "\n"
    "f:F          specific file format (overrides -itype setting)" "\n"
    "\n"
    "The last line of the output is a summary of all results." "\n"
    "This line contains the following information separated by blanks:" "\n"
    "  channel station instrument julian_day date time" "\n"
    "  explained_rms signal_rms residual_rms" "\n"
    "  for each trial signal: regression_coefficient" "\n"
    "  rms_of_all_coefficients" "\n"
    "  for each trial signal: normalized_regression_coefficient" "\n"
    "  for each trial signal: regression_coefficient*trial_signal_rms" "\n"
    "  for each trial signal:" "\n"
    "    normalized_scalar_product_between_trial_signal_and_signal" "\n"
    "\n"
    "The output file (residual argument on the command line) will contain\n"
    "several traces (N test signals including DC offset and linear trend):\n"
    "  trace #1              signal, channel name as in input file\n"
    "  trace #2              \"syn\", synthetic trace, superposition of all\n"
    "                        test signals\n"
    "  trace #3              \"dif\", residual dif=signal-syn\n"
    "  traces #4 - #4+N-1    raw test signals\n"
    "  traces #4+N - #4+2N-1 scaled test signals in units of the final\n"
    "                        displaying their contribution to \"syn\"\n"
    "  trace #4+2N           \"csi\": csi=signal-cor\n"
    "  trace #4+2N+1         \"cys\": csy=syn-cor\n"
    "  trace #4+2N+2         \"cor\" internal corretion, i.e. the\n"
    "                        contributions of the DC offset and the linear\n"
    "                        trend to \"syn\"\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: date tolerance
    {"Tdate",arg_yes,"0."},
    // 3: truncate to a common number of samples
    {"truncate",arg_no,"-"},
    // 4: output SFF file
    {"residual",arg_yes,"-"},
    // 5: search range
    {"searchrange",arg_opt,"100."},
    // 6: equal search ranges
    {"equalsearch",arg_no,"-"},
    // 7: fit a trend
    {"Sramp",arg_opt,"1."},
    // 8: fit an offset
    {"Sconst",arg_opt,"1."},
    // 9: fit an offset
    {"skip",arg_yes,"0"},
    // 10: fit an offset
    {"Sexp",arg_opt,"1."},
    // 11: input file format
    {"itype",arg_yes,"sff"},
    // 12: output file format
    {"otype",arg_yes,"sff"},
    {NULL}
  };

  //! key to select traces
  const char* const tracekey="t";
  //! key to select file format
  const char* const formatkey="f";
  //! list of keys for filename specific parameters
  static const char* keys[]={
    //! select traces
    tracekey,
    //! set file format
    formatkey,
    0
  }; // const char* keys[]

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
    cout << usage_text << endl;
    cout << help_text << endl;
    exit(0);
  }

  // read options
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.Tdate=cmdline.double_arg(2);
  opt.truncate=cmdline.optset(3);
  opt.writeresidual=cmdline.optset(4);
  opt.residualname=cmdline.string_arg(4);
  opt.usesearchrange=cmdline.optset(5);
  opt.searchrange=cmdline.double_arg(5);
  opt.equalsearch=cmdline.optset(6);
  opt.fittrend=cmdline.optset(7);
  opt.fitoffset=cmdline.optset(8);
  opt.amptrend=cmdline.double_arg(7);
  opt.ampoffset=cmdline.double_arg(8);
  opt.skip=cmdline.double_arg(9);
  opt.doskip=cmdline.optset(9);
  opt.fitexp=cmdline.optset(10);
  opt.tcexp=cmdline.double_arg(10);
  opt.itype=cmdline.string_arg(11);
  opt.otype=cmdline.string_arg(12);

  /*----------------------------------------------------------------------*/

  // read file names
  TFXX_assert(cmdline.extra(),"ERROR: missing signal file name!");
  //std::string signalname=cmdline.next();
  tfxx::cmdline::Tparsed infiles(tfxx::cmdline::parse_cmdline(cmdline, keys));
  //TFXX_assert((cmdline.extra()||opt.fittrend||opt.fitoffset),
  //            "ERROR: missing trial signal file name!");
  //Tnamelist namelist;
  //while (cmdline.extra()) { namelist.push_back(cmdline.next()); }

  // read signal file
  tfxx::cmdline::Tparsed::const_iterator infile(infiles.begin());
  TFXX_assert((infile!=infiles.end()||opt.fittrend||opt.fitoffset),
    "ERROR (sigfit): missing trial signal file name");
  std::string tracelist("1");
  if (infile->haskey(tracekey)) { tracelist=infile->value(tracekey); }
  tfxx::RangeListStepper<int> rls(tfxx::string::rangelist<int>(tracelist));
  TFXX_assert(rls.valid(), "Illegal tracelist");
  std::string signalname(infile->name);
  int signaltrace = rls.current();
  std::string signalformat(opt.itype);
  if (infile->haskey(formatkey)) { signalformat=infile->value(formatkey); }
 
  Tbundle signal;
  sff::FREE signalfilefree, signaltracefree;
  bool hasSignalFileFree;
  {
    if (opt.verbose) 
    { 
      cout << "sigfit: open signal file " << signalname << " of format " 
        << signalformat << endl;
    }
    std::ifstream ifs(signalname.c_str(), 
      datrw::ianystream::openmode(signalformat));
    datrw::ianystream is(ifs, signalformat);
    if (is.hasfree()) 
    { 
      is >> signalfilefree;
      hasSignalFileFree = true;
    }
    int itrace=1;
    while (itrace!=signaltrace) { is.skipseries(); ++itrace; }
    TFXX_assert(is.good(), "Illegal trace for signal input");
    if (opt.verbose) { cout << "read trace" << endl; }
    is >> signal;
    is >> signal.header;
    if (is.hasfree()) { is >> signaltracefree; }
  }
  ++infile;
 
  // read trial files
  Tbundlevec bundlevec;
  while (infile != infiles.end())
  {
    std::string itype(opt.itype);
    if (infile->haskey(formatkey)) { itype=infile->value(formatkey); }
    if (opt.verbose) 
    { 
      cout << "sigfit: Open trial signal file " << infile->name 
        << " of format " << itype << endl;
    }

    std::ifstream ifs(infile->name.c_str(),
                        datrw::ianystream::openmode(itype));
    datrw::ianystream is(ifs, itype);

    bool selectedTraces=infile->haskey(tracekey);
    tfxx::RangeList<int> rangeList;
    if (selectedTraces)
    {
      rangeList = tfxx::string::rangelist<int>(infile->value(tracekey));
    }
    int traceNum = 1;
    while (is.good())
    {
      Tbundle bundle;
      bool readthistrace=true;
      if (selectedTraces) { readthistrace=rangeList.contains(traceNum); }
      if (readthistrace)
      {
        if (opt.verbose)
        {
          cout << "sigfit: Reading trace #" << traceNum << " ..." << endl;
        }
        is >> bundle;
        is >> bundle.header;
        bundlevec.push_back(bundle);
        if (opt.verbose) 
        { 
          cout << "  " << bundle.header.line().substr(0,69) << endl;
        }
      } 
      else
      {
        is.skipseries();
      }
      ++traceNum;
    }
    ++infile;
  }

  unsigned int ntracesread=bundlevec.size();

  // add synthetic trial files
  if (opt.fitoffset)
  {
    if (opt.verbose) { cout << "add offset to trial signals" << endl; }
    Tbundle synsignal;
    synsignal.header=signal.header;
    synsignal=Tseries(signal.shape());
    synsignal=opt.ampoffset;
    synsignal.header.station="NSP";
    synsignal.header.channel="off";
    synsignal.header.auxid="NSP";
    synsignal.header.instype="NSP";
    bundlevec.push_back(synsignal);
  }

  if (opt.fittrend)
  {
    if (opt.verbose) { cout << "add trend to trial signals" << endl; }
    Tbundle synsignal;
    synsignal.header=signal.header;
    synsignal=Tseries(signal.shape());
    for (int i=synsignal.first(); i<=synsignal.last(); ++i)
    {
      synsignal(i)=2.*opt.amptrend*
        (i-(0.5*(synsignal.first()+synsignal.last())))/
        synsignal.size();
    }
    synsignal.header.station="NSP";
    synsignal.header.channel="ramp";
    synsignal.header.auxid="NSP";
    synsignal.header.instype="NSP";
    bundlevec.push_back(synsignal);
  }

  if (opt.fitexp)
  {
    if (opt.verbose) 
    {
      cout << "add exponential decay to trial signals" << endl; 
    }
    Tbundle synsignal;
    synsignal.header=signal.header;
    synsignal=Tseries(signal.shape());
    for (int i=synsignal.first(); i<=synsignal.last(); ++i)
    {
      double argval=-1.*double(i-synsignal.first())/
        (double(synsignal.size())*opt.tcexp);
      synsignal(i)=std::exp(argval);
    }
    synsignal.header.station="NSP";
    synsignal.header.channel="exp";
    synsignal.header.auxid="NSP";
    synsignal.header.instype="NSP";
    bundlevec.push_back(synsignal);
  }

  /*----------------------------------------------------------------------*/

  // truncate length if requested
  if (opt.truncate)
  {
    if (opt.verbose) { cout << "truncate signals if necessary..." << endl; }
    long int n=signal.last();
    for (Tbundlevec::const_iterator i=bundlevec.begin(); 
         i!=bundlevec.end(); i++)
    { n=n<i->last() ? n : i->last(); }
    if (signal.last()>n)
    {
      signal.setlastindex(n);
      signal.header.nsamples=signal.size();
      if (opt.verbose) { cout << "truncate signal to "
        << signal.header.nsamples << " samples" << endl; }
    }
    for (Tbundlevec::iterator i=bundlevec.begin(); 
         i!=bundlevec.end(); i++)
    { 
      if (i->last()>n)
      {
        i->setlastindex(n);
        i->header.nsamples=signal.size();
        if (opt.verbose) { cout << "truncate trial signal" << endl
          << i->header.line() << endl; }
      }
    }
  }

  /*----------------------------------------------------------------------*/

  // check header consistency
  sff::WID2compare compare(sff::Fnsamples | sff::Fdt | sff::Fdate);
  compare.setdatetolerance(opt.Tdate);
  if (opt.verbose) { cout << "checking consistency..." << endl; }
  for (Tbundlevec::const_iterator i=bundlevec.begin(); i!=bundlevec.end(); i++)
  {
    if (!compare (i->header,signal.header))
    {
      cerr << "ERROR: header signature mismatch:" << endl;
      cerr << "signal:" << endl;
      cerr << signal.header.line();
      cerr << "trial signal:" << endl;
      cerr << i->header.line();
      TFXX_abort("baling out...");
    }
  }

  /*----------------------------------------------------------------------*/

  // skip samples if requested
  if (opt.doskip) 
  {
    if (opt.verbose)
    {
      cout << "skip " << opt.skip << " seconds" << endl;
    }
    int nskip=int(floor(opt.skip/signal.header.dt));
    if (nskip>0)
    {
      libtime::TRelativeTime add=nskip*libtime::double2time(signal.header.dt);
      signal.setfirstindex(signal.first()+nskip);
      signal.header.date+=add;
      for (Tbundlevec::iterator i=bundlevec.begin(); 
           i!=bundlevec.end(); i++)
      {
        i->setfirstindex(i->first() + nskip);
        i->header.date += add;
      }
      if (opt.verbose)
      {
        cout << "skipped " << nskip << " samples ("
          << add.timestring() << ")" << endl;
      }
    }
    else
    {
      if (opt.verbose)
      {
        cout << "NOTICE: nothing to skip..." << endl;
        cout << "  seems like " 
          << opt.skip << " seconds to skip < " 
          << signal.header.dt << " seconds sampling interval" << endl;
      }
    }
  }

  /*----------------------------------------------------------------------*/
  double signalrms=ts::rms(signal);
  
  // set up system of linear equations
  if (opt.verbose) { cout << "set up system of linear equations..." << endl; }
  int N=bundlevec.size();
  if (opt.verbose) { cout << "system is of size " << N << "x" << N << endl; }
  Tmatrix Matrix(N,N), rhs(N), coeff(N), normproduct(N), trialrms(N);
  double fulltrialrms=0.;
  for (int i=1; i<=N; ++i)
  {
    for (int k=i; k<=N; ++k)
    {
      Matrix(i,k)=ts::innerproduct(bundlevec[i-1], bundlevec[k-1]);
      Matrix(k,i)=Matrix(i,k);
    }
    rhs(i)=ts::innerproduct(bundlevec[i-1], signal);
    trialrms(i)=ts::rms(bundlevec[i-1]);
    fulltrialrms+=(trialrms(i)*trialrms(i));
    normproduct(i)=rhs(i)/(signalrms*trialrms(i)*signal.size());
  }
  fulltrialrms=sqrt(fulltrialrms/double(N));

  // add stabilization
  if (opt.usesearchrange)
  {
    if (opt.verbose) 
    {
      cout << "stabilize fit:" << endl; 
      cout << "  relative search range: " << opt.searchrange << endl;
    }
    double searchfac=signal.size()*signalrms;
    for (int i=1; i<=N; ++i)
    {
      double range;
      if (opt.equalsearch)
      { range=opt.searchrange*signalrms/fulltrialrms; }
      else
      { range=opt.searchrange*signalrms/trialrms(i); }
      if (opt.verbose)
      {
        cout << "  search range for trial series " << i 
          << " is " << range << endl;
      }
      Matrix(i,i)+=pow((searchfac/range),2);
    }
  }

  // solve
  coeff=linear::lapack::dposv(Matrix,rhs);

  // set up synthetics
  Tbundle synthetics;
  synthetics=Tseries(signal.shape());
  synthetics=0.;
  for (int i=1; i<=N; ++i)
  { synthetics += coeff(i) * bundlevec[i-1]; }
  synthetics.header=signal.header;
  synthetics.header.channel="synt";

  // set up correction
  Tbundle correction;
  correction=Tseries(signal.shape());
  correction=0.;
  if (ntracesread<bundlevec.size())
  {
    for (int i=ntracesread; i<N; ++i)
    { correction += coeff(i+1) * bundlevec[i]; }
  }
  correction.header=signal.header;
  correction.header.channel="corr";
  Tbundle corrsignal;
  corrsignal.header=signal.header;
  corrsignal.header.channel="csig";
  corrsignal=signal-correction;
  Tbundle corrsynthetics;
  corrsynthetics.header=signal.header;
  corrsynthetics.header.channel="csyn";
  corrsynthetics=synthetics-correction;

  // set up residual
  Tbundle residual;
  residual=Tseries(signal.shape());
  residual=signal-synthetics;
  residual.header=signal.header;
  residual.header.channel="diff";

  // write waveforms
  if (opt.writeresidual)
  {
    if (opt.verbose) 
    {
      cout << "write residual waveform to " << opt.residualname 
        << " in format " << opt.otype << endl;
    }
    std::ofstream ofs(opt.residualname.c_str(),
                        datrw::oanystream::openmode(opt.otype));
    datrw::oanystream os(ofs, opt.otype);
    if (os.handlesfilefree())
    {
      sff::FREE residualFileFree;
      residualFileFree.append(SIGFIT_VERSION); 
      if (hasSignalFileFree)
      {
        residualFileFree.append("Signal file free block:");
        residualFileFree.append(signalfilefree);
      }
      os << residualFileFree;
    }
    os << signal;
    os << synthetics;
    os << residual;
    for (Tbundlevec::const_iterator i=bundlevec.begin(); 
         i!=bundlevec.end(); i++)
    { os << i->header; os << *i; }
    for (int i=1; i<=N; ++i)
    {
      Tbundle psyn=bundlevec[i-1];
      psyn=bundlevec[i-1];
      psyn *= coeff(i);
      char chan[6];
      std::sprintf(chan,"s%1.1d", i);
      psyn.header.channel=std::string(chan);
      os << psyn.header; 
      os << psyn;
    }
    os << corrsignal;
    os << corrsynthetics;
    os << correction;
  }
  
  // calculate rms values
  double residualrms=ts::rms(residual);
  double explained=1.-(residualrms/signalrms);

  // calculate direction cosines
  Tmatrix normcoeff(N);
  double length=0.;
  for (int i=1; i<=N; ++i)
  { length += coeff(i)*coeff(i); }
  length=sqrt(length);
  for (int i=1; i<=N; ++i)
  { normcoeff(i) = coeff(i)/length; }
  
  std::string timestring(signal.header.date.timestring());
  cout << "         time: " << timestring.substr(4,21) << endl;
  cout << "      channel: " << signal.header.channel << endl;
  cout << "      station: " << signal.header.station << endl;
  cout << "   instrument: " << signal.header.instype << endl;
  cout << "    signalrms: " << signalrms << endl;
  cout << "  residualrms: " << residualrms << endl;
  cout << "explained rms: " << explained << endl;
  cout << " coefficients: ";
  for (int i=1; i<=N; ++i)
  { cout << coeff(i) << "  "; }
  cout << endl;
  cout << " rms coeffic.: " << length << endl;
  cout << " normalized coefficients: ";
  for (int i=1; i<=N; ++i)
  { cout << normcoeff(i) << "  "; }
  cout << endl;
  cout << " coefficients x rms: ";
  for (int i=1; i<=N; ++i)
  { 
    cout << coeff(i)*trialrms(i) << "  "; 
  }
  cout << endl;
  cout << " normalized scalar product: ";
  for (int i=1; i<=N; ++i)
  { 
    cout << normproduct(i) << "  "; 
  }
  cout << endl;

  // reportline
  cout << signal.header.station << " "
    << signal.header.channel << " "
    << signal.header.instype << " "
    << timestring << " ";
  cout << formatfloat(explained);
  cout << formatfloat(signalrms);
  cout << formatfloat(residualrms);
  for (int i=1; i<=N; ++i)
  { 
    cout << formatfloat(coeff(i));
  }
  cout << formatfloat(length);
  for (int i=1; i<=N; ++i)
  { 
    cout << formatfloat(normcoeff(i));
  }
  for (int i=1; i<=N; ++i)
  { 
    cout << formatfloat(coeff(i)*trialrms(i)); 
  }
  for (int i=1; i<=N; ++i)
  { 
    cout << formatfloat(normproduct(i)); 
  }
  cout << endl;
}

/* ----- END OF sigfit.cc ----- */
