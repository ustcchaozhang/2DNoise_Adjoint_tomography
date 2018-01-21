/*! \file dccal.cc
 * \brief DC calibration
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/06/2007
 * 
 * DC calibration
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 12/06/2007   V1.0   Thomas Forbriger (thof)
 *  - 18/06/2007   V1.1   first operating version
 *                 V1.2   report residual rms
 *  - 24/09/2007   V1.3   gain residual may have a sign
 *  - 21/01/2011   V1.4   estimate gain precision
 *  - 01/02/2014 thof:    use new libtsioxx
 *  - 14/04/2016 thof:    use proper libtsioxx output operators
 * 
 * ============================================================================
 */
#define DCCAL_VERSION \
  "DCCAL  V2016-04-14   DC calibration"

#include <string>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <list>
#include <iterator>
#include <tfxx/commandline.h>
#include <tfxx/misc.h>
#include <sffxx.h>
#include <libtime++.h>
#include <datrwxx/readany.h>
#include <datrwxx/writeany.h>
#include <tsioxx/inputoperators.h>
#include <tsioxx/outputoperators.h>
#include <aff/subarray.h>
#include <aff/functions/sum.h>
#include <aff/functions/sqrsum.h>

#define DEBUG false

using std::cout;
using std::cerr;
using std::endl;

// values type to be used for samples
typedef double Tvalue;

// time series
typedef aff::Series<Tvalue> Tseries;

// sff time series
typedef ts::sff::SFFTimeSeries<Tseries> Ttimeseries;

/*======================================================================*/
// here we go with classes to evaluate the calibration

// a gain and offset pair
struct GainOffset {
  Tvalue gain, offset;
}; // struct GainOffset

// rms of signals
struct RMS {
  Tvalue signalrms, residualrms, inputrms;
}; // struct RMS

// parameters of data acquisition system
class AcqPar {
  public:
    AcqPar():
      dataunits("NSP"), Mdesiredgain(1.), Mdesiredgainset(false) { }
    std::string dataunits;
    double desiredgain() const
    {
      TFXX_assert(this->desiredgainset(),
                  "AcqPar: desired gain is not set!");
      return(Mdesiredgain);
    }
    bool desiredgainset() const { return Mdesiredgainset; }
    void desiredgain(const double &gain)
    { Mdesiredgain=gain, Mdesiredgainset=true; }
  private:
    double Mdesiredgain;
    bool Mdesiredgainset;
}; // struct AcqPar

// a struct to store calibration results
struct Results {
  AcqPar acqpar;
  std::string inputunits;
  Ttimeseries residual;
  bool hasdiff, hascm;
  //! differential mode results (gain and offset)
  GainOffset diffresult;
  //! common mode results (gain and offset)
  GainOffset cmresult;
  //! differential model rms values
  RMS diffrms;
  //! common model rms values
  RMS cmrms;
  //! differential model gain precision
  Tvalue diffgainprecision;
  //! common mode gain precision
  Tvalue cmgainprecision;
}; // struct Results

// a class to handle a single time window
class Window {
  public:
    Window(const std::string& start,
           const std::string& end,
           const Tvalue& inputlevel):
      Mstart(start), Mend(end), Minputlevel(inputlevel)
      { TFXX_assert(Mstart <= Mend, "Window: window ends before it starts"); }
    // set a local handle to the input series and the residual
    void settimeseries(const Ttimeseries::Tconsttimeseries& ts,
                       const Tseries& rts);
    // return number of samples in this window
    long int size() const { return Mts.size(); }
    // return sum of samples in this window
    Tvalue sum() const { return aff::func::sum(Mts); }
    // return sum of squared samples in this window
    Tvalue sqrsum() const { return aff::func::sqrsum(Mts); }
    // return sum of squared residual samples in this window
    Tvalue sqrressum() const { return aff::func::sqrsum(Mrts); }
    // return input level
    Tvalue inputlevel() const { return Minputlevel; }
    // set residual
    void setresidual(const Tvalue& offset, const Tvalue& gain) const;
  private:
    libtime::TAbsoluteTime Mstart;
    libtime::TAbsoluteTime Mend;
    Tvalue Minputlevel;
    Tseries::Tcoc Mts;
    Tseries Mrts;
}; // class Window

// a class to handle time windows
class Windows {
  public:
    typedef std::list<Window> Twindowlist;
    void addwindow(const std::string& start,
                   const std::string& end,
                   const Tvalue& inputlevel)
    { windowlist.push_back(Window(start, end, inputlevel)); }
    void settimeseries(const Ttimeseries::Tconsttimeseries& ts,
                       const Tseries& rts);
    long int size() const;
    Tvalue sum() const;
    Tvalue sqrsum() const;
    Tvalue sqrressum() const;
    Tvalue sumtimesinput() const;
    Tvalue inputsum() const;
    Tvalue inputsqrsum() const;
    void setresidual(const GainOffset& gainoffset) const;
  private:
    Twindowlist windowlist;
}; // class Windows

// function to solve the least-squares problem of linear regression
GainOffset calibrate(const Windows& windows);

// a class to handle the calibration process
class Calibrator {
  public:
    // constructor takes calibration definition from input stream
    Calibrator(std::istream &is);
    // calibration function
    Results operator()(const Ttimeseries::Tconsttimeseries& ts,
                       const AcqPar& acqpar=AcqPar()) const;
    // access functions
    std::string units() const { return Munits; }
  private:
    std::string Munits;
    mutable Windows Mdiffmode;
    mutable Windows Mcmmode;
}; // class Calibrator

/*----------------------------------------------------------------------*/

void Window::settimeseries(const Ttimeseries::Tconsttimeseries& ts,
                           const Tseries& rts)
{
  Tseries::Tcoc cts(ts);
  TFXX_assert(cts.size()==rts.size(),
              "Window: size of residual series does not match");
  TFXX_assert(cts.first()==rts.first(),
              "Window: first index of residual series does not match");
  libtime::TAbsoluteTime tfirst=ts.header.wid2().date;
  libtime::TAbsoluteTime tlast=sff::wid2lastsample(ts.header.wid2());
  TFXX_assert(Mstart <= Mend,
              "Window: first sample later than last one");
  TFXX_assert(tfirst <= Mstart,
              "Window: first sample outside time series");
  TFXX_assert(tlast >= Mend,
              "Window: last sample outside time series");
  long int ifirst=sff::wid2isample(ts.header.wid2(), Mstart);
  long int ilast=sff::wid2isample(ts.header.wid2(), Mend);
  TFXX_assert(ifirst <= ilast,
              "Window: last index smaller than first index");
  TFXX_assert(cts.first() <= ifirst,
              "Window: first index outside time series");
  TFXX_assert(cts.last() >= ilast,
              "Window: last index outside time series");
  Mts=aff::subarray(cts)(ifirst,ilast);
  Mrts=aff::subarray(rts)(ifirst,ilast);
} // void Window::settimeseries(const Ttimeseries::Tconsttimeseries& ts,
  //                            const Tseries& rts)

/*----------------------------------------------------------------------*/

void Window::setresidual(const Tvalue& offset, const Tvalue& gain) const
{
  for (int i=Mts.first(); i<=Mts.last(); ++i)
  {
    Mrts(i)=Mts(i)-offset-gain*this->inputlevel();
//    cout << "DEBUG: " << "set sample " << i << "to " << Mrts(i) << endl;
  }
} // void Window::setresidual(const Tvalue& offset, const Tvalue& gain) const

/*----------------------------------------------------------------------*/

long int Windows::size() const
{
  long int retval(0);
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    retval += I->size();
    ++I;
  }
  return retval;
} // long int Windows::size() const

/*----------------------------------------------------------------------*/

Tvalue Windows::sumtimesinput() const
{
  Tvalue retval(0);
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    retval += (I->sum()*I->inputlevel());
    ++I;
  }
  return retval;
} // Tvalue Windows::sumtimesinput() const

/*----------------------------------------------------------------------*/

Tvalue Windows::inputsum() const
{
  Tvalue retval(0);
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    retval += (I->size()*I->inputlevel());
    ++I;
  }
  return retval;
} // Tvalue Windows::inputsum() const

/*----------------------------------------------------------------------*/

Tvalue Windows::inputsqrsum() const
{
  Tvalue retval(0);
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    retval += (I->size()*I->inputlevel()*I->inputlevel());
    ++I;
  }
  return retval;
} // Tvalue Windows::inputsqrsum() const

/*----------------------------------------------------------------------*/

Tvalue Windows::sqrsum() const
{
  Tvalue retval(0);
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    retval += I->sqrsum();
    ++I;
  }
  return retval;
} // Tvalue Windows::sqrsum() const

/*----------------------------------------------------------------------*/

Tvalue Windows::sqrressum() const
{
  Tvalue retval(0);
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    retval += I->sqrressum();
    ++I;
  }
  return retval;
} // Tvalue Windows::sqrressum() const

/*----------------------------------------------------------------------*/

Tvalue Windows::sum() const
{
  Tvalue retval(0);
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    retval += I->sum();
    ++I;
  }
  return retval;
} // Tvalue Windows::sum() const

/*----------------------------------------------------------------------*/

void Windows::setresidual(const GainOffset& gainoffset) const
{
  Twindowlist::const_iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    I->setresidual(gainoffset.offset, gainoffset.gain);
    ++I;
  }
} // void Windows::setresidual(const Tvalue& offset, const Tvalue& gain) const

/*----------------------------------------------------------------------*/

void Windows::settimeseries(const Ttimeseries::Tconsttimeseries& ts,
                            const Tseries& rts)
{
  TFXX_debug(DEBUG, "Windows::settimeseries",
             "entered function");
  TFXX_debug(DEBUG, "Windows::settimeseries",
             "number of windows: " << windowlist.size());
  Twindowlist::iterator I(windowlist.begin());
  while(I != windowlist.end())
  {
    TFXX_debug(DEBUG, "Windows::settimeseries",
               "set time series to next window");
    I->settimeseries(ts, rts);
    ++I;
  }
} // void Windows::settimeseries(const Ttimeseries::Tconsttimeseries& ts,
  //                             const Tseries& rts)

/*----------------------------------------------------------------------*/

Calibrator::Calibrator(std::istream &is)
{
  while(is.good())
  {
    std::string ID;
    is >> ID;
//    cout << "DEBUG: " << ID << endl;
    if (ID == "U")
    {
      is >> Munits;
    }
    else if (ID == "DM")
    {
      std::string start, end;
      Tvalue level;
      is >> level >> start >> end;
      Mdiffmode.addwindow(start, end, level);
    }
    else if (ID == "CM")
    {
      std::string start, end;
      Tvalue level;
      is >> level >> start >> end;
      Mcmmode.addwindow(start, end, level);
    }
    else if (is.good())
    {
      TFXX_abort("Calibrator: illegal line ID");
    }
  }
} // Calibrator::Calibrator(std::istream &is)

/*----------------------------------------------------------------------*/

Results Calibrator::operator()(const Ttimeseries::Tconsttimeseries& ts,
                               const AcqPar& acqpar) const
{
  Results retval;
  retval.acqpar=acqpar;
  retval.inputunits=Munits;
  retval.hasdiff=false;
  retval.hascm=false;
  Tseries res(ts.first(),ts.last());
  res=0.;
  retval.residual=res;
  retval.residual.header=ts.header;
  // differential mode calibration
  if (Mdiffmode.size() > 0)
  {
    retval.hasdiff=true;
    Mdiffmode.settimeseries(ts, retval.residual);
    retval.diffresult=calibrate(Mdiffmode);
    Mdiffmode.setresidual(retval.diffresult);
    retval.diffrms.signalrms=std::sqrt(Mdiffmode.sqrsum()/Mdiffmode.size());
    retval.diffrms.residualrms
      =std::sqrt(Mdiffmode.sqrressum()/Mdiffmode.size());
    retval.diffrms.inputrms
      =std::sqrt(Mdiffmode.inputsqrsum()/Mdiffmode.size());
    retval.diffgainprecision
      =retval.diffrms.residualrms/retval.diffrms.inputrms;
  }
  // differential mode calibration
  if (Mcmmode.size() > 0)
  {
    retval.hascm=true;
    Mcmmode.settimeseries(ts, retval.residual);
    retval.cmresult=calibrate(Mcmmode);
    Mcmmode.setresidual(retval.cmresult);
    retval.cmrms.signalrms=std::sqrt(Mcmmode.sqrsum()/Mcmmode.size());
    retval.cmrms.residualrms
      =std::sqrt(Mcmmode.sqrressum()/Mcmmode.size());
    retval.cmrms.inputrms
      =std::sqrt(Mcmmode.inputsqrsum()/Mcmmode.size());
    retval.cmgainprecision
      =retval.cmrms.residualrms/retval.cmrms.inputrms;
  }
  return(retval);
} // Results Calibrator::operator()(const Ttimeseries::Tcoc& ts)

/*----------------------------------------------------------------------*/

GainOffset calibrate(const Windows& windows)
{
  GainOffset retval;
  // solve system of linear equations for least squares fit
  Tvalue A11=Tvalue(windows.size());
  Tvalue A12=windows.inputsum();
  Tvalue A21=A12;
  Tvalue A22=windows.inputsqrsum();
  Tvalue B1=windows.sum();
  Tvalue B2=windows.sumtimesinput();
  Tvalue P1=A11*A22;
  Tvalue P2=A12*A21;
  Tvalue det=P1-P2;
  Tvalue AP1=P1 > 0 ? P1 : -P1;
  Tvalue Adet=det > 0 ? det : -det;
  TFXX_assert(AP1 < (1.e10*Adet),
              "problem is ill conditioned");
  retval.offset=(A22*B1-A12*B2)/det;
  retval.gain=(A11*B2-A21*B1)/det;
  return retval;
}

/*----------------------------------------------------------------------*/

// my output format
struct MyFormat {
  int width;
  int precision;
  bool scientific;
  bool showpos;
  bool showpoint;
  MyFormat():
    width(15), precision(4), scientific(true), showpos(true), showpoint(true)
    { }
  MyFormat operator()(const int& w, const int& p, const bool& s=true) const
  { 
    MyFormat retval=*this;
    retval.width=w;
    retval.precision=p;
    retval.scientific=s;
    return retval;
  }
}; // struct MyFormat

// use MyFormat as output manipulator
std::ostream& operator<<(std::ostream& os, const MyFormat& format)
{
  os << std::setw(format.width);
  os << std::setprecision(format.precision);
  if (format.scientific) { os << std::scientific; } else { os << std::fixed; }
  if (format.showpos) { os << std::showpos; } else { os << std::noshowpos; }
  if (format.showpoint) { os << std::showpoint; } 
  else { os << std::noshowpoint; }
  return os;
} // std::ostream& operator<<(std::ostream& os, const MyFormat& format)

/*----------------------------------------------------------------------*/

std::ostream& operator<<(std::ostream& os, const Results& results)
{
  os << "Calibration for:" << endl;
  os << "  " << "station: " << results.residual.header.wid2().station << endl;
  os << "  " << "channel: " << results.residual.header.wid2().channel << endl;
  os << "  " << "auxid:   " << results.residual.header.wid2().auxid << endl;
  os << "  " << "input units (of calibration source):  " 
    << results.inputunits << endl;
  os << "  " << "data units (of data samples in file): " 
    << results.acqpar.dataunits << endl;
  std::string unitratio=results.acqpar.dataunits+"/"+results.inputunits;
  MyFormat format;
  MyFormat cmrformat=format;
  cmrformat.showpos=false;
  os << endl;
  if (results.hasdiff)
  {
    os << "  differential mode gain:" 
      << format(26,5)
      << results.diffresult.gain 
      << " " << unitratio << endl;
    /*
     * in fact
     *
     *   relative gain precision
     *  results.diffgainprecision/std::abs(results.diffresult.gain)
     *
     * and
     *
     *   residual rms with respect to data rms
     *   results.diffrms.residualrms/results.diffrms.signalrms
     *
     * are mathematically identical.
     *
     * In this way they can serve as an internal consistency check here.
     */
    os << "  estimate of relative gain precision:"
      << cmrformat(13,8,false)
      << results.diffgainprecision/std::abs(results.diffresult.gain)
      <<  "\n  (if residual is mean free and normally distributed)"
      << endl;
    os << "  differential mode offset (in data):" 
      << format(14,5)
      << results.diffresult.offset 
      << " " << results.acqpar.dataunits << endl;
    os << "  differential mode offset (at input):" 
      << format(13,5)
      << results.diffresult.offset/results.diffresult.gain
      << " " << results.inputunits << endl;

    os << "  signal rms (data file):"
      << format(26,5)
      << results.diffrms.signalrms 
      << " " << results.acqpar.dataunits << endl;
    os << "  residual rms (data file):"
      << format(24,5)
      << results.diffrms.residualrms 
      << " " << results.acqpar.dataunits << endl;
    os << "  residual rms (data file):"
      << format(24,5)
      << results.diffrms.residualrms/results.diffrms.signalrms
      << " of signal rms" << endl;

    if (results.acqpar.desiredgainset()) {
      os << "  desired gain:" 
        << format(36,5)
        << results.acqpar.desiredgain()
        << " " << unitratio << endl;
      os << "  gain residual:" 
        << format(29,3,false)
        << 100.*((results.diffresult.gain/results.acqpar.desiredgain())-1.)
        << " %" << endl;
    }
  }
  else
  {
    os << "  " << "no differential mode recording is available" << endl;
  }
  os << endl;
  if (results.hascm)
  {
    os << "  common mode gain:" 
      << format(32,5)
      << results.cmresult.gain
      << " " << unitratio << endl;
    os << "  estimate of relative gain precision:"
      << cmrformat(13,8,false)
      << results.cmgainprecision/std::abs(results.cmresult.gain)
      <<  "\n  (if residual is mean free and normally distributed)"
      << endl;
    os << "  common mode offset (in data):" 
      << format(20,5)
      << results.cmresult.offset 
      << " " << results.acqpar.dataunits << endl;

    os << "  signal rms (data file):"
      << format(26,5)
      << results.cmrms.signalrms 
      << " " << results.acqpar.dataunits << endl;
    os << "  residual rms (data file):"
      << format(24,5)
      << results.cmrms.residualrms 
      << " " << results.acqpar.dataunits << endl;
    os << "  residual rms (data file):"
      << format(24,5)
      << results.cmrms.residualrms/results.cmrms.signalrms
      << " of signal rms" << endl;
  }
  else
  {
    os << "  " << "no common mode recording is available" << endl;
  }
  if (results.hascm && results.hasdiff)
  {
    os << endl;
    os << "  common mode rejection:" 
      << cmrformat(27,5)
      << std::abs(results.diffresult.gain/results.cmresult.gain) << endl;
  }
  return(os);
} // std::ostream& operator<<(std::ostream& os, const Results& results)

/*----------------------------------------------------------------------*/

// command line options and arguments
struct Options {
  bool verbose;
  std::string inputformat;
  AcqPar acqpar;
}; // struct Options

/*======================================================================*/

/*!
 * dccal is used to calibrate data recorders with DC voltages at the input to
 * obtain their DC gain values.
 *
 * \todo
 * The calculation and the reports for differential mode gain and common mode
 * gain are identical.
 * Defining a generic gain and result class to be used by both would reduce
 * the amount of code.
 *
 * \todo
 * The precision and accuracy to the test voltage given in the time window
 * file currently is not provided to the program but should be used in the
 * estimate of gain precision.
 */
int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    DCCAL_VERSION "\n"
    "usage: dccal [-v] [-type t] [-units u] [-gain g]" "\n"
    "             infile tabfile resfile logfile" "\n"
    "   or: dccal --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "infile       SFF data file" "\n"
    "tabfile      ASCII table that defines calibration input" "\n"
    "resfile      write residuals to resfile" "\n"
    "logfile      write results to logfile" "\n"
    "-v           be verbose" "\n"
    "-type t      time series file type" "\n"
    "-units u     set data file sample units" "\n"
    "-gain g      set desired differential mode gain" "\n"
    "\n"
    "Definition of table file entries:" "\n"
    "---------------------------------" "\n"
    "\n"
    "Each line has to start with an identifier. Whitespace and" "\n"
    "newline are used to separate strings. No extra words at the" "\n"
    "end of each line are allowed. No comments are allowed." "\n"
    "\n"
    "The program recognizes 3 different identifiers." "\n"
    "  U units" "\n"
    "    A line starting with identifier U will set the units" "\n"
    "    of the input value to the string following the identifier." "\n"
    "  DM val start end" "\n"
    "    val      calibration source value for differential mode calibration\n"
    "    start    start of differential mode time window\n"
    "    end      end of differential mode time window\n"
    "  CM val start end" "\n"
    "    val      calibration source value for common mode calibration\n"
    "    start    start of common mode time window\n"
    "    end      end of common mode time window\n"
    "\n"
    "--- END of program specific definitions ---" "\n"
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
    // 3: data file units
    {"units",arg_yes,"NSP"},
    // 4: desired gain
    {"gain",arg_yes,"0"},
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
    cerr << endl;
    cerr << libtime::usage_time_format_string;
    exit(0);
  }

  // read command line
  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.inputformat=cmdline.string_arg(2);
  opt.acqpar.dataunits=cmdline.string_arg(3);
  if (cmdline.optset(4)) { opt.acqpar.desiredgain(cmdline.double_arg(4)); }

  TFXX_assert(cmdline.extra(), "missing input file");
  std::string infilename=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing table file");
  std::string tabfilename=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing residual file");
  std::string resfilename=cmdline.next();
  TFXX_assert(cmdline.extra(), "missing log file");
  std::string logfilename=cmdline.next();

  /*----------------------------------------------------------------------*/
  // open files
  std::ifstream infs(infilename.c_str());
  datrw::ianystream ins(infs, opt.inputformat);
  std::ifstream tabfs(tabfilename.c_str());
  std::ofstream resfs(resfilename.c_str());
  datrw::oanystream ress(resfs, opt.inputformat);
  std::ofstream logfs(logfilename.c_str());

  /*----------------------------------------------------------------------*/
  if (opt.verbose)
  {
    cout << DCCAL_VERSION << endl;
  }
  logfs << DCCAL_VERSION << endl;

  /*----------------------------------------------------------------------*/
  // initialize calibrator
  Calibrator calibrator(tabfs);

  /*----------------------------------------------------------------------*/

  // cycle through all traces
  while (ins.good()) {

    // create a double precision series container
    Ttimeseries timeseries;
    // read the series
    ins >> timeseries;
    if (opt.verbose)
    {
      cout << "read time series:" << endl;
      cout << " " << timeseries.header.wid2().station 
        << " " << timeseries.header.wid2().channel 
        << " " << timeseries.header.wid2().auxid << endl;
    }
    // calibrate
    Ttimeseries::Tconsttimeseries cts(timeseries, timeseries.header);
    Results results=calibrator(cts, opt.acqpar);
    ress << results.residual;
    if (opt.verbose) { cout << "\n" << results; }
    logfs << "\n" << results;

  } // end of cycling through all traces
}

/* ----- END OF dccal.cc ----- */
