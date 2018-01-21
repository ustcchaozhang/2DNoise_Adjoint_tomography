/*! \file sfftest.cc
 * \brief test library modules
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 21/12/2003
 * 
 * test library modules
 *
 * ----
 * libsffxx is free software; you can redistribute it and/or modify
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
 * Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
 * 
 * REVISIONS and CHANGES 
 *  - 21/12/2003   V1.0   Thomas Forbriger
 *  - 13/04/2010   V1.1   start implementing WIDX
 * 
 * ============================================================================
 */
#define SFFTEST_VERSION \
  "SFFTEST   V1.0   test library modules"

#include <fstream>
#include <iostream>
#include <sffxx.h>
#include <aff/series.h>
#include <tfxx/commandline.h>

using std::cout;
using std::cerr;
using std::endl;

/*----------------------------------------------------------------------*/

void test_internals()
{
  cout << "test internals\n"
    <<    "--------------\n" << endl;
  cout << "sizeof(sff::WID2): " << sizeof(sff::WID2) << endl;
} // void test_internals()

/*----------------------------------------------------------------------*/

void test_extended_functions()
{
  cout << "test WIDX functions" << endl
    <<    "-------------------" << endl;

  sff::WID2 mywid2;
  sff::verbose(cout, mywid2);
  mywid2.date += libtime::TRelativeTime(0,0,0,0,432,567);
  mywid2.station="BFO";
  mywid2.channel="UGZ";
  mywid2.instype="ET-19";
  mywid2.nsamples=2048;
  mywid2.dt=60.;
  mywid2.hang=98.3342;
  mywid2.vang=-15.3;
  sff::verbose(cout, mywid2);

  std::string Xline=sff::WIDXline(mywid2);
  cout << Xline << std::endl;

  sff::WID2 readwid2=sff::WIDXline(Xline);
  sff::verbose(cout, readwid2);

} // test_extended_functions()

/*----------------------------------------------------------------------*/

void test_line_functions()
{
  cout << "test line functions" << endl
    <<    "-------------------" << endl;

  sff::STAT mystat;
  cout << mystat.line();
  cout << endl;

  sff::SRCE mysrce;
  cout << mysrce.line();
  cout << endl;

  sff::DAST mydast;
  cout << mydast.line();
  cout << endl;

  sff::INFO myinfo;
  cout << myinfo.line();
  cout << endl;

  sff::WID2 mywid2;
  cout << mywid2.line();
  mywid2.station="BFO";
  mywid2.channel="UGZ";
  mywid2.instype="ET-19";
  mywid2.nsamples=2048;
  mywid2.dt=10.;
  cout << mywid2.line();
  cout << endl;

  sff::FREE myfree;
  myfree.lines.push_back("Hi there!");
  myfree.lines.push_back("A second free line");
  myfree.write(cout);
  cout << endl;

} // test_line_functions()

/*----------------------------------------------------------------------*/

void test_verbose_functions()
{
  cout << "test verbose functions" << endl
    <<    "----------------------" << endl;

  sff::STAT mystat;
  sff::verbose(cout, mystat);

  sff::SRCE mysrce;
  sff::verbose(cout, mysrce);

  sff::DAST mydast;
  sff::verbose(cout, mydast);

  sff::INFO myinfo;
  sff::verbose(cout, myinfo);

  sff::WID2 mywid2;
  sff::verbose(cout, mywid2);
  mywid2.station="BFO";
  mywid2.channel="UGZ";
  mywid2.instype="ET-19";
  mywid2.nsamples=2048;
  mywid2.dt=10.;
  sff::verbose(cout, mywid2);

  sff::FREE myfree;
  myfree.lines.push_back("Hi there!");
  myfree.lines.push_back("A second free line");
  sff::verbose(cout, myfree);

  sff::TraceHeader myth;
  sff::verbose(cout, myth);

  sff::FileHeader myfh;
  sff::verbose(cout, myfh);

} // test_verbose_functions()

/*----------------------------------------------------------------------*/

void test_write_wrappers()
{
  cout << "test write wrappers" << endl
    <<    "-------------------" << endl;

  sff::SRCE mysrce;
  sff::INFO myinfo;
  sff::FREE myfree;
  myfree.lines.push_back("auch ein text");
  sff::WID2 mywid2;
  mywid2.station="BFO";
  mywid2.channel="UGZ";
  mywid2.instype="ET-19";
  mywid2.nsamples=2048;
  mywid2.dt=10.;

  { cout << sff::FileHeader(); }
  cout << endl << "next:" << endl;
  { cout << sff::FileHeader(mysrce); }
  cout << endl << "next:" << endl;
  { cout << sff::FileHeader(myfree); }
  cout << endl << "next:" << endl;
  { cout << sff::FileHeader(mysrce, myfree); }

  cout << endl << "next:" << endl;
  { cout << sff::TraceHeader(mywid2); }
  cout << endl << "next:" << endl;
  { cout << sff::TraceHeader(mywid2, myinfo); }
  cout << endl << "next:" << endl;
  { cout << sff::TraceHeader(mywid2, myfree); }
  cout << endl << "next:" << endl;
  { cout << sff::TraceHeader(mywid2, myinfo, myfree); }
  cout << endl << "next:" << endl;
  { cout << sff::TraceHeader(mywid2, myinfo,true); }
}

/*----------------------------------------------------------------------*/

typedef aff::Series<double> Tseries;
void scale_series(Tseries& s, const double& f)
{
  for (int i=s.first(); i<=s.last(); i++)
  { s(i) *= f; }
  cout << "scale by " << f << endl;
}

void scaling(const sff::TraceHeader& hd)
{
  if (hd.scale())
  { cout << "will be scaled with ampfac "; }
  else
  { cout << "will not be scaled and ampfac is "; }
  cout << hd.dast().ampfac << endl; 
}

void test_waveform_normalizer()
{
  cout << "test waveform normalizer" << endl
    <<    "------------------------" << endl;

  cout << "limit: " << sff::WaveformNormalizer::limit << "   ";
  cout << "2^23: " << std::pow(2.,23.) << endl;

  const int msamp=100;
  Tseries series(msamp);
  for (int i=series.first(); i<=series.last(); i++)
  { series(i)=sin(3.1415926*i*5/msamp); }

  sff::WID2 wid2;
  sff::TraceHeader hd(wid2);
  hd.scanseries(series);
  scaling(hd);

  scale_series(series, 1.e10);
  hd.scanseries(series);
  scaling(hd);

  scale_series(series, 1.e-4);
  hd.scanseries(series, sff::NM_ifneeded);
  scaling(hd);

  scale_series(series, 1.e4);
  hd.scanseries(series, sff::NM_ifneeded);
  scaling(hd);

  scale_series(series, 1.e-4);
  hd.scanseries(series, sff::NM_one);
  scaling(hd);

#ifdef ILLEGAL1
  scale_series(series, 1.e4);
  hd.scanseries(series, sff::NM_one);
  scaling(hd);
#endif
}

/*----------------------------------------------------------------------*/

void test_write_file()
{
  char filename[]="junk.sff";
  cout << "test writing to file: " << filename << endl
    <<    "---------------------" << endl;

  const int msamp=1000;
  Tseries series(msamp);
  for (int i=series.first(); i<=series.last(); i++)
  { series(i)=10.*sin(3.1415926*i*5/msamp); }

  sff::SRCE mysrce;
  mysrce.cx=1.;
  mysrce.cy=2.;
  mysrce.cz=3.;
  mysrce.type="Quelle";

  sff::INFO myinfo;

  sff::FREE myfree;
  myfree.lines.push_back("auch ein text");

  sff::WID2 mywid2;
  mywid2.station="BFO";
  mywid2.channel="UGZ";
  mywid2.instype="ET-19";
  mywid2.nsamples=2048;
  mywid2.dt=10.;

  sff::FileHeader fhd(mysrce,myfree);
  std::ofstream os(filename);
  os << fhd;
  {
    sff::TraceHeader hd(mywid2,myinfo,myfree);
    os << sff::OutputWaveform<Tseries>(series, hd, sff::NM_maxdyn);
  }
  {
    myinfo.cx=10.;
    sff::TraceHeader hd(mywid2,myinfo,myfree,true);
    os << sff::OutputWaveform<Tseries>(series, hd, sff::NM_ifneeded);
  }
  cout << "you will find the data in " << filename << endl;
}

/*----------------------------------------------------------------------*/

void test_read_file(const bool& debug)
{
  char infile[]="junk.sff";
  char outfile[]="junk2.sff";
  cout << "test reading from file: " << infile << endl
    <<    "-----------------------" << endl;
 
  {
    char c;
    std::cerr << "manipulate " << infile 
      << " - if you like to..." << std::endl;
    std::cin.get(c);
  }

  std::ifstream is(infile);
  sff::FileHeader fhd(is);

  std::ofstream os(outfile);
  os << fhd;

  if (fhd.hasfree()) 
  { 
    cout << "FREE: " << std::endl << fhd.free() << endl; 
  }
  if (fhd.hassrce()) 
  { 
    cout << "SRCEtime: " << fhd.srce().date.timestring() << endl; 
  }

  bool last=false;
  while (!last)
  {
    sff::InputWaveform<Tseries> iwf(debug);
    try {
      is >> iwf;
    } catch (GSE2::Terror) {
      cerr << "UUpppsss" << endl;
      sff::WID2 wid2line(iwf.header().wid2());
      cerr << wid2line.line();
      Tseries series(iwf.series());
      cerr << series.size() << " samples" << endl;
      throw;
    }
    cerr << iwf.header().wid2().line() << endl;
    last=iwf.last();
    os << sff::OutputWaveform<Tseries>(iwf.series(), 
                                       iwf.header(), 
                                       sff::NM_maxdyn);
  }
  cout << "you will find the data in " << outfile << endl;
}

/*----------------------------------------------------------------------*/

void test_skip_trace()
{
  char outfile[]="junk3.sff";
  cout << "test kipping every second trace" << endl
    <<    "-------------------------------" << endl;
 
  cout << "enter name of input file: ";
  std::string infile;
  std::cin >> infile;

  std::ifstream is(infile.c_str());
  sff::FileHeader fhd(is);

  std::ofstream os(outfile);
  os << fhd;

  if (fhd.hassrce()) 
  { 
    cout << "SRCEtime: " << fhd.srce().date.timestring() << endl; 
  }

  bool last=false;
  while (!last)
  {
    sff::SkipWaveform swf(is);
    cout << swf.header().wid2().line() << endl;
    last=swf.last();
    if (!last) 
    {
      sff::InputWaveform<Tseries> iwf(is);
      last=iwf.last();
      os << sff::OutputWaveform<Tseries>(iwf.series(), 
                                         iwf.header(), 
                                         sff::NM_maxdyn);
    }
    else
    {
      Tseries dummy(5);
      dummy=0.;
      sff::TraceHeader ihd(swf.header());
      sff::TraceHeader ohd(ihd.wid2(),true);
      os << sff::OutputWaveform<Tseries>(dummy, ohd, sff::NM_one);
    }
  }
  cout << "you will find the data in " << outfile << endl;
}

/*----------------------------------------------------------------------*/

struct Options {
  bool debug, verbose, extended, internals;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    "usage: sfftest [-D] [-v] [-x] [-i]" "\n"
    "   or: sfftest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-D           produce debug output" "\n"
      "notice: you have to pass at least one dummy argument to make" "\n"
      "the program run" "\n"
    "-v           test verbose output" "\n"
    "-x           test extended WIDX format" "\n"
    "-i           test library internals" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: debug mode
    {"D",arg_no,"-"},
    // 2: test verbose functions
    {"v",arg_no,"-"},
    // 3: test WIDX
    {"x",arg_no,"-"},
    // 4: test internals
    {"i",arg_no,"-"},
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

  Options opt;
  opt.debug=cmdline.optset(1);
  opt.verbose=cmdline.optset(2);
  opt.extended=cmdline.optset(3);
  opt.internals=cmdline.optset(4);

  if (opt.verbose)
  {
    test_verbose_functions(); cout << endl;
  }
  else if (opt.extended)
  {
    test_extended_functions(); cout << endl;
  }
  else if (opt.internals)
  {
    test_internals(); cout << endl;
  }
  else
  {
    test_line_functions(); cout << endl;
    test_write_wrappers(); cout << endl;
    test_waveform_normalizer(); cout << endl;
    test_write_file(); cout << endl;
    test_read_file(opt.debug); cout << endl;
    test_skip_trace(); cout << endl;
  }
}

/* ----- END OF sfftest.cc ----- */
