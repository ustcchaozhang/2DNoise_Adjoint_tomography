/*! \file tstest.cc
 * \brief test time series modules
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/03/2016
 * 
 * test time series modules
 * 
 * Copyright (c) 2003 by Thomas Forbriger (BFO Schiltach) 
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
 *  - 20/12/2003   V1.0   Thomas Forbriger
 *  - 27/06/2006   V1.1   did not work anymore; deleted old code and filled
 *                        with convolution test
 *  - 21/11/2006   V1.2   test drop containers
 *  - 28/01/2012   V1.3   test OffsetVariableTaper
 *  - 20/05/2015   V1.4   print library usage texts
 *  - 19/03/2016   V1.5   test subsequent calls to function dugauss
 * 
 * ============================================================================
 */
#define TSTEST_VERSION \
  "TSTEST   V1.5   test time series modules"

#include <tfxx/commandline.h>
#include <iostream>
#include <tsxx/tsxx.h>
#include <aff/dump.h>
#include <tsxx/convolve.h>
#include <tsxx/random.h>
#include <tsxx/dropcontainer.h>
#include <tsxx/tapers.h>
#include <tsxx/ovtaper.h>
#include <tsxx/seifeclass.h>
#include <tsxx/filter.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool convtest1, convtest2;
  bool randomnoise;
  bool droptest, tapertest;
  bool ovtapertest;
  std::string ovtaperfile;
}; // struct Options

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    TSTEST_VERSION "\n"
    "usage: tstest [-c1] [-c2] [-r] [-drop] [-tap] [-ovtap f]" "\n"
    "   or: tstest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "-c1          convolution test 1" "\n"
    "-c2          convolution test 2" "\n"
    "-r           random noise test" "\n"
    "-drop        drop container test" "\n"
    "-tap         test tapers" "\n"
    "-ovtap f     test OffsetVariableTaper by reading taper from file f" "\n"
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
    // 2: convolution test 1
    {"c1",arg_no,"-"},
    // 3: convolution test 2
    {"c2",arg_no,"-"},
    // 4: random noise test
    {"r",arg_no,"-"},
    // 5: drop container test
    {"drop",arg_no,"-"},
    // 6: taper test
    {"tap",arg_no,"-"},
    // 7: OffsetVariableTaper test
    {"ovtap",arg_yes,"-"},
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
    ts::fir::help(cerr);
    cerr << endl;
    ts::filter::print_help(cerr);
    cerr << endl;
    ts::seife::print_help(cerr);
    exit(0);
  }

  Options opt;
  opt.convtest1=cmdline.optset(2);
  opt.convtest2=cmdline.optset(3);
  opt.randomnoise=cmdline.optset(4);
  opt.droptest=cmdline.optset(5);
  opt.tapertest=cmdline.optset(6);
  opt.ovtapertest=cmdline.optset(7);
  opt.ovtaperfile=cmdline.string_arg(7);

  /*======================================================================*/

  if (opt.convtest1)
  {
    aff::Series<double> a(-2,2), b(-3,3), c, d;
    cout << endl;
    cout << "convolution test 1" << endl;
    for (int i=a.f(); i<=a.l(); ++i) { a(i)=i; }
    for (int i=b.f(); i<=b.l(); ++i) { b(i)=i; }
    DUMP( a );
    DUMP( b );
    DUMP( ts::convolve(a,b) );
    c=a;
    c.shift(-c.f());
    DUMP( c );
    DUMP( ts::convolve(c,b) );
    d=b;
    d.shift(-d.f());
    DUMP( d );
    DUMP( ts::convolve(c,d) );
  }

  /*======================================================================*/

  if (opt.convtest2)
  {
    aff::Series<double> a(-2,2), b(-3,3);
    cout << endl;
    cout << "convolution test 2" << endl;
    for (int i=a.f(); i<=a.l(); ++i) { a(i)=1; }
    for (int i=b.f(); i<=b.l(); ++i) { b(i)=1; }
    DUMP( a );
    DUMP( b );
    DUMP( ts::convolve(a,b) );
  }

  /*======================================================================*/

  if (opt.randomnoise)
  {
    cout << "test random noise generation" << endl;
    ts::rnd::Tdseries s=ts::rnd::dugauss(10);
    DUMP( s );
    s=ts::rnd::dugauss(10);
    DUMP( s );
  }

  /*======================================================================*/

  if (opt.droptest)
  {
    cout << "test drop containers" << endl;
    aff::Series<int> a(1,108);
    for (int i=a.f(); i<=a.l(); ++i) { a(i)=i; }
    ts::drop::DropDropContainer<int> d1(3);
    ts::drop::DropDropContainer<int> d2(2);
    ts::drop::DropDropContainer<int> d3(2);
    ts::drop::PassDropContainer<int> pass;
    cout << "downsampling with factor " << d1.downsampling_factor() << endl;
    cout << "downsampling with gain " << d1.gain() << endl;
    d1.attach(d2)->attach(d3)->attach(pass)->attach(d1);
    cout << "downsampling with factor " << d1.downsampling_factor() << endl;
    cout << "downsampling with gain " << d1.gain() << endl;
    d1.initialize(a.size());
    cout << "downsampling with factor " << d1.downsampling_factor() << endl;
    cout << "downsampling with gain " << d1.gain() << endl;
    for (int i=a.f(); i<=a.l(); ++i) { d1.drop(a(i)); }
    DUMP( a );
    DUMP( d1.container() );

    cout << ts::fir::SeisCompMP << endl;
    cout << ts::fir::SeisCompLP << endl;
    cout << ts::fir::SeisCompVLP << endl;
    aff::Series<double> b(1,1000);
    for (int i=b.f(); i<=b.l(); ++i) { b(i)=i; }
    ts::drop::FIRDropContainer<double> d4("MP");
    ts::drop::FIRDropContainer<double> d5("LP");
//    d4.attach(d5);
    d4.initialize(b.size());
    for (int i=b.f(); i<=b.l(); ++i) { d4.drop(b(i)); }
    DUMP( d4.container() );
  }

  /*======================================================================*/

  if (opt.tapertest)
  {
    cout << "test taper" << endl;
    aff::Series<double> a(-5,5);
    a=1.;
    DUMP( a );
    ts::tapers::Hanning h;
    h.apply(a);
    DUMP( a );


    aff::Series<double> b(-50,50);
    b=1;
    h.apply(b);
    for (int i=b.f(); i<=b.l(); ++i)
    {
      cout << i << " " << b(i) << endl;
    }
  }

  /*======================================================================*/

  if (opt.ovtapertest)
  {
    ts::tapers::OffsetVariableTaper ovt(true);
    ovt.read(opt.ovtaperfile);
    const int n=30;
    const double xmax=90.; 
    const double dx=xmax/n;
    ts::tapers::ovtaper::Picks picks=ovt.t3();
    for (int i=0; i<n; ++i)
    {
      double x=dx*i-9.;
      cout << "x=" << x << " t=" << picks.time(x) << "\n";
    }

    ts::tapers::FourPoint taper=ovt.taper(30., 0., 1.);
    aff::Series<double> b(-50,50);
    b=1;
    taper.apply(b);
    for (int i=b.f(); i<=b.l(); ++i)
    {
      cout << i << " " << b(i) << endl;
    }
  }

}

/* ----- END OF tstest.cc ----- */
