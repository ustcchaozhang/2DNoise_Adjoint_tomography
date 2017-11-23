/*! \file cxxfftwtest.cc
 * \brief a small test program for fftw when called from C++
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 12/09/2007
 * 
 * a small test program for fftw when called from C++
 * 
 * Copyright (c) 2007 by Thomas Forbriger (BFO Schiltach) 
 *
 * ----
 * libfourier is free software; you can redistribute it and/or modify
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
 *  - 12/09/2007   V1.0   Thomas Forbriger
 *  - 07/10/2010   V1.1   just activated this code
 *
 * ============================================================================
 */
#define CXXFFTWTEST_VERSION \
  "CXXFFTWTEST   V1.1   a small test program for fftw when called from C++"

#include <iostream>
#include <tfxx/commandline.h>
#include <fourier/fftwaff.h>
#include <fourier/error.h>

using std::cout;
using std::cerr;
using std::endl;

using namespace fourier::fft;
typedef DRFFTWAFF::Tseries Tseries;
typedef DRFFTWAFF::Tspectrum Tspectrum;

void fill(Tseries& a, int n, int m)
{
  a=Tseries(n);
  int k;
  double f1, f2;
  f1=3.141592653589/n;
  f2=f1*m*2;
  /* fill input array */
  for (k=0; k<n; ++k)
  {
    a(a.f()+k)=sin(k*f1)*sin(k*f1)*sin(k*f2);
  }
  cout << "array filled" << endl;
}

/*----------------------------------------------------------------------*/

void process(int n, int m, const bool& verbose, const bool& debug)
{
  cout << "test with " << n << " samples" << endl;
  cout << "test with frequency " << m << endl;
  Tseries a(n);
  fill(a, n, m);
  if (verbose) { cout << "array is filled " << endl; }
  DRFFTWAFF fftprocessor(n);
  if (verbose) { cout << "processor is created " << endl; }
  Tspectrum b=fftprocessor(a, debug);
  if (verbose) { cout << "forward transform is finished" << endl; }
  Tseries c=fftprocessor(b, debug);
  if (verbose) { cout << "backward transform is finished" << endl; }
  cout << "time series:" << endl;
  for (int i=0; i<n ; ++i)
  {
    cout.width(4);
    cout << i << " ";
    cout.width(15);
    cout.precision(6);
    cout << a(i) << " ";
    cout.width(15);
    cout.precision(6);
    cout << c(i) << endl;
  }
  cout << "power spectrum:" << endl;
  for (int i=0; i<b.size() ; ++i)
  {
    cout.width(4);
    cout << i << " " << abs(b(i)*conj(b(i))) << endl;
  }
}

/*----------------------------------------------------------------------*/
struct Options {
  bool verbose, debug;
  int n, m;
}; // Options

/*----------------------------------------------------------------------*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    CXXFFTWTEST_VERSION "\n"
    "usage: cxxfftwtest [-n n] [-m f]" "\n"
    "   or: cxxfftwtest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-n n   set number of samples to n\n"
    "-m f   set frequency to f\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: debug mode
    {"DEBUG",arg_no,"-"},
    // 3: number of samples
    {"n",arg_yes,"50"},
    // 4: frequency
    {"m",arg_yes,"3"},
    {NULL}
  };

  // no arguments? print usage...
  if (iargc<1) 
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
  opt.verbose=cmdline.optset(1);
  opt.debug=cmdline.optset(2);
  opt.n=cmdline.int_arg(3);
  opt.m=cmdline.int_arg(4);

  FOURIER_assert(opt.n>0, "illegal number of samples")
  FOURIER_assert(opt.m>0, "illegal frequency")

  process(opt.n, opt.m, opt.verbose, opt.debug);
}

/* ----- END OF cxxfftwtest.cc ----- */
