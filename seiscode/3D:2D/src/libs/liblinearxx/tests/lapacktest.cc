/*! \file lapacktest.cc
 * \brief Test interfaced LAPACK functions
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2007
 * 
 * Test interfaced LAPACK functions
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
 *  - 19/12/2007   V1.0   Thomas Forbriger
 *  - 18/01/2008   V1.1   reactivated test with random matrix
 *                        apparently operates well
 * 
 * ============================================================================
 */
#define LAPACKTEST_VERSION \
  "LAPACKTEST   V1.1   Test interfaced LAPACK functions"

#include <iostream>
#include <tfxx/commandline.h>
#include <linearxx/lapackxx.h>
#include <linearxx/operators.h>
#include <aff/iterator.h>
#include <aff/arrayoperators.h>
#include <tfxx/rng.h>

using std::cout;
using std::cerr;
using std::endl;

struct Options {
  bool verbose;
  int nsize;
}; // struct Options

typedef linear::TDmatrix Tmatrix;

void dump(const Tmatrix::Tcoc& m)
{
  const int width=15;
  const int precision=5;
  cout.width(width);
  cout << " ";
  for (int i=m.first(1); i<=m.last(1); ++i)
  {
    cout.width(width);
    cout << i;
  }
  cout << endl;
  for (int j=m.first(0); j<=m.last(0); ++j)
  {
    cout.width(width);
    cout << j;
    for (int i=m.first(1); i<=m.last(1); ++i)
    {
      cout.width(width);
      cout.precision(precision);
      cout << m(j,i);
    }
    cout << endl;
  }
} // void dump(const Tmatrix::Tcoc& m)

#define DUMP( M ) cout << #M << ":" << endl; dump(M);

/*----------------------------------------------------------------------*/

// test a system defined by a matrix and a vector
void test(const Tmatrix::Tcoc& M, const Tmatrix::Tcoc V)
{
  cout << endl;
  cout << "test function" << endl
    <<    "-------------" << endl;
  cout << "input matrix and vector:" << endl;
  DUMP(M);
  DUMP(V);
  Tmatrix A=linear::op::dotNxM(M,linear::op::transposeNxM(M));
  cout << "positive definite system matrix: " << endl;
  DUMP(A);
  Tmatrix B=linear::op::dotNxM(A,V);
  cout << "right hand side vector: " << endl;
  DUMP(B);
  Tmatrix X=linear::lapack::dposv(A,B,'U',false);
  cout << "solution to system of linear equations: " << endl;
  DUMP(X);
  cout << "expected solution: " << endl;
  DUMP(V);
  DUMP(X-V);
}

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    LAPACKTEST_VERSION "\n"
    "usage: lapacktest [-n n]" "\n"
    "   or: lapacktest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
    "\n"
    "-v     be verbose" "\n"
    "-n N   test system of size NxN" "\n"
  };

  // define commandline options
  using namespace tfxx::cmdline;
  static Declare options[]= 
  {
    // 0: print help
    {"help",arg_no,"-"},
    // 1: verbose mode
    {"v",arg_no,"-"},
    // 2: verbose mode
    {"n",arg_yes,"3"},
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

  /*
  // dummy operation: print option settings
  for (int iopt=0; iopt<2; iopt++)
  {
    cout << "option: '" << options[iopt].opt_string << "'" << endl;
    if (cmdline.optset(iopt)) {  cout << "  option was set"; }
    else { cout << "option was not set"; }
    cout << endl;
    cout << "  argument (string): '" << cmdline.string_arg(iopt) << "'" << endl;
    cout << "     argument (int): '" << cmdline.int_arg(iopt) << "'" << endl;
    cout << "    argument (long): '" << cmdline.long_arg(iopt) << "'" << endl;
    cout << "   argument (float): '" << cmdline.float_arg(iopt) << "'" << endl;
    cout << "  argument (double): '" << cmdline.double_arg(iopt) << "'" << endl;
    cout << "    argument (bool): '";
    if (cmdline.bool_arg(iopt))
    { cout << "true"; } else { cout << "false"; }
    cout << "'" << endl;
  }
  while (cmdline.extra()) { cout << cmdline.next() << endl; }

  // dummy operation: print rest of command line
  while (cmdline.extra()) { cout << cmdline.next() << endl; }
  */

  Options opt;
  opt.verbose=cmdline.optset(1);
  opt.nsize=cmdline.int_arg(2);

  /*----------------------------------------------------------------------*/

  {

    if (opt.verbose)
    {
      cout << "Define system of size " << opt.nsize << "x" << opt.nsize << endl;
    }

    Tmatrix M(opt.nsize,opt.nsize);
    Tmatrix R(opt.nsize,1);

    // set up random number generator
    tfxx::numeric::RNGgaussian rng;

    {
      // set up system matrix
      aff::Iterator<Tmatrix> I(M);
      while (I.valid())
      {
        *I = rng();
        ++I;
      }
    }

    {
      // set up system vector
      aff::Iterator<Tmatrix> I(R);
      while (I.valid())
      {
        *I = rng();
        ++I;
      }
    }
    test(M,R);
  }

  /*----------------------------------------------------------------------*/

  {
    Tmatrix M(3,3), V(3,1), W(3,2);
    M(1,1)=1.;
    M(1,2)=0.4;
    M(1,3)=0.6;
    M(2,1)=-0.2;
    M(2,2)=-1.;
    M(2,3)=0.7;
    M(3,1)=-1.;
    M(3,2)=-0.3;
    M(3,3)=1.;
    DUMP(M);
    V(1)=-80.;
    V(2)=70.;
    V(3)=-35.;
    DUMP(V);
    test(M,V);
    W(1,1)=-80.;
    W(2,1)=70.;
    W(3,1)=-35.;
    W(1,2)=0.;
    W(2,2)=2.;
    W(3,2)=-3.;
    DUMP(W);
    test(M,W);
  }
}

/* ----- END OF lapacktest.cc ----- */
