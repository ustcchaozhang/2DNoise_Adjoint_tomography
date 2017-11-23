/*! \file matrixtest.cc
 * \brief test matrix functions and operators
 * 
 * ----------------------------------------------------------------------------
 * 
 * \author Thomas Forbriger
 * \date 19/12/2007
 * 
 * test matrix functions and operators
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
 * 
 * ============================================================================
 */
#define MATRIXTEST_VERSION \
  "MATRIXTEST   V1.0   test matrix functions and operators"

#include <iostream>
#include <tfxx/commandline.h>
#include <linearxx/operators.h>
#include <aff/shaper.h>

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
  const int width=10;
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

#define DUMP( M ) cout << "matrix " << #M << ":" << endl; dump(M);

/*======================================================================*/

int main(int iargc, char* argv[])
{

  // define usage information
  char usage_text[]=
  {
    MATRIXTEST_VERSION "\n"
    "usage: matrixtest" "\n"
    "   or: matrixtest --help|-h" "\n"
  };

  // define full help text
  char help_text[]=
  {
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
  Tmatrix A(aff::Shaper(1,4)(1,3));
  Tmatrix B(aff::Shaper(1,3)(1,5));
  A=2;
  A(1,3)=-5;
  B=3;
  B(1,5)=-11;
  DUMP(A);
  DUMP(B);
  DUMP(linear::op::transposeNxM(A));
  DUMP(linear::op::transposeNxM(B));
  DUMP(linear::op::dotNxM(A,B));
}

/* ----- END OF matrixtest.cc ----- */
